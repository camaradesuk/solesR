#' Get unscreened studies from database'
#'
#' @param con connection to db
#' @param classify_NA boolean true/false based on whether you want to retain missing abstracts - the machine
#' does not perform well with missing abstracts
#' @param project_name name of soles project
#' @param classifier_name name of classifier e.g. in vivo
#' @return citations which have not been screened
#' @export
#'
get_studies_to_screen <- function(con, classify_NA = FALSE, project_name, classifier_name){
  
  ####------ Get studies to screen  ------ ####
  
  screened <- tbl(con, "study_classification")  %>% filter(name == classifier_name) %>% select(uid)
  
  unscreened <- tbl(con, "unique_citations") %>%
    select(uid, title, keywords, abstract) %>%
    anti_join(screened, by="uid") %>%
    collect()
  
  if(classify_NA == FALSE){
    
    # remove studies with no abstract
    unscreened[unscreened == "" ] <- NA
    unscreened[unscreened == "NA" ] <- NA
    studies_to_screen <- unscreened %>%
      filter(!is.na(abstract)) %>%
      unique()
  }
  
  else{
    
    unscreened[unscreened == "" ] <- NA
    studies_to_screen <- unscreened %>%
      unique()
  }
  
  # randomise dataset rows
  set.seed(111)
  studies_to_screen <- studies_to_screen[sample(nrow(studies_to_screen)),]
  
  # format columns
  studies_to_screen <- studies_to_screen %>%
    rename(ABSTRACT = abstract,
           ITEM_ID = uid,
           TITLE = title,
           KEYWORDS = keywords)  %>%
    mutate(Cat = "",
           LABEL = 99,
           REVIEW_ID = project_name) %>%
    select(ITEM_ID, REVIEW_ID, KEYWORDS, Cat, ABSTRACT, TITLE, LABEL) %>%
    unique()
  
}

#' Get Screening Decisions from the Database
#'
#' This function retrieves screening decisions from the "study_classification" table
#' in the specified database and returns a data frame in the correct format to run the machine learning function.
#'
#' @import dplyr
#'
#' @param con A database connection object.
#' @param review_id A unique identifier for the review associated with the screening decisions.
#' @param classifier_name Name of classifier used in study_classification table (e.g. "in-vivo)
#'
#' @return A data frame containing screening decisions with columns: ITEM_ID, LABEL, TITLE, ABSTRACT, KEYWORDS, Cat, REVIEW_ID.
#'
#' @examples
#' \dontrun{
#'   screening_decisions <- get_screening_decisions(con = your_database_connection, review_id = "your_project_plus_date", classifier_name = "in-vivo")
#' }
#' @import dplyr
#' @export
#'
get_screening_decisions <- function(con, review_id = "", classifier_name = NULL){
  
  if (!is.null(classifier_name)){
    
    screening_decisions <- tbl(con, "study_classification") %>%
      filter(type == "human_reviewer") %>%
      filter(name %in% classifier_name) %>%  
      left_join(tbl(con, "unique_citations"), by = "uid") %>%
      select(ITEM_ID = uid, LABEL = decision, TITLE = title, ABSTRACT = abstract, KEYWORDS = keywords) %>%
      mutate(LABEL = ifelse(LABEL == "include", 1, 0),
             Cat = "",
             REVIEW_ID = review_id) %>%
      collect()
    
  } else {
    
    screening_decisions <- tbl(con, "study_classification") %>%
      filter(type == "human_reviewer") %>%
      left_join(tbl(con, "unique_citations"), by = "uid") %>%
      select(ITEM_ID = uid, LABEL = decision, TITLE = title, ABSTRACT = abstract, KEYWORDS = keywords) %>%
      mutate(LABEL = ifelse(LABEL == "include", 1, 0),
             Cat = "",
             REVIEW_ID = review_id) %>%
      collect()
  }
  
  return(screening_decisions)
  
}

#' Run machine learning to get included studies and send them to soles database
#' @import readr
#' @param con connection to db
#' @param project_name name or project identifier
#' @param training_set set of screened citations with 1/0 LABEL
#' @param unscreened_set set of unscreened citations LABEL 99
#' @param project_name name of soles project
#' @param classifier_name name of classifier that describes what it does e.g. "screening" or "in vivo"
#' @return citations which have not been screened
#' @export
#'
run_ml <- function(con, training_set, unscreened_set, project_name, classifier_name){
  
  # Create directories
  dir.create("screening", showWarnings = F)
  dir.create("screening/output", showWarnings = F)
  
  source("/opt/sharedFolder/SSML/create_files_API.R")
  source("/opt/sharedFolder/SSML/JT_API_config.R")
  source("/opt/sharedFolder/SSML/JT_API_wrap.R")
  source("/opt/sharedFolder/SSML/ML_analysis.R")
  
  date <- format(Sys.Date(), "%d%m%y")
  
  # Get CID to use as unique ID
  new_cid_no <- tbl(con, "study_classification") %>%
    select(cid) %>%
    distinct() %>%
    summarize(new_cid = max(cid) + 1) %>%
    collect() %>%
    pull(new_cid)
  
  new_cid <- paste0("CID", new_cid_no)
  
  # Specify data folder
  dataFolder <- "screening/output/"
  
  # Write data and label files out as IOE API protocol
  outputFilenames <- CreateMLFilenames(paste0(dataFolder,project_name, "_"), new_cid)
  
  combined_data <- rbind(training_set, unscreened_set)
  
  # Send the data to API and write the results
  allDecisions <- WriteFilesForIOE(combined_data, outputFilenames)
  ifilenames <- CreateFileNamesForIOEAPI(outputFilenames$Decisions, outputFilenames$Results)
  
  # Main wrapper for API call - write file to container, run API, download results file
  TrainCollection(ifilenames, gsub("[-]","", paste0(project_name, "_", new_cid)))
  
  # Performance calculations
  analysisResult <- FindBestPerformance(outputFilenames$Results,
                                        outputFilenames$AllDecisions,
                                        outputFilenames$Analysis)
  
  # Get threshold
  threshold <- analysisResult[which(as.logical(analysisResult[,"Chosen"])), "Threshold"][[1]]
  
  
  # Bring in all records and convert temporary ID back
  all_records <- read_tsv(outputFilenames$AllDecisions)
  
  results <- read_tsv(ifilenames$ResultsFileName) %>% 
    select(score = probabilities, ID = PaperId) %>% 
    left_join(all_records, by = c("ID" = "TEMP_ID")) %>% 
    select(score, ITEM_ID, REVIEW_ID) 
  
  included <- results %>% 
    filter(
      ITEM_ID %in% unscreened_set$ITEM_ID,
      score > threshold
    )
  
  # Write csv of latest included studies - IS THIS NEEDED??
  write_tsv(included, file = paste0(dataFolder, project_name, "_", new_cid, "_final_results", ".tsv"))
  
  
  # Make df of performance at given threshold
  performance <- as.data.frame(analysisResult)
  performance <- performance %>%
    filter(Chosen==1)
  
  performance <- performance %>%
    mutate(cid = new_cid_no) %>%
    select(-Chosen) %>%
    rename(balanced_accuracy = Balanced.Accuracy)
  
  names(performance) <- tolower(names(performance))
  
  # Write performance log to table
  dbWriteTable(con, "ml_performance", performance, append=TRUE)
  
  # make lower case
  included$ITEM_ID <- tolower(included$ITEM_ID)
  unscreened_set$ITEM_ID <- tolower(unscreened_set$ITEM_ID)
  
  # Wrangle data to be added to study class table
  screen_id <- tbl(con, "study_classification") %>% select(cid) %>% distinct() %>% collect()
  to_write <- unscreened_set %>%
    mutate(uid = ITEM_ID) %>%
    select(uid) %>%
    mutate(type = "eppi-machine") %>%
    mutate(name = classifier_name) %>%
    mutate(decision = "exclude") %>%
    mutate(decision = ifelse(uid %in% c(included$ITEM_ID),
                             "include", decision)) %>%
    mutate(cid = new_cid_no) %>%
    mutate(date = lubridate::dmy(date))
  
  all_score <- results %>% 
    select(uid = ITEM_ID, score) %>%
    arrange(uid)
  
  all_score$uid <- tolower(all_score$uid)
  
  to_write <- to_write %>%
    arrange(uid)
  
  to_write <- left_join(to_write, all_score, by="uid")
  
  # Write new included studies
  dbWriteTable(con, "study_classification", to_write, append=TRUE)
  message(paste0(length(to_write$uid), " new included citations written to database!"))
  
}

#' Run K-Fold Cross-Validation for Machine Learning
#'
#' @description
#' This function performs K-fold cross-validation. It retrieves data
#' from a database, splits it into `k` folds, trains and tests a classifier for each fold, and
#' computes performance metrics. Results are saved to CSV files for further analysis.
#'
#' @param con A database connection object.
#' @param project_name A string specifying the project name. Used for naming output files and directories.
#' @param sample Boolean true/false based on whether you want to use all screening decisions or take a random sample.
#' @param sample_number An integer specifying the number of studies you want in the sample.
#' @param file_id A string to be used as a file id to distinguish results, for repeat runs of this function on the same day.
#' @param fold_number An integer specifying the number of folds for cross-validation. Default is 5.
#' @param repeats An integer specifying how many repeats of the k-fold validation
#' @param training_prop A decimal between 0 and 1. The proportion of all of the screening decisions to be used for training. Set to 1 to use all studies for k-fold with no validation. Default is 0.8.
#' @param classifier_name A string specifying the classifier name to be used, taken from the study_classification table (e.g. "in-vivo", "in-vitro", "clinical")
#' @details
#' This function:
#' 1. Retrieves included and excluded screening decisions from the database.
#' 2. Shuffles the data and splits it into the specified number of folds.
#' 3. For each iteration:
#'    - Assigns a test set while others are used for training.
#'    - Runs the specified machine learning classifier via an external API.
#'    - Computes and saves performance metrics and test scores.
#' 
#' Results are saved in a directory named `screening/k-fold-validation` in the current working directory.
#'
#' @import dplyr tidyr readr janitor rsample caTools
#' @examples
#' \dontrun{
#' # Example usage:
#' run_k_fold(
#'   con = db_con,
#'   project_name = "MS-SOLES",
#'   sample = FALSE,
#'   file_id = "_1",
#'   fold_number = 5,
#'   repeats = 3,
#'   classifier_name = "animal"
#' )
#' }
#' @return
#' The function does not return an R object. It writes the results to the `screening/k-fold-validation` directory.
#' 
#' @export
#'
run_k_fold <- function(con, 
                       project_name = as.character(), 
                       sample = FALSE,
                       sample_number = as.numeric(),
                       file_id = "",
                       fold_number = 5,
                       repeats = 3,
                       training_prop = 0.8,
                       classifier_name = NULL) {
  
  # Create folders for saving data
  data_folder  <- "screening/k-fold-validation/"
  data_output  <- file.path(data_folder, "output/")
  data_results <- file.path(data_folder, "results/")
  
  dir.create(data_output,  recursive = TRUE, showWarnings = FALSE)
  dir.create(data_results, recursive = TRUE, showWarnings = FALSE)
  
  # Wrappers and config for EPPI ML algorithm
  source("/opt/sharedFolder/SSML/create_files_API.R")
  source("/opt/sharedFolder/SSML/JT_API_config.R")
  source("/opt/sharedFolder/SSML/JT_API_wrap.R")
  
  # Retrieve screening decisions from db
  message("Retrieving human screening decisions from the database...")
  screening_decisions <- get_screening_decisions(con, project_name, classifier_name = classifier_name)
  
  # Set seed and shuffle
  set.seed(123)
  screening_decisions <- screening_decisions[sample(nrow(screening_decisions)), ]
  
  # If user does not want use all screening decisions then use the sample argument
  if (sample){
    screening_decisions <- screening_decisions %>%
      sample_n(sample_number)
  }
  
  # If the user does not want to use a final validation set training_prop should equal 1
  if (training_prop == 1){
    
    training_set <- screening_decisions
    
  } else {
    
    # Split in to stratified split for training & validation
    # K-fold is then performed on the training data and a set held out for final validation. Default is 0.8
    split <- caTools::sample.split(
      screening_decisions$LABEL,
      SplitRatio = training_prop
    )
    
    training_set <- screening_decisions[split, ]
    validation_set <- screening_decisions[!split, ]
    
  }
  
  # Create dfs for folds, repeats & results
  test_scores_all <- data.frame()
  performance_all_folds <- data.frame()
  all_folds <- list()
  
  # Create the stratified folds for each repeat, shuffling the data for each repeat
  for (r in 1:repeats) {
    message(paste0("Repeat ", r, ": Creating stratified folds..."))
    
    screening_dec_incl <- training_set %>% filter(LABEL == 1)
    screening_dec_excl <- training_set %>% filter(LABEL == 0)
    
    set.seed(123 + r)
    shuffled_incl <- screening_dec_incl[sample(nrow(screening_dec_incl)), ]
    shuffled_excl <- screening_dec_excl[sample(nrow(screening_dec_excl)), ]
    
    incl_splits <- shuffled_incl %>%
      group_by((row_number()-1) %/% (n()/fold_number)) %>%
      tidyr::nest() %>% pull(data)
    
    excl_splits <- shuffled_excl %>%
      group_by((row_number()-1) %/% (n()/fold_number)) %>%
      tidyr::nest() %>% pull(data)
    
    for (i in 1:fold_number) {
      fold <- rbind(incl_splits[[i]], excl_splits[[i]])
      fold$fold <-  i
      fold$n_repeat <- r
      all_folds[[length(all_folds) + 1]] <- fold
    }
  }
  
  
  date <- format(Sys.Date(), "%d%m%y")
  
  # Bind all folds and repeats into 1 dataframe
  set <- do.call(rbind, all_folds) %>%
    mutate(date = date) %>%
    mutate(TEMP_ID = row_number())
  
  training_set <- set %>%
    filter(n_repeat == 1) %>%
    select(-fold, -n_repeat, -date)
  
  
  # Save validation set data, if there is a validation set
  if (training_prop != 1){
    
    # Get the max TEMP_ID from the full set
    max_temp_id <- max(set$TEMP_ID, na.rm = TRUE)
    
    # Add TEMP_ID to validation_set starting from max_temp_id + 1
    validation_set <- validation_set %>%
      mutate(TEMP_ID = seq(from = max_temp_id + 1, length.out = n()))
    
    write.csv(validation_set, paste0(data_output, "kfold_validation_set_", date, file_id, ".csv"), row.names = F)
  }
  
  # Write the sets to csv
  write.csv(training_set, paste0(data_output, "kfold_training_set_", date, file_id, ".csv"), row.names = F)
  write.csv(set, paste0(data_output, "kfold_full_splits_", date, file_id, ".csv"), row.names = F)
  
  # Iterate through each fold and each repeat, changing the calibration fold each time
  # Added error handling for failed ML attempts, each fold will get a max of 3 attempts
  for (r in 1:repeats) {
    for (j in 1:fold_number) {
      
      max_attempts <- 3
      attempt <- 1
      success <- FALSE
      
      while (attempt <= max_attempts && !success) {
        
        message(paste0("Repeat ", r, ", Fold ", j, ", Attempt ", attempt, ": Creating Test/Train split..."))
        
        tryCatch({
          current_set <- set %>%
            filter(n_repeat == r)
          
          # Split the data into train & calibrate, removing the labels for the calibration set
          calibration_set <- current_set$fold == j
          current_set$Cat <- ifelse(calibration_set, "Calibrate", "Train")
          
          # Format data to be run through ML
          current_set_processed <- current_set %>%
            mutate(LABEL = ifelse(Cat %in% c("Calibrate"), 99, LABEL),
                   REVIEW_ID = project_name) %>%
            select(REVIEW_ID, ITEM_ID, TITLE, ABSTRACT, LABEL, TEMP_ID, n_repeat, fold) %>%
            mutate(TITLE = stringr::str_squish(TITLE),
                   ABSTRACT = stringr::str_squish(ABSTRACT))
          
          write_tsv(
            current_set_processed,
            paste0(data_output, "kfold_repeat_", r, "_fold_", j,"_", date, file_id, ".tsv")
          )
          
          # Create iteration filenames
          filenames <- CreateFileNamesForIOEAPI(
            paste0(data_output, "kfold_repeat_", r, "_fold_",j, "_", date, file_id, ".tsv"),
            paste0(data_output, "kfold_repeat_", r, "_fold_",j, "_results_", date, file_id, ".tsv")
          )
          
          # Send data to ML via API and return results to output folder
          TrainCollection(filenames, projectId = paste0(project_name, "_calibration_repeat_", r, "_fold_", j, "_", date, file_id))
          
          
          message(paste0("Writing repeat_", r, ", fold_", j, " performance and scores to ", data_output))
          message(paste0("Repeat ", r, ", Fold ", j, ": Success"))
          success <- TRUE
          
        }, error = function(e) {
          message(paste("Error on repeat", r, "fold", j, "attempt", attempt))
          message(e$message)
        })
        
        
        if (!success) {
          attempt <- attempt + 1
          if (attempt <= max_attempts) {
            message(sprintf("Retrying... Attempt %d", attempt))
          } else {
            message(sprintf("Max attempts reached for repeat %d, fold %d. Skipping...", r, j))
          }
        } else{
          
          file.remove(paste0(data_output, "kfold_repeat_", r, "_fold_", j,"_", date, file_id, ".tsv"))
        }
      }
    }
  }
  
  # Gather all of the results
  ml_scores_list <- list()
  file_paths <- c()  # Store file paths her
  counter <- 1
  
  for (r in 1:repeats) {
    for (j in 1:fold_number) {
      
      file_path <- paste0(data_output, "kfold_repeat_", r, "_fold_", j, "_results_", date, file_id, ".tsv")
      
      ml_scores_list[[counter]] <- read_tsv(file_path)
      
      # save the file paths for removing after
      file_paths <- c(file_paths, file_path)
      
      counter <- counter + 1
    }
  }
  
  # Combine all results data frames in the list into a single data frame
  ml_scores <- bind_rows(ml_scores_list) %>%
    select(-Incl) %>%
    rename(TEMP_ID = PaperId)
  
  # Join using TEMP_ID to get the original UIDs for each study
  ml_results <- ml_scores %>%
    left_join(set , by = "TEMP_ID") %>%
    select(uid = ITEM_ID, decision = LABEL, score = probabilities, TEMP_ID, n_repeat, fold)
  
  write.csv(ml_results, paste0(data_output, "ml_scores_all_", date, file_id, ".csv"), row.names = F)
  
  try(file.remove(file_paths[file.exists(file_paths)]))
  
  # Apply function to each fold & repeat. Giving the performance metrics and threshold when recall >= 0.95
  performance_results <- ml_results %>%
    group_by(n_repeat, fold) %>%
    group_modify(~ evaluate_fold(.x)) %>%
    ungroup()
  
  write.csv(performance_results, paste0(data_results, "calibration_results_", date, file_id, ".csv"), row.names = F)
  
  # Summarise performance
  # Calculate mean, median & global threshold 
  summary_stats <- performance_results %>%
    summarise(
      mean_threshold = mean(threshold, na.rm = TRUE),
      median_threshold = median(threshold, na.rm = TRUE),
      global_threshold = evaluate_fold(ml_results) %>% pull(threshold),
      mean_recall = round(mean(recall, na.rm = TRUE),4),
      sd_recall = round(sd(recall, na.rm = TRUE),4),
      mean_specificity = round(mean(specificity, na.rm = TRUE),4),
      sd_specificity = round(sd(specificity, na.rm = TRUE),4),
      mean_f1 = round(mean(f1, na.rm = TRUE),4),
      sd_f1 = round(sd(f1, na.rm = TRUE),4),
      mean_f2 = round(mean(f2, na.rm = TRUE),4),
      sd_f2 = round(sd(f2, na.rm = TRUE),4)
    )
  
  write.csv(summary_stats, paste0(data_results, "summary_results_", date, file_id, ".csv"), row.names = F)
  
}

evaluate_fold <- function(df, score_col = "score", decimals = 4) {
  
  
  # Pull the score vector out of df using the column name provided
  score <- df[[score_col]]
  
  # Threshold grid resolution, e.g. decimals = 4 -> step = 0.0001
  # (matches the granularity of the original brute-force version, but
  # computed without looping over the data at every threshold)
  step <- 10^(-decimals)
  thresholds <- seq(0, 1, by = step)
  
  # --- Sort scores once, ascending ---
  # Once sorted, "how many scores fall below threshold X" becomes a simple
  # lookup via findInterval() instead of a fresh full-table scan per threshold.
  # This is the key trick that makes this O(n log n) instead of O(n * thresholds).
  ord    <- order(score)
  scores <- score[ord]
  labels <- df$decision[ord]   # reorder labels to match the sorted scores
  
  # Total counts of each class, needed to derive TP/FP from FN/TN below
  total_pos <- sum(labels == 1)
  total_neg <- sum(labels == 0)
  
  # Running (cumulative) counts of positives/negatives as we walk up the
  # sorted score list. cum_pos[i] = number of positive labels among the
  # i lowest-scoring rows. This lets us answer "how many positives/negatives
  # have a score below some cutoff" in O(1) per threshold via indexing.
  cum_pos <- cumsum(labels == 1)
  cum_neg <- cumsum(labels == 0)
  
  # For each threshold, find how many sorted scores are strictly less than it.
  # left.open = TRUE means we count scores < threshold (not <=), which
  # matches "pred = 1 if score >= threshold" -> everything below is pred = 0.
  # n_below is effectively the index into cum_pos/cum_neg for "count of
  # positives/negatives predicted negative at this threshold".
  n_below <- findInterval(thresholds, scores, left.open = TRUE)
  
  # FN = positives incorrectly predicted negative (score < threshold)
  # TN = negatives correctly predicted negative (score < threshold)
  # Guard against n_below == 0 (threshold below all scores -> no one predicted
  # negative yet), since cum_pos[0]/cum_neg[0] would be an invalid index.
  FN <- ifelse(n_below > 0, cum_pos[pmax(n_below, 1)], 0)
  TN <- ifelse(n_below > 0, cum_neg[pmax(n_below, 1)], 0)
  
  # TP/FP are just what's left over once FN/TN are known
  TP <- total_pos - FN
  FP <- total_neg - TN
  
  perf_df <- tibble::tibble(
    threshold   = thresholds,
    recall      = TP / (TP + FN),               # a.k.a. sensitivity / TPR
    specificity = TN / (TN + FP),                # a.k.a. TNR
    # +1e-10 avoids 0/0 = NaN when a threshold predicts nothing positive
    # (TP + FP == 0), at the cost of a negligible (~1e-10) numerical bias
    precision   = TP / (TP + FP + 1e-10),
  ) %>%
    dplyr::mutate(
      # Same epsilon trick applied to the harmonic-mean-based F-scores,
      # so they stay finite even when precision + recall would otherwise be 0
      f1 = 2 * (precision * recall) / (precision + recall + 1e-10),
      # F2 weights recall 4x more heavily than precision (beta = 2)
      f2 = 5 * (precision * recall) / (4 * precision + recall + 1e-10)
    )
  
  # We want the strictest (highest) threshold that still hits our recall
  # target, so first narrow down to thresholds meeting the recall floor
  valid <- perf_df %>% dplyr::filter(recall >= 0.95)
  
  # No threshold achieved the recall target for this fold -> caller should
  # handle NULL (e.g. skip this fold / score column)
  if (nrow(valid) == 0) return(NULL)
  
  # Among valid thresholds, pick the highest one (maximizes precision/
  # specificity while still satisfying recall >= 0.95)
  valid %>% dplyr::slice_max(threshold)
}


#' Run ML using the training and validation sets from K-fold
#'
#' @description
#' This function takes the stratified training/validation splits from the k-fold function and re-trains the algorithm using
#' all of the training data to get scores for the final validation set.
#' 
#' @import readr
#' @param con connection to db
#' @param project_name name or project identifier
#' @param training_set set of screened citations with 1/0 LABEL
#' @param validation_set set of citations to be screened for validation LABEL 99
#' @param file_id A string to be used as a file id to distinguish results, for repeat runs of this function on the same day.
#' @return A CSV results file of probability scores
#' @export
#'
run_ml_kfold_val <- function(con, training_set, validation_set, project_name, file_id = ""){
  
  # Create directories
  data_folder <- "screening/k-fold-validation/"
  data_output <- file.path(data_folder, "output/")
  
  dir.create(data_output, recursive = TRUE, showWarnings = FALSE)
  
  # Wrappers and config for EPPI ML algorithm
  source("/opt/sharedFolder/SSML/create_files_API.R")
  source("/opt/sharedFolder/SSML/JT_API_config.R")
  source("/opt/sharedFolder/SSML/JT_API_wrap.R")
  
  date <- format(Sys.Date(), "%d%m%y")
  
  validation_set <- validation_set %>% 
    mutate(LABEL = 99)
  
  # Merge labelled and unlabelled data
  ml_data <- rbind(training_set, validation_set)
  
  # Write training + validation file to tsv
  write_tsv(ml_data, paste0(data_output, "run_ml_", date, file_id, ".tsv"))
  
  # Create ML filenames
  ml_filenames <- CreateFileNamesForIOEAPI(
    paste0(data_output, "run_ml_", date, file_id, ".tsv"),
    paste0(data_output, "run_ml_", date,"_results", file_id, ".tsv")
  )
  
  # Send data to ML via API and return results to output folder
  TrainCollection(ml_filenames, projectId = paste0(project_name, "_run_ml_kfold_val_",date, file_id))
  
}

#' Evaluate Model Performance Across Thresholds
#'
#' This function calculates a variety of performance metrics (e.g., sensitivity, specificity, F1-score, etc.)
#' for a binary classifier across a range of thresholds. It merges predicted scores with ground truth labels,
#' computes metrics for predefined thresholds (mean, median, global), and also performs a full sweep across
#' thresholds. The sweep is calculated at fine (4dp) resolution internally, but only 2dp thresholds are
#' kept in the returned/written full sweep to keep the output compact. Results are optionally saved to a
#' CSV file and returned as a data frame.
#'
#' @param validation_set_scores A data frame containing the model's predicted probabilities.
#' @param validation_set_labels A data frame containing the human screening labels.
#' @param summary_results A named list or data frame containing predefined thresholds to evaluate.
#' @param file_id Optional string identifier to append to the output CSV filename. Defaults to an empty string.
#' @param download_res_csv Logical; if TRUE, writes the results to a CSV file.
#' @param calc_decimals Resolution of the internal threshold sweep (default 4, i.e. step = 0.0001).
#' @param output_decimals Resolution of thresholds kept in the returned/written full sweep (default 2,
#'   i.e. step = 0.01). Must be <= calc_decimals.
#'
#' @return A data frame containing threshold performance metrics for each threshold tested.
#' Results are also optionally written to a CSV file in the \code{screening/k-fold-validation/results/} directory.
#'
#' Output is saved to a CSV named \code{threshold_performance_<date><file_id>.csv}.
#'
#' @export
evaluate_model_performance <- function(validation_set_scores, validation_set_labels, summary_results,
                                       file_id = "", download_res_csv = TRUE,
                                       calc_decimals = 4, output_decimals = 2) {
  
  # Folder for results
  data_results <- "screening/k-fold-validation/results/"
  
  date <- format(Sys.Date(), "%d%m%y")
  
  # Merge and prepare the main dataframe
  df <- validation_set_scores %>%
    left_join(validation_set_labels, by = c("PaperId" = "TEMP_ID")) %>%
    rename(score = probabilities, label = LABEL)
  
  # Smoothing via a small epsilon is used to avoid division-by-zero errors during metric computation.
  eps <- 1e-10
  
  # --- Vectorized metric calculation for a vector of thresholds at once ---
  # Sorts scores once, then uses cumulative sums + findInterval to get TP/TN/FP/FN
  # for every threshold in a single pass, instead of rescanning the full dataframe
  # per threshold.
  calc_metrics_vec <- function(thresholds, scores, labels) {
    
    ord           <- order(scores)
    scores_sorted <- scores[ord]
    labels_sorted <- labels[ord]
    
    total_pos <- sum(labels_sorted == 1)
    total_neg <- sum(labels_sorted == 0)
    n_total   <- length(labels_sorted)
    
    cum_pos <- cumsum(labels_sorted == 1)
    cum_neg <- cumsum(labels_sorted == 0)
    
    # Count how many sorted scores fall strictly below each threshold
    # (predicted negative, since prediction = 1 when score >= threshold)
    n_below <- findInterval(thresholds, scores_sorted, left.open = TRUE)
    
    FN <- ifelse(n_below > 0, cum_pos[pmax(n_below, 1)], 0)
    TN <- ifelse(n_below > 0, cum_neg[pmax(n_below, 1)], 0)
    TP <- total_pos - FN
    FP <- total_neg - TN
    
    sensitivity <- TP / (TP + FN + eps)
    specificity <- TN / (TN + FP + eps)
    accuracy    <- (TP + TN) / (n_total + eps)
    precision   <- TP / (TP + FP + eps)
    f1_score    <- 2 * (precision * sensitivity) / (precision + sensitivity + eps)
    f2_score    <- (5 * (precision * sensitivity)) / ((4 * precision) + sensitivity + eps)
    
    data.frame(
      threshold   = thresholds,
      sensitivity = round(sensitivity, 3),
      specificity = round(specificity, 3),
      accuracy    = round(accuracy, 3),
      precision   = round(precision, 3),
      f1_score    = round(f1_score, 3),
      f2_score    = round(f2_score, 3)
    )
  }
  
  scores <- df$score
  labels <- df$label
  
  # --- Custom thresholds (mean, median, global) ---
  # These are exact values (not on any particular grid), so evaluated as-is
  custom_thresholds <- c(
    mean_threshold   = summary_results$mean_threshold,
    median_threshold = summary_results$median_threshold,
    global_threshold = summary_results$global_threshold
  )
  
  custom_results <- calc_metrics_vec(unname(custom_thresholds), scores, labels)
  custom_results$threshold_type <- names(custom_thresholds)
  custom_results <- custom_results %>% select(threshold_type, everything())
  
  # --- Full threshold sweep, calculated at fine (calc_decimals) resolution ---
  calc_step <- 10^(-calc_decimals)
  calc_thresholds <- seq(0, 1, by = calc_step)
  
  full_results_fine <- calc_metrics_vec(calc_thresholds, scores, labels)
  full_results_fine$threshold_type <- "all"
  
  # --- Thin down to output_decimals resolution before writing/returning ---
  # Keep only the rows whose threshold lands on the coarser output grid
  # (e.g. calc at 4dp, but only keep the 2dp values: 0.00, 0.01, 0.02, ...)
  out_step <- 10^(-output_decimals)
  full_results <- full_results_fine %>%
    filter(abs(threshold / out_step - round(threshold / out_step)) < 1e-8) %>%
    mutate(threshold = round(threshold, output_decimals)) %>%
    select(threshold_type, threshold, everything())
  
  # Combine for plotting or comparison ---
  combined_results <- bind_rows(custom_results, full_results)
  
  if (download_res_csv) {
    dir.create(data_results, recursive = TRUE, showWarnings = FALSE)
    write.csv(combined_results, paste0(data_results, "threshold_performance_", date, file_id, ".csv"))
  }
  return(combined_results)
}


#' Identify Studies for Error Correction Review
#'
#' This function identifies studies that should be re-reviewed based on 
#' disagreements between human screening decisions and machine-predicted scores.  
#' It takes the output from a single repeat of the `run_k_fold()` function (where 
#' training_proportion == 1) which includes human decisions and machine scores for each 
#' study, and then compares each study’s score against its corresponding fold-specific threshold.  
#'
#' Studies for which the machine prediction disagrees with the human decision 
#' are flagged as "disagreements." These can then be filtered, ranked, or sampled 
#' (e.g., selecting the most extreme cases, cases near the threshold, or random 
#' disagreements) depending on the user’s selection during interactive prompts.
#'
#' The function retrieves citation metadata from the database, merges it with the 
#' disagreement set, and outputs a SyRF-formatted CSV file containing the selected 
#' studies for re-review.
#'
#' @param con A database connection object (e.g., a \code{DBI} or \code{dplyr} 
#'   connection) pointing to a database containing a table named 
#'   \code{"unique_citations"}.
#'
#' @param k_fold_scores A character string specifying the file path to the CSV 
#'   containing machine scores from \code{run_k_fold()} (for one repeat).  
#'   The file must include columns for \code{uid}, \code{decision}, \code{score}, 
#'   \code{fold}, and \code{n_repeat}.
#'
#' @param k_fold_thresholds A character string specifying the file path to the CSV 
#'   containing the calibration results with fold-specific thresholds and performance metrics.  
#'   The file must contain \code{fold} column for merging.
#'
#' @param abstracts_only Logical; if \code{TRUE}, only disagreements with 
#'   non-missing, non-empty abstracts are retained.  
#'   Defaults to \code{FALSE}.
#'
#' @return  A SyRF-formatted CSV file is also written to disk via \code{solesR::get_syrf_sample()}.
#'
#' @section Interactive Behavior:
#' The function prompts the user to choose one of the following selection types:
#' \itemize{
#'   \item \strong{All} — return all disagreements.
#'   \item \strong{Random} — sample a user-specified number of disagreements.
#'   \item \strong{Nearest Threshold} — select disagreements with scores closest to the threshold.
#'   \item \strong{Extreme Discrepancies} — select disagreements with the largest absolute distance from the threshold.
#' }
#'
#' Additional prompts guide the user if selected quantities exceed the available 
#' disagreements in either human-included or human-excluded groups.
#'
#' @examples
#' \dontrun{
#' run_error_correction(
#'   con = my_connection,
#'   k_fold_scores = "k-fold-validation/output/ml_scores_all_091225for_EC.csv",
#'   k_fold_thresholds = "k-fold-validation/results/calibration_results_091225for_EC.csv",
#'   abstracts_only = TRUE
#' )
#' }
#'
#' @import dplyr
#' @importFrom janitor clean_names
#' @export
run_error_correction <- function(con,
                                 k_fold_scores = as.character(),
                                 k_fold_thresholds = as.character(),
                                 abstracts_only = FALSE) {
  
  set.seed(123)
  
  # Read in thresholds
  thresholds <- read.csv(k_fold_thresholds) %>% 
    filter(n_repeat == 1)
  
  # Check required columns in thresholds
  required_threshold_cols <- c("fold", "threshold", "n_repeat")
  missing_threshold_cols <- setdiff(required_threshold_cols, colnames(thresholds))
  
  if (length(missing_threshold_cols) > 0) {
    stop(paste0(
      "Thresholds file seems incorrect. Looking for calibration_results file from run_k_fold() results.\n",
      "Missing columns: ", paste(missing_threshold_cols, collapse = ", ")
    ))
  }
  
  # Read in scores and labels and connect to thresholds
  k_fold_scores <- read.csv(k_fold_scores) %>%
    filter(n_repeat == 1)
  
  # Check required columns in k_fold_scores
  required_scores_cols <- c("uid", "decision", "score", "fold", "n_repeat")
  missing_scores_cols <- setdiff(required_scores_cols, colnames(k_fold_scores))
  
  if (length(missing_scores_cols) > 0) {
    stop(paste0(
      "k_fold_scores file seems incorrect. Looking for ml_scores_all files from run_k_fold() output.\n",
      "Missing columns: ", paste(missing_scores_cols, collapse = ", ")
    ))
  }
  
  # Join with thresholds
  k_fold_scores <- k_fold_scores %>%
    left_join(thresholds, by = "fold") %>%
    janitor::clean_names() %>%
    select(uid, label = decision, threshold, score)
  
  # Calculate disagreements between human and machine
  disagreements <- k_fold_scores %>%
    mutate(disagreement = case_when(
      (label == 1 & score < threshold) ~ TRUE,
      (label == 0 & score >= threshold) ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    filter(disagreement == TRUE) %>% 
    mutate(uid = sub("^wos:", "wos-", uid))
  
  # Connect to metadata in database
  all_disagreements <- tbl(con, "unique_citations") %>%
    filter(uid %in% disagreements$uid) %>% 
    select(uid, title, abstract, author, year, doi, journal) %>%
    collect() %>% 
    left_join(disagreements, by = "uid") %>% 
    mutate(distance = abs(threshold - score))
  
  # Keep only the disagreements with abstracts for studies to be re-screened
  if (abstracts_only){
    all_disagreements <- all_disagreements %>%
      filter(!is.na(abstract) | !abstract == "")
    
    message(paste0("Total number of disagreements found (with abstracts): ", nrow(all_disagreements)))
    
  } else{
    
    message(paste0("Total number of disagreements found: ", nrow(all_disagreements)))
    
  }
  
  # Ask user which studies they would like for re-review
  type_answer <- menu(
    c("All", "Random", "Nearest Threshold", "Extreme Discrepancies"),
    title = paste0(
      "What studies would you like for re-review?"
    )
  )
  
  if (type_answer != 1){
    
    # If not all studies, how many for re-review
    num_studies <- as.numeric(
      readline("How many studies would you like for re-review? ")
    )
    
    if (num_studies > nrow(all_disagreements)) {
      stop("Requested more studies than available.")
    }
    
    cat("You selected type:", type_answer, "\n")
    cat("Number of studies:", num_studies, "\n")
    
    if (type_answer == 4){
      
      # Calculate number of disagreements which were included/excluded by the human reviewer
      # Take the most "extreme" disagreements from each side, at the specified amount
      message(paste0("Finding ", num_studies, " studies with the most extreme discrepancies between Human and Machine descisions..."))
      
      disagreements_human_included <- all_disagreements %>%
        filter(label == 1) %>%
        arrange(desc(distance)) %>%
        head(num_studies/2)
      
      disagreements_human_excluded <- all_disagreements %>%
        filter(label == 0) %>%
        arrange(desc(distance)) %>%
        head(num_studies/2)
      
      # If the requested number for re-screening is greater than the number of disagreements included by human, 
      # ask user to take disagreements from human excluded side
      if (nrow(disagreements_human_included) < (num_studies/2)){
        
        answer <- menu(
          c("Yes", "No"),
          title = paste0(
            "Number of disagreements between human and machine (which the human \"Included\"), is less than ", 
            (num_studies/2), 
            ".\n",
            "Would you like to take the remaining amount from the disagreements which the human \"Excluded\"?"
          )
        )
        
        
        if (answer == 1){
          
          disagreements_human_excluded <- all_disagreements %>%
            filter(label == 0) %>%
            arrange(desc(distance)) %>%
            head(num_studies - nrow(disagreements_human_included))
        }
        
        # If the requested number for re-screening is greater than the number of disagreements "Excluded" by human, 
        # ask user to take disagreements from human "Included" side
      }else if ((nrow(disagreements_human_excluded) < (num_studies/2))){
        
        answer <- menu(
          c("Yes", "No"),
          title = paste0(
            "Number of disagreements between human and machine (which the human \"Excluded\"), is less than ", 
            (num_studies/2), 
            ".\n",
            "Would you like to take the remaining amount from the disagreements which the human \"Included\"?"
          )
        )
        
        if (answer == 1){
          
          disagreements_human_included <- all_disagreements %>%
            filter(label == 1) %>%
            arrange(desc(distance)) %>%
            head(num_studies - nrow(disagreements_human_excluded))
          
        }
      }
      
      # Combine "extreme discrepancies" which the human included and excluded
      total_to_re_screen <- disagreements_human_excluded %>%
        rbind(disagreements_human_included)
      
      
      # If the user wants to re-review a certain number of random studies where the machine disagreed with the human reviewer
    } else if (type_answer == 3){
      
      message(paste0("Finding ", num_studies, " studies nearest the threshold which have potentially been misclassified"))
      
      # Calculate the num_studies nearest the threshold
      disagreements_human_included <- all_disagreements %>%
        filter(label == 1) %>%
        arrange(distance) %>%
        head(num_studies/2)
      
      disagreements_human_excluded <- all_disagreements %>%
        filter(label == 0) %>%
        arrange(distance) %>%
        head(num_studies/2)
      
      
      # If the requested number for re-screening is greater than the number of disagreements included by human, 
      # ask user to take disagreements from human excluded side
      if (nrow(disagreements_human_included) < (num_studies/2)){
        
        answer <- menu(
          c("Yes", "No"),
          title = paste0(
            "Number of disagreements between human and machine (which the human \"Included\"), is less than ", 
            (num_studies/2), 
            ".\n",
            "Would you like to take the remaining amount from the disagreements which the human \"Excluded\"?"
          )
        )
        
        if (answer == 1){
          
          disagreements_human_excluded <- all_disagreements %>%
            filter(label == 0) %>%
            arrange(distance) %>%
            head(num_studies - nrow(disagreements_human_included))
        }
        
        # If the requested number for re-screening is greater than the number of disagreements "Excluded" by human, 
        # ask user to take disagreements from human "Included" side
      }else if ((nrow(disagreements_human_excluded) < (num_studies/2))){
        
        answer <- menu(
          c("Yes", "No"),
          title = paste0(
            "Number of disagreements between human and machine (which the human \"Excluded\"), is less than ", 
            (num_studies/2), 
            ".\n",
            "Would you like to take the remaining amount from the disagreements which the human \"Included\"?"
          )
        )
        
        if (answer == 1){
          
          disagreements_human_included <- all_disagreements %>%
            filter(label == 1) %>%
            arrange(distance) %>%
            head(num_studies - nrow(disagreements_human_excluded))
          
        }
      }
      
      # Combine studies nearest the threshold which the human included and excluded
      total_to_re_screen <- disagreements_human_excluded %>%
        rbind(disagreements_human_included)
      
    } else if (type_answer == 2){
      
      # Calculate a random sample of num_studies specified by the user for re-review
      message(paste0("Finding ", num_studies, " studies at random with disagreements between the human and machine..."))
      
      total_to_re_screen <- all_disagreements[sample(nrow(all_disagreements), num_studies), ]
      
    }
  } else if (type_answer == 1){
    
    # Return all studies for re-review
    message(paste0("Finding all studies at with disagreements between the human and machine..."))
    
    total_to_re_screen <- all_disagreements
    
  }
  
  # Shuffle the dataframe before returning for re-review
  total_to_re_screen <- total_to_re_screen[sample(nrow(total_to_re_screen)), ]
  
  # Use get_syrf_sample to return csv in correct format for SyRF
  solesR::get_syrf_sample(total_to_re_screen, sample_size = nrow(total_to_re_screen), abstracts_only = FALSE)
  
  message(paste0(nrow(total_to_re_screen), " studies for re-review written to syrf_sample_date.csv"))
  
}

#' Run machine learning screening at a fixed threshold
#'
#' Trains the classifier on the k-fold training set and scores the validation and
#' unscreened studies, using the median k-fold threshold to include or exclude.
#' Writes the unscreened studies' decisions to `study_classification` and the
#' validation performance to `ml_performance`.
#'
#' @param con A DBI database connection.
#' @param project_name ML project name.
#' @param classifier_name Classifier name, e.g. "in-vivo".
#' @param training_set Training set from `run_k_fold()`.
#' @param validation_set Validation set from `run_k_fold()`.
#' @param unscreened_set Unscreened studies from `get_studies_to_screen()`.
#' @param summary_results Summary results from `run_k_fold()`; `median_threshold` is used.
#' @param file_id Optional string passed to `evaluate_model_performance()`.
#' @return Called for its side effects.
#' @import DBI dplyr readr
#' @importFrom stringr str_squish str_replace_all
#' @export
run_ml_at_threshold <- function(con, 
                                project_name = "", 
                                classifier_name = "in-vivo", 
                                training_set,
                                validation_set,
                                unscreened_set, 
                                summary_results,
                                file_id = ""){
  
  # Threshold used to classify records as include/exclude
  threshold <- summary_results$median_threshold
  
  # Create directories
  dir.create("screening", showWarnings = F)
  dir.create("screening/output", showWarnings = F)
  
  # Load ML files
  source("/opt/sharedFolder/SSML/create_files_API.R")
  source("/opt/sharedFolder/SSML/JT_API_config.R")
  source("/opt/sharedFolder/SSML/JT_API_wrap.R")
  source("/opt/sharedFolder/SSML/ML_analysis.R")
  
  # Set date
  date <- format(Sys.Date(), "%y%m%d")
  
  ml_data <- validation_set %>% 
    mutate(LABEL = 99) %>% 
    rbind(training_set) %>% 
    select(-TEMP_ID) %>% 
    rbind(unscreened_set) %>% 
    mutate(TEMP_ID = row_number())
  
  validation_TEMP_IDs <- ml_data %>% 
    filter(ITEM_ID %in% validation_set$ITEM_ID) %>% 
    pull(TEMP_ID)
  
  # Strip line breaks/tabs from text so records don't split across rows in the TSV
  ml_data <- ml_data %>%
    mutate(across(where(is.character),
                  ~ stringr::str_squish(str_replace_all(.x, "[\r\n\t]+", " "))))
  
  # Write unscreened data to tsv
  write_tsv(ml_data, paste0("screening/output/ml_run_",date,".tsv"))
  
  # Create ML filenames
  ml_filenames <- CreateFileNamesForIOEAPI(
    paste0("screening/output/ml_run_",date,".tsv"),
    paste0("screening/output/ml_run_",date,"_results.tsv")
  )
  
  # Send data to ML via API and return results to output folder
  TrainCollection(ml_filenames, projectId = paste0(project_name, "_run_ml_",date))
  
  # Read in results
  ml_scores <- read_tsv(paste0("screening/output/ml_run_",date,"_results.tsv"))
  
  # Gather latest CID  
  latest_CID <- tbl(con, "study_classification") %>% 
    select(cid) %>% 
    collect() %>% 
    pull(cid) %>% 
    max() + 1
  
  # Read in scores and process those above threshold
  new_ml_scores <- ml_scores %>%
    select(-Incl, TEMP_ID = PaperId) %>%
    filter(!TEMP_ID %in% validation_TEMP_IDs) %>% 
    left_join(ml_data, by = "TEMP_ID") %>%
    select(uid = ITEM_ID, score = probabilities) %>%
    mutate(decision = ifelse(score >= threshold, "include", "exclude")) %>%
    mutate(type = "eppi-machine",
           name = classifier_name,
           cid = latest_CID,
           date = Sys.Date())
  
  # Write data to table
  dbWriteTable(con, "study_classification", new_ml_scores, append = T)
  
  validation_scores <- ml_scores %>% 
    filter(PaperId %in% validation_TEMP_IDs) 
  
  validation_labels <- validation_set %>%
    select(ITEM_ID, LABEL) %>% 
    left_join(ml_data %>% select(ITEM_ID, TEMP_ID), by ="ITEM_ID")
  
  
  # Calculate validation set performance
  performance_metrics <- evaluate_model_performance(validation_set_scores = validation_scores, 
                                                    validation_set_labels = validation_labels, 
                                                    summary_results = summary_results, 
                                                    file_id = file_id, download_res_csv = FALSE)
  
  # Format data for ml_performance table
  latest_performance <- performance_metrics %>%
    filter(threshold_type == "median_threshold") %>% 
    mutate(
      balanced_accuracy = (sensitivity + specificity) / 2,
      utility = (5/6) * sensitivity + (1/6) * specificity,
      cid = latest_CID
    ) %>% 
    select(threshold, utility, specificity, sensitivity, precision, f1 = f1_score, balanced_accuracy, cid)
  
  
  dbWriteTable(con, "ml_performance", latest_performance, append = T)
  
  
}