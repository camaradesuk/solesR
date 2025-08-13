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
#'
#' @return A data frame containing screening decisions with columns: ITEM_ID, LABEL, TITLE, ABSTRACT, KEYWORDS, Cat, REVIEW_ID.
#'
#' @examples
#' \dontrun{
#'   screening_decisions <- get_screening_decisions(con = your_database_connection, review_id = "your_project_plus_date")
#' }
#' @import dplyr
#' @export
#'
get_screening_decisions <- function(con, review_id = ""){
  
  screening_decisions <- tbl(con, "study_classification") %>%
    filter(type == "human_reviewer") %>%
    left_join(tbl(con, "unique_citations"), by = "uid") %>%
    select(ITEM_ID = uid, LABEL = decision, TITLE = title, ABSTRACT = abstract, KEYWORDS = keywords) %>%
    mutate(LABEL = ifelse(LABEL == "include", 1, 0),
           Cat = "",
           REVIEW_ID = review_id) %>%
    collect()
  
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
#' Results are saved in a directory named `k-fold-validation` in the current working directory.
#'
#' @import dplyr tidyr readr janitor rsample
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
#' The function does not return an R object. It writes the results to the `k-fold-validation` directory.
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
  data_folder <- "k-fold-validation/"
  data_output <- "k-fold-validation/output/"
  data_results <- "k-fold-validation/results/"
  
  dir.create(data_folder, showWarnings = FALSE)
  dir.create(data_output, showWarnings = FALSE)
  dir.create(data_results, showWarnings = FALSE)
  
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
    split <- rsample::initial_split(screening_decisions, prop = training_prop, strata = "LABEL")
    
    training_set <- rsample::training(split)
    validation_set  <- rsample::testing(split)
    
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
  
  
  # Function to calculate performance at the threshold that gives ≥ 0.95 recall
  evaluate_fold <- function(df) {
    thresholds <- seq(0, 1, by = 0.01)
    
    perf_df <- map_dfr(thresholds, function(thresh) {
      
      df_thresh <- df %>%
        mutate(pred = ifelse(score >= thresh, 1, 0))
      
      TP <- sum(df_thresh$pred == 1 & df_thresh$decision == 1)
      TN <- sum(df_thresh$pred == 0 & df_thresh$decision == 0)
      FP <- sum(df_thresh$pred == 1 & df_thresh$decision == 0)
      FN <- sum(df_thresh$pred == 0 & df_thresh$decision == 1)
      
      recall <- TP / (TP + FN)
      specificity <- TN / (TN + FP)
      precision <- TP / (TP + FP + 1e-10)
      
      f1 <- 2 * (precision * recall) / (precision + recall + 1e-10)
      f2 <- 5 * (precision * recall) / (4 * precision + recall + 1e-10)
      
      tibble(threshold = thresh, recall, specificity, precision, f1, f2)
    })
    
    # Keep only rows with recall >= 0.95
    valid <- perf_df %>% filter(recall >= 0.95)
    
    # Skip if no valid threshold found
    if (nrow(valid) == 0) return(NULL)  
    
    # Use the threshold that achieves the recall goal
    best <- valid %>% slice_max(threshold)
    
    return(best)
  }
  
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
      mean_recall = round(mean(recall, na.rm = TRUE),3),
      sd_recall = round(sd(recall, na.rm = TRUE),3),
      mean_specificity = round(mean(specificity, na.rm = TRUE),3),
      sd_specificity = round(sd(specificity, na.rm = TRUE),3),
      mean_f1 = round(mean(f1, na.rm = TRUE),3),
      sd_f1 = round(sd(f1, na.rm = TRUE),3),
      mean_f2 = round(mean(f2, na.rm = TRUE),3),
      sd_f2 = round(sd(f2, na.rm = TRUE),3)
    )
  
  write.csv(summary_stats, paste0(data_results, "summary_results_", date, file_id, ".csv"), row.names = F)
  
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
  data_folder <- "k-fold-validation/"
  data_output <- "k-fold-validation/output/"
  
  dir.create(data_folder, showWarnings = F)
  dir.create(data_output, showWarnings = F)
  
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
#' thresholds from 0 to 1 by increments of 0.01. The results are saved to a CSV file and returned as a data frame.
#'
#' @param validation_set_scores A data frame containing the model's predicted probabilities. 
#' @param validation_set_labels A data frame containing the human screening labels.
#' @param summary_results A named list or data frame containing predefined thresholds to evaluate.
#' @param file_id Optional string identifier to append to the output CSV filename. Defaults to an empty string.
#'
#' @return A data frame containing threshold performance metrics for each threshold tested.
#' Results are also written to a CSV file in the \code{k-fold-validation/results/} directory.
#'
#' Output is saved to a CSV named \code{threshold_performance_<date><file_id>.csv}.
#'
#' @export
evaluate_model_performance <- function(validation_set_scores, validation_set_labels, summary_results, file_id = "") {
  
  # Create folders
  data_folder <- "k-fold-validation/"
  data_output <- "k-fold-validation/output/"
  data_results <- "k-fold-validation/results/"
  
  date <- format(Sys.Date(), "%d%m%y")
  
  # Merge and prepare the main dataframe
  df <- validation_set_scores %>%
    left_join(validation_set_labels, by = c("PaperId" = "TEMP_ID")) %>% 
    rename(score = probabilities, label = LABEL)
  
  # Smoothing via a small epsilon is used to avoid division-by-zero errors during metric computation.
  eps <- 1e-10
  
  # Define function to calculate metrics for a given threshold
  calc_metrics <- function(threshold) {
    df$prediction <- ifelse(df$score >= threshold, 1, 0)
    
    TP <- sum(df$prediction == 1 & df$label == 1)
    TN <- sum(df$prediction == 0 & df$label == 0)
    FP <- sum(df$prediction == 1 & df$label == 0)
    FN <- sum(df$prediction == 0 & df$label == 1)
    
    sensitivity <- TP / (TP + FN + eps)
    specificity <- TN / (TN + FP + eps)
    accuracy <- (TP + TN) / (TP + TN + FP + FN + eps)
    precision <- TP / (TP + FP + eps)
    f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity + eps)
    f2_score <- (5 * (precision * sensitivity)) / ((4 * precision) + sensitivity + eps)
    
    return(data.frame(
      threshold = threshold,
      sensitivity = round(sensitivity, 3),
      specificity = round(specificity, 3),
      accuracy = round(accuracy, 3),
      precision = round(precision, 3),
      f1_score = round(f1_score, 3),
      f2_score = round(f2_score, 3)
    ))
  }
  
  # Calculate metrics for custom thresholds (mean, median, global)
  custom_thresholds <- c(
    mean_threshold = summary_results$mean_threshold,
    median_threshold = summary_results$median_threshold,
    global_threshold = summary_results$global_threshold
  )
  
  # Compute metrics for each custom threshold
  custom_results <- lapply(names(custom_thresholds), function(name) {
    metrics <- calc_metrics(custom_thresholds[[name]])
    metrics$threshold_type <- name
    return(metrics)
  }) %>%
    bind_rows() %>%
    select(threshold_type, everything())
  
  # Calculate metrics for full threshold sweep (0 to 1 by 0.01)
  all_thresholds <- seq(0, 1, by = 0.01)
  
  # Compute metrics for each threshold in the sweep
  full_results <- lapply(all_thresholds, function(thresh) {
    metrics <- calc_metrics(thresh)
    metrics$threshold <- thresh
    metrics$threshold_type <- "all"
    return(metrics)
  }) %>%
    bind_rows() %>%
    select(threshold_type, threshold, everything())
  
  # Combine for plotting or comparison ---
  combined_results <- bind_rows(custom_results, full_results)
  
  write.csv(combined_results, paste0(data_results, "threshold_performance_", date, file_id, ".csv"))
  
  return(combined_results)
}

#' Run Error Correction
#'
#' This function identifies and selects studies for re-review based on disagreements between human and machine decisions.
#' This function takes the results from the run_k_fold function on 1 repeat, which will contain a human screening decision and a machine score for each study.
#'
#' @param con A database connection object.
#' @param k_fold_scores A character string specifying the file path to the CSV containing machine scores.
#' @param k_fold_performance A character string specifying the file path to the CSV containing performance metrics.
#' @param type A character string specifying the type of selection method. Options are "extreme discrepancies" or "random".
#' @param number_to_be_re_reviewed An integer specifying the number of studies to be re-reviewed.
#'
#' @return A dataframe containing the selected studies for re-review.
#'
#' @examples
#' \dontrun{
#' # Run error correction with default parameters
#' run_error_correction(con = my_connection,
#'                      k_fold_scores = "k_fold_scores.csv",
#'                      k_fold_performance = "k_fold_performance.csv",
#'                      type = "extreme discrepancies",
#'                      number_to_be_re_reviewed = 100)
#' }
#'
#' @export
run_error_correction <- function(con,
                                 k_fold_scores = as.character(),
                                 k_fold_thresholds = as.character(),
                                 type = "extreme discrepancies",
                                 number_to_be_re_reviewed = as.numeric()){
  
  set.seed(123)
  
  thresholds <- read.csv(k_fold_thresholds)
  
  k_fold_scores <-  read.csv(k_fold_scores) %>%
    left_join(thresholds, by = "fold") %>%
    janitor::clean_names() %>%
    select(uid, label = decision, fold, threshold, score)
  
  disagreements <- k_fold_scores %>%
    mutate(disagreement = case_when(
      (label == 1 & score < threshold) ~ TRUE,
      (label == 0 & score >= threshold) ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    filter(disagreement == TRUE) %>% 
    mutate(uid = sub("^wos:", "wos-", uid))
  
  
  # Calculate the number of folds
  number_folds <- length(unique(k_fold_scores$fold))
  
  message(paste0("Total number of disagreements found: ", nrow(disagreements)))

  
  unique_citations <- tbl(con, "unique_citations") %>%
    filter(uid %in% disagreements$uid) %>% 
    select(uid, title, abstract, author, year, doi, journal) %>%
    collect()
  
  # Keep only the disagreements with abstracts for studies to be re-screened
  disagreements_abstracts_only <- disagreements %>%
    select(uid, score, label) %>%
    left_join(unique_citations, by = c("uid")) 
  
  message(paste0("Total number of disagreements found: ", nrow(disagreements_abstracts_only)))
  
  if (type == "extreme discrepancies"){
    
    message(paste0("Finding ", number_to_be_re_reviewed, " studies with the most extreme discrepancies between Human and Machine descisions..."))
    
    # Calculate number of disagreements which were included/excluded by the human reviewer
    # Take the most "extreme" disagreements from each side, at the specified amount
    disagreements_human_included <- disagreements_abstracts_only %>%
      filter(label == 1) %>%
      arrange(score) %>%
      head(number_to_be_re_reviewed/2)
    
    disagreements_human_excluded <- disagreements_abstracts_only %>%
      filter(label == 0) %>%
      arrange(desc(score)) %>%
      head(number_to_be_re_reviewed/2)
    
    # If the requested number for re-screening is greater than the number of disagreements included by human, ask user to take disagreements from human excluded side
    if (nrow(disagreements_human_included) < (number_to_be_re_reviewed/2)){
      
      answer <- menu(
        c("Yes", "No"),
        title = paste0(
          "Number of disagreements between human and machine (which the human \"Included\"), is less than ", 
          (number_to_be_re_reviewed/2), 
          ".\n",
          "Would you like to take the remaining amount from the disagreements which the human \"Excluded\"?"
        )
      )
      
      
      if (answer == 1){
        
        disagreements_human_excluded <- disagreements_abstracts_only %>%
          filter(label == 0) %>%
          arrange(desc(score)) %>%
          head(number_to_be_re_reviewed - nrow(disagreements_human_included))
      }
      
      # If the requested number for re-screening is greater than the number of disagreements "Excluded" by human, ask user to take disagreements from human "Included" side
    }else if ((nrow(disagreements_human_excluded) < (number_to_be_re_reviewed/2))){
      
      answer <- menu(
        c("Yes", "No"),
        title = paste0(
          "Number of disagreements between human and machine (which the human \"Excluded\"), is less than ", 
          (number_to_be_re_reviewed/2), 
          ".\n",
          "Would you like to take the remaining amount from the disagreements which the human \"Included\"?"
        )
      )
      
      
      if (answer == 1){
        
        disagreements_human_included <- disagreements_abstracts_only %>%
          filter(label == 1) %>%
          arrange(desc(score)) %>%
          head(number_to_be_re_reviewed - nrow(disagreements_human_excluded))
        
      }
    }
    
    # Combine "extreme discrepancies" which the human included and excluded
    total_to_re_screen <- disagreements_human_excluded %>%
      rbind(disagreements_human_included)
    
    
    # If the user wants to re-review a certain number of random studies where the machine disagreed with the human reviewer
  } else if (type == "random"){
    
    message(paste0("Finding ", number_to_be_re_reviewed, " studies at random with disagreements between the human and machine..."))
    
    total_to_re_screen <- disagreements_abstracts_only[sample(nrow(disagreements_abstracts_only), number_to_be_re_reviewed), ]
    
  }
  
  # Shuffle the dataframe before returning for re-review
  total_to_re_screen <- total_to_re_screen[sample(nrow(total_to_re_screen)), ]
  
  # Use get_syrf_sample to return csv in correct format for SyRF
  total_to_re_screen <- get_syrf_sample(total_to_re_screen, sample_size = nrow(total_to_re_screen), abstracts_only = FALSE)
  
  message(paste0(nrow(total_to_re_screen), " studies for re-review written to syrf_sample_date.csv and returned in dataframe"))
  
  return(total_to_re_screen)
  
}