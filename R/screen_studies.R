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
#' @param classifier_name A string representing the classifier's name.
#' @param screening_decisions_review_id A string indicating the review ID for retrieving human screening decisions.
#' @param fold_number An integer specifying the number of folds for cross-validation. Default is 5.
#'
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
#' @import dplyr tidyr readr janitor
#' @examples
#' \dontrun{
#' # Example usage:
#' run_k_fold(
#'   con = db_con,
#'   project_name = "MS-SOLES",
#'   classifier_name = "in-vivo",
#'   screening_decisions_review_id = "ms-soles-screening",
#'   fold_number = 5
#' )
#' }
#' @return
#' The function does not return an R object. It writes the results to the `k-fold-validation` directory.
#' 
#' @export
#'
run_k_fold <- function(con, 
                       project_name = as.character(), 
                       classifier_name = as.character(), 
                       screening_decisions_review_id = as.character(),
                       fold_number = 5){
  
  message(paste0("Retrieving human screening decisions from the database..."))
  
  # Split the data
  screening_dec_incl <- get_screening_decisions(con, review_id = screening_decisions_review_id) %>%
    filter(LABEL == 1) 
  
  screening_dec_excl <- get_screening_decisions(con, review_id = screening_decisions_review_id) %>%
    filter(LABEL == 0) 
  
  # Shuffle the included & excluded datasets
  set.seed(123)
  shuffled_incl <- screening_dec_incl[sample(nrow(screening_dec_incl)), ]
  shuffled_excl <- screening_dec_excl[sample(nrow(screening_dec_excl)), ]
  
  incl_splits <- shuffled_incl %>% 
    group_by((row_number()-1) %/% (n()/fold_number)) %>%
    nest %>% pull(data)
  
  excl_splits <- shuffled_excl %>% 
    group_by((row_number()-1) %/% (n()/fold_number)) %>%
    nest %>% pull(data)
  
  # Initialize an empty list to store folds
  fold_list <- list()
  
  message(paste0("Creating ", fold_number, " folds..."))
  
  # Loop through each fold
  for (i in 1:fold_number) {
    
    # Combine the included and excluded splits for each fold
    fold <- rbind(incl_splits[[i]], excl_splits[[i]])
    
    # Add a new column indicating the fold number
    fold$fold <- paste0("fold_", i)
    
    # Append the fold to the fold_list
    fold_list[[i]] <- fold
  }
  
  # Combine all folds into a single data frame
  set <- do.call(rbind, fold_list)
  
  test_scores_all <- data.frame()
  performance_all_folds <- data.frame()
  
  for (j in 1:fold_number) {
    
    tryCatch({
      
      message(paste0("Creating Test/Train split for fold ", j))
      
      # Run the ML "fold_number" times assigning a new fold as "Test" each time
      test_rows <- set$fold == paste0("fold_", j)
      set$Cat <- ifelse(test_rows, "Test", "Train")
      
      # create directories
      dir.create("k-fold-validation", showWarnings = FALSE)
      dir.create("k-fold-validation/output", showWarnings = FALSE)
      dir.create("k-fold-validation/results", showWarnings = FALSE)
      
      # codes for JTML
      source("/opt/sharedFolder/SSML/create_files_API.R")
      source("/opt/sharedFolder/SSML/JT_API_config.R")
      source("/opt/sharedFolder/SSML/JT_API_wrap.R")
      source("/opt/sharedFolder/SSML/ML_analysis.R")
      
      # set date
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
      dataFolder <- "k-fold-validation/output/"
      k_fold_folder <- "k-fold-validation/results/"
      
      
      # Write data and label files out as IOE API protocol
      outputFilenames <- CreateMLFilenames(paste0(dataFolder,project_name, "_"), new_cid)
      
      # Round down the test percentage
      test_percentage <- set %>%
        filter(Cat == "Test")
      
      fraction_fold <- nrow(test_percentage) / nrow(set)
      
      fraction_rounded <- floor(fraction_fold * 100) / 100
      
      # Send the data to API and write the results
      allDecisions <- WriteFilesForIOE(set, outputFilenames, testPercentage = fraction_rounded)
      ifilenames <- CreateFileNamesForIOEAPI(outputFilenames$Decisions, outputFilenames$Results)
      
      # Main wrapper for API call - write file to container, run API, download results file
      TrainCollection(ifilenames, gsub("[-]", "", paste0(project_name, "_set_",  j, "_", date)))
      
      
      # Performance calculations
      analysisResult <- FindBestPerformance(outputFilenames$Results,
                                            outputFilenames$AllDecisions,
                                            outputFilenames$Analysis)
      
      # Get threshold
      threshold <- analysisResult[which(as.logical(analysisResult[,"Chosen"])), "Threshold"][[1]]
      
      # Make dateframe of performance at given threshold
      performance <- as.data.frame(analysisResult)
      
      performance <- performance %>%
        filter(Chosen == 1) %>% 
        janitor::clean_names()
      
      # format performance
      performance <- performance %>%
        mutate(fold = j) %>%
        select(-chosen)
      
      # Create a dataframe with the performance results from each fold
      performance_all_folds <- bind_rows(performance_all_folds, performance)
      
      write.csv(performance, file = paste0(dataFolder, project_name, "_k_fold_perf_fold_", j, "_", date,".csv"))
      
      test_set <- read_tsv(outputFilenames$AllDecision) %>%
        filter(Category == "Test") %>%
        select(REVIEW_ID, ITEM_ID, LABEL) %>% 
        janitor::clean_names()
      
      test_ids <- read_tsv(outputFilenames$AllDecision) %>%
        select(TEMP_ID, ITEM_ID)
      
      scores <- read_tsv(ifilenames$ResultsFileName) %>%
        left_join(test_ids, by = c("PaperId" = "TEMP_ID")) %>%
        select(score = probabilities, uid = ITEM_ID)
      
      test_set_scores <- test_set %>% 
        left_join(scores, by = c("item_id" = "uid")) %>% 
        mutate(fold = j)
      
      # Create a df with all test fold scores combined
      test_scores_all <- bind_rows(test_scores_all, test_set_scores)
      
      message(paste0("Writing fold_", j, " performance and scores to ", k_fold_folder))
      
      write.csv(test_set_scores, file = paste0(dataFolder, project_name, "_test_scores_fold_", j, "_", date,".csv"))
      
    }, error = function(e) {
      print(paste("Error on fold", j))
      print(e)
    })
  }
  
  write.csv(test_scores_all, file = paste0(k_fold_folder, project_name, "_test_scores_all", "_", date,".csv"))
  write.csv(performance_all_folds, file = paste0(k_fold_folder, project_name, "_perf_all_folds_", date,".csv"))
  
}

#' Run Error Correction
#'
#' This function identifies and selects studies for re-review based on disagreements between human and machine decisions.
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
                                 k_fold_performance = as.character(),
                                 type = "extreme discrepancies",
                                 number_to_be_re_reviewed = as.numeric()){
  
  set.seed(123)
  
  # Read in performance and results files
  performance <- read.csv(k_fold_performance) %>%
    group_by(fold) %>%
    slice_head() %>%
    select(threshold, fold)
  
  k_fold_scores <- read.csv(k_fold_scores) %>%
    left_join(performance, by = "fold")
  
  # Work out the number of disagreements between human and machine and the avg threshold
  disagreements <- k_fold_scores %>%
    mutate(correction_needed = case_when(
      (label == 1 & score < threshold) ~ TRUE,
      (label == 0 & score >= threshold) ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    filter(correction_needed == TRUE)
  
  # Calculate the number of folds
  number_folds <- length(unique(k_fold_scores$fold))
  
  message(paste0("Total number of disagreements found: ", nrow(disagreements)))
  
  unique_citations <- tbl(con, "unique_citations") %>%
    select(uid, title, abstract, author, year, doi, journal) %>%
    collect()
  
  # Keep only the disagreements with abstracts for studies to be re-screened
  disagreements_abstracts_only <- disagreements %>%
    select(uid = item_id, score, label) %>%
    left_join(unique_citations, by = c("uid")) %>%
    filter(!((abstract == "") | is.na(abstract)))
  
  message(paste0("Total number of disagreements with abstracts found: ", nrow(disagreements_abstracts_only)))
  
  
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
      
      message(paste0("Number of disagreements between human and machine (which the human \"Included\"), is less than ", (number_to_be_re_reviewed/2),
                     "."))
      
      message(paste0("Would you like to take the remaining amount from the disagreements which the human \"Excluded\"?"))
      message(paste0("(Yes/No)"))
      
      answer <- readline()
      
      if (tolower(answer) == "yes"){
        
        disagreements_human_excluded <- disagreements_abstracts_only %>%
          filter(label == 0) %>%
          arrange(desc(score)) %>%
          head(number_to_be_re_reviewed - nrow(disagreements_human_included))
        
      }
      
      # If the requested number for re-screening is greater than the number of disagreements "Excluded" by human, ask user to take disagreements from human "Included" side
    }else if ((nrow(disagreements_human_excluded) < (number_to_be_re_reviewed/2))){
      
      message(paste0("Number of disagreements between human and machine (which the human \"Excluded\"), is less than ", (number_to_be_re_reviewed/2),
                     "."))
      
      message(paste0("Would you like to take the remaining amount from the disagreements which the human \"Included\"? (yes/no)"))
      
      answer <- readline()
      
      if (tolower(answer) == "yes"){
        
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
  total_to_re_screen <- get_syrf_sample(total_to_re_screen, sample_size = nrow(total_to_re_screen))
  return(new_total_to_re_screen)
  
  message(paste0(nrow(total_to_re_screen), " studies for re-review written to syrf_sample_date.csv and returned in dataframe"))
}

#' Add Corrected Errors to Screening Decisions
#'
#' Incorporates corrected screening decisions into the database.
#' This function requires a database connection object, a file containing corrected screening decisions, and the systematic search name.
#'
#' @param con A database connection object.
#' @param syrf_file A character string specifying the file path containing corrected screening decisions.
#' @param sys_search_name A character string specifying the name of the systematic search for the re-screened studies.
#'
#' @return This function does not explicitly return a value. It updates the database with corrected
#' screening decisions if specified and saves pre and post-correction human decisions to .fst files.
#'
#'
#' @import dplyr
#' @import stringr
#' @importFrom lubridate ymd
#' @importFrom stringr str_replace
#' @importFrom janitor clean_names
#' @importFrom fst write.fst
#' @importFrom DBI dbWriteTable
#' @importFrom utils menu
#'
#' @export
add_corrected_errors <- function(con,
                                 syrf_file = "",
                                 sys_search_name = ""){
  
  if (!file.exists("screening")){
    dir.create("screening")
  }
  if (!file.exists("screening/error_correction")){
    dir.create("screening/error_correction")
  }
  
  # Previous human decisions
  all_decisions <- tbl(con, "study_classification") %>%
    collect()
  
  human_decisions <- all_decisions %>%
    filter(type == "human_reviewer") %>%
    collect()
  
  # Writing pre_correction decisions to csv
  message("Writing pre-correction decisions to fst in screening/error_correction.")
  
  filename_all <- paste0("screening/error_correction/pre_correction_all_decisions_", Sys.Date(), ".fst")
  write.fst(all_decisions, filename_all)
  
  filename_human <- paste0("screening/error_correction/pre_correction_human_decisions_", Sys.Date(), ".fst")
  write.fst(human_decisions, filename_human)
  
  # Clean latest decisions chosen for error correction
  corrected_studies <- read.csv(syrf_file) %>%
    janitor::clean_names() %>%
    filter(systematic_search_name == sys_search_name) %>%
    select(uid = custom_id, date = date_time_of_screening, decision = screening_status, systematic_search_name) %>%
    mutate(uid = tolower(uid),
           decision = tolower(decision),
           date = Sys.Date(),
           type = "human_reviewer",
           name = "in-vivo",
           score = NA,
           cid = 101) %>%
    mutate(decision = ifelse(decision == "included", "include", "exclude")) %>%
    group_by(uid, decision) %>%
    slice_head() %>%
    ungroup()
  
  # Remove decisions to be corrected
  human_decisions_remove_corrected <- human_decisions %>%
    filter(!uid %in% corrected_studies$uid)
  
  # Add latest error corrected decisions back in
  human_decisions_updated <- corrected_studies %>%
    select(-systematic_search_name) %>%
    rbind(human_decisions_remove_corrected) %>%
    mutate(uid = str_replace(uid, "^wos:", "wos-"))
  
  # Check the uid's line up with previous decisions
  decisions_check <- human_decisions %>%
    select(uid, old_decisions = decision, old_date = date) %>%
    left_join(corrected_studies, by = "uid") %>%
    filter(!is.na(date))
  
  message(paste0("Number of studies re-screened for correction: ", nrow(decisions_check)))
  
  # Count the number of errors which has been corrected
  error_correction_decisions <- corrected_studies %>%
    filter(systematic_search_name == sys_search_name) %>%
    select(uid, new_decision = decision, new_decision_date = date) %>%
    left_join(human_decisions, by = "uid") %>%
    mutate(error_corrected = ifelse(decision != new_decision, TRUE, FALSE)) %>%
    select(uid, new_decision, new_decision_date, decision, date, error_corrected) %>%
    count(error_corrected) %>%
    filter(error_corrected == TRUE)
  
  message(paste0("Number of decisions changed: ", error_correction_decisions))
  
  update_db <- menu(c("Yes", "No"),
                    title = "Would you like to update the \"study_classification\" table in the database with the new decisions?")
  
  if (update_db == 1){
    
    # Writing to db and saving fst in screening/error_correction folder
    message("Updating database and writing post-correction decisions to fst in screening/error_correction")
    dbWriteTable(con, "study_classification", human_decisions_updated, overwrite = TRUE)
    filename <- paste0("screening/error_correction/post_correction_human_decisions_", Sys.Date(), ".fst")
    write.fst(human_decisions_updated, filename)
    
  } else if(update_db == 2){
    
    message("Database not updated with new screening decisions")
    
  }
}

#' ML Multi-validation function
#'
#' @description
#' This function performs a multi-validation for a machine learning algorithm, splitting data into folds
#' and processing it for model training, calibration, and validation.
#'
#' @param con A database connection object.
#' @param review_id A string indicating the review ID.
#' @param project_name A string for the name of the project.
#'
#' @import tidyr
#' @import dplyr 
#' @import stringr
#' @import readr
#' @import caret
#
#' @examples
#' \dontrun{
#' ml_multi_validation(con = db_con, review_id = "my_soles_screening", project_name = "my_soles_project")
#' }
#' @return Writes output to .csv files
#' @export
#'
#'
ml_multi_validation <- function(con, review_id = "", project_name= ""){
  
  # Load source files ============================================================
  source("/opt/sharedFolder/SSML/create_files_API.R")
  source("/opt/sharedFolder/SSML/JT_API_config.R")
  source("/opt/sharedFolder/SSML/JT_API_wrap.R")
  source("/opt/sharedFolder/SSML/ML_analysis.R")
  
  fold_number <- 5
  sys_date <- format(Sys.Date(), "%d-%m-%Y")
  
  message(paste0("Retrieving human screening decisions from the database..."))
  
  # Split the data between included and excluded decisions
  screening_dec_incl <- get_screening_decisions(con, review_id = review_id) %>%
    filter(LABEL == 1)
  
  screening_dec_excl <- get_screening_decisions(con, review_id = review_id) %>%
    filter(LABEL == 0)
  
  # Shuffle the decisions
  set.seed(123)
  shuffled_incl <- screening_dec_incl[sample(nrow(screening_dec_incl)), ]
  shuffled_excl <- screening_dec_excl[sample(nrow(screening_dec_excl)), ]
  
  # Create the splits
  incl_splits <- shuffled_incl %>% 
    group_by((row_number()-1) %/% (n()/fold_number)) %>%
    nest %>% pull(data)
  
  excl_splits <- shuffled_excl %>% 
    group_by((row_number()-1) %/% (n()/fold_number)) %>%
    nest %>% pull(data)
  
  fold_list <- list()
  
  message(paste0("Creating ", fold_number, " folds..."))
  
  # Loop through each fold
  for (i in 1:fold_number) {
    
    # Combine the included and excluded splits for each fold
    fold <- rbind(incl_splits[[i]], excl_splits[[i]])
    
    # Add a new column indicating the fold number
    fold$fold <- i
    
    # Append the fold to the fold_list
    fold_list[[i]] <- fold
  }
  
  set <- do.call(rbind, fold_list)
  
  # Function to create the different fold sequences
  generate_sequence <- function() {
    base_seq <- 1:5
    result <- list()
    
    for (i in 1:5) {
      # Shift the first element to the front
      first_num <- base_seq[i]
      remaining <- base_seq[-i]
      
      # Now rotate the remaining elements 4 times
      for (j in 0:3) {
        rotated <- c(first_num, remaining[(1+j):(4+j) %% 4 + 1])
        result[[length(result) + 1]] <- rotated
      }
    }
    
    # Convert result to a dataframe
    sequence_df <- as.data.frame(do.call(rbind, result))
    
    # Add 'iteration' column
    sequence_df$iteration <- 1:nrow(sequence_df)
    
    # Add 'Cat' column for each position
    colnames(sequence_df) <- c("Validate", "Calibrate", "Train1", "Train2", "Train3", "iteration")
    
    return(sequence_df)
  }
  
  sequence <- generate_sequence()
  
  # Create an empty list to store each iteration's dataframe
  all_iterations <- list()
  
  # Loop over each iteration in the sequence
  for (iter in 1:nrow(sequence)) {
    
    # Extract the current fold assignment for this iteration
    fold_assignment <- sequence[iter, 1:5]
    
    # Create a copy of the original data for this iteration
    iteration_df <- set
    
    iteration_df <- iteration_df %>%
      mutate(Cat = case_when(
        fold == fold_assignment$Validate ~ "Validate",
        fold == fold_assignment$Calibrate ~ "Calibrate",
        fold == fold_assignment$Train1 ~ "Train1",
        fold == fold_assignment$Train2 ~ "Train2",
        fold == fold_assignment$Train3 ~ "Train3"
      ))
    
    # Add the iteration column
    iteration_df$iteration <- iter
    
    # Append to the list
    all_iterations[[iter]] <- iteration_df
  }
  
  # Combine all iterations into a single dataframe
  labelled_data_assigned <- do.call(rbind, all_iterations)
  
  labelled_data_assigned_summary <- labelled_data_assigned %>%
    select(iteration, LABEL) %>%
    group_by(iteration, LABEL) %>%
    count() %>%
    pivot_wider(id_cols = iteration, 
                names_from = LABEL, 
                names_glue = "LABEL_{LABEL}_n",
                values_from = n) %>%
    mutate(total_n = LABEL_0_n + LABEL_1_n)
  
  # Create file paths for output
  path <- "screening/validation/output"
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message(paste("Directory created:", path))
  } else {
    message(paste("Directory already exists:", path))
  }
  
  write.csv(labelled_data_assigned, paste0("screening/validation/labelled_data_assigned_", sys_date, ".csv"), row.names = F)
  write.csv(labelled_data_assigned_summary, paste0("screening/validation/labelled_data_assigned_summary_", sys_date, ".csv"), row.names = F)
  
  labelled_data_assigned_iteration <- labelled_data_assigned
  
  labelled_data_assigned_iteration$TEMP_ID <- 1:nrow(labelled_data_assigned_iteration)
  
  write.csv(labelled_data_assigned_iteration,
            paste0("screening/validation/labelled_data_assigned_iteration_", sys_date, ".csv"),
            row.names = F)
  
  # Prepare each fold to go through ML ===========================================
  
  # When passing data into the ML, only the training set should be labelled with
  # 1 or 0. The Calibration and validation set should be given 99 labels (e.g. 
  # unknown).
  
  #Format data to be run through ML
  labelled_data_assigned_iteration_processed <- labelled_data_assigned_iteration %>%
    mutate(LABEL = ifelse(Cat %in% c("Calibrate", "Validate"), 99, LABEL),
           REVIEW_ID = review_id) %>%
    select(REVIEW_ID, ITEM_ID, TITLE, ABSTRACT, LABEL, TEMP_ID, iteration) %>%
    mutate(TITLE = str_squish(TITLE),
           ABSTRACT = str_squish(ABSTRACT))
  
  
  # Set up for ML and run ML =====================================================
  n_iterations <- max(labelled_data_assigned_iteration_processed$iteration)
  
  # Loop through iterations
  for (i in 1:n_iterations) {
    # Initialize attempt counter
    attempt <- 1
    success <- FALSE
    
    # Retry loop
    while (attempt <= 3 && !success) {
      tryCatch(
        {
          # Write data for each iteration
          write_tsv(
            filter(labelled_data_assigned_iteration_processed, iteration == i) %>%
              select(-iteration),
            paste0("screening/validation/labelled_data_iteration_", i, "_", sys_date, ".tsv")
          )
          
          # Create iteration filenames
          iteration_filenames <- CreateFileNamesForIOEAPI(
            paste0("screening/validation/labelled_data_iteration_", i, "_", sys_date, ".tsv"),
            paste0("screening/validation/output/labelled_data_iteration_", i, "_results_", sys_date, ".tsv")
          )
          
          # Send data to ML via API and return results to output folder
          TrainCollection(iteration_filenames, projectId = paste0(project_name, "_validation_iteration_", i, "_", sys_date))
          
          # If successful, mark success and exit retry loop
          success <- TRUE
        },
        error = function(e) {
          message(paste0("Error in iteration ", i, ", attempt ", attempt, ": ", e$message))
          attempt <- attempt + 1
          if (attempt > 3) {
            message(paste0("Failed to process iteration ", i, " after 3 attempts."))
          }
        }
      )
    }
  }
  
  # Process scores from ML =======================================================
  
  # Initialize an empty list to store each iteration's data frame
  ml_scores_list <- list()
  
  for (i in 1:n_iterations) {
    
    file_path <- paste0("screening/validation/output/labelled_data_iteration_", i, "_results_", sys_date, ".tsv")
    
    # Read the file, add the iteration column, and store it in the list
    ml_scores_list[[i]] <- read_tsv(file_path) %>%
      mutate(iteration = i)
  }
  
  # Combine all data frames in the list into a single data frame
  ml_scores <- bind_rows(ml_scores_list) %>%
    select(-Incl, TEMP_ID = PaperId)
  
  # match with input data
  ml_results <- merge(labelled_data_assigned_iteration, ml_scores, 
                      by = c("TEMP_ID", "iteration"), all = T) %>%
    select(iteration, uid = ITEM_ID, decision = LABEL, fold, Cat, score = probabilities)
  
  # Calculate performance at each threshold ======================================
  
  # Create a dataframe with just calibration data for analysis
  ml_results_calibrate <- ml_results %>%
    filter(Cat == "Calibrate") %>%
    mutate(decision = factor(decision))
  
  # Assign 1 or 0 to each score at each threshold from 0.01 to 1 (0.01 increments)
  for(i in seq(0.01,1,by=0.01)){
    col <- paste("Threshold", i, sep= "_")
    ml_results_calibrate[[col]] <- as.factor(ifelse(ml_results_calibrate$score >= i, 1, 0))
  }
  
  # Create vectors containing names of all columns relevant to regex tiab screening
  cols <- colnames(select(ml_results_calibrate,contains("Threshold_")))
  
  # Create empty dataframe for results
  results <- data.frame(matrix(nrow = 0, ncol = 8))
  
  # Loop over iterations
  for (i in 1:n_iterations){
    # Get the calibration set data for the iteration
    iteration_calibration <- filter(ml_results_calibrate, iteration == i)
    # Calculate the results for the iteration across all thresholds
    result <- data.frame(iteration = i,
                         threshold = seq(0.01,1, by = 0.01),
                         recall = lapply(iteration_calibration[cols],
                                         sensitivity,
                                         reference = iteration_calibration$decision,
                                         positive = 1) %>%
                           unlist() %>%
                           unname(),
                         specificity = lapply(iteration_calibration[cols],
                                              specificity,
                                              reference = iteration_calibration$decision,
                                              negative = 0) %>%
                           unlist() %>%
                           unname(),
                         tp = lapply(iteration_calibration[cols],
                                     function(x){x$tpos <- nrow(iteration_calibration %>%
                                                                  filter(x == 1 & decision == 1))}) %>%
                           unlist() %>%
                           unname(),
                         tn = lapply(iteration_calibration[cols],
                                     function(x){x$tneg <- nrow(iteration_calibration %>%
                                                                  filter(x == 0 & decision == 0))}) %>%
                           unlist() %>%
                           unname(),
                         fp = lapply(iteration_calibration[cols],
                                     function(x){x$fpos <- nrow(iteration_calibration %>%
                                                                  filter(x == 1 & decision == 0))}) %>%
                           unlist() %>%
                           unname(),
                         fn = lapply(iteration_calibration[cols],
                                     function(x){x$fneg <- nrow(iteration_calibration %>%
                                                                  filter(x == 0 & decision == 1))}) %>%
                           unlist() %>%
                           unname()) %>%
      mutate(precision = tp / (tp + fp),
             f1 = (2 * precision * recall)/(precision + recall))
    
    # Combine with full dataset
    results <- rbind(results, result)
  }
  
  # Initialize an empty list to store each iteration's results
  results_best_list <- list()
  
  # Loop through each iteration
  for (i in 1:n_iterations) {
    # Filter for the current iteration, get rows with recall >= 0.95, and keep the last row
    results_best_list[[i]] <- results %>%
      filter(iteration == i, recall >= 0.95) %>%
      tail(1)
  }
  
  # Combine all results into a single data frame
  results_best <- bind_rows(results_best_list)
  
  # Get ml scores for validation =================================================
  # Create empty dataframe for results
  ml_results_validate <- data.frame(matrix(nrow = 0, ncol = 4))
  
  for (i in 1:n_iterations){
    val <- ml_results %>%
      filter(Cat == "Validate") %>%
      filter(iteration == i) %>%
      mutate(ml_decision = ifelse(score >= results_best$threshold[results_best$iteration == i], 1, 0)) %>%
      mutate(decision = factor(decision)) %>%
      mutate(ml_decision = factor(ml_decision))
    
    ml_results_validate <- rbind(ml_results_validate, val)
  }
  
  # Create empty dataframe for validation results
  results_val <- data.frame(matrix(nrow = 0, ncol = 7))
  
  # Loop over iterations
  for (i in 1:n_iterations){
    # Get validation set data for iteration
    iteration_validate <- ml_results_validate %>% filter(iteration == i)
    # Calculate results for validation in each iteration
    result_val <- data.frame(iteration = i,
                             recall = sensitivity(iteration_validate$ml_decision,
                                                  reference = iteration_validate$decision,
                                                  positive = 1),
                             specificity = specificity(iteration_validate$ml_decision,
                                                       reference = iteration_validate$decision,
                                                       negative = 0),
                             tp = nrow(filter(iteration_validate, decision == 1 & ml_decision == 1)),
                             tn = nrow(filter(iteration_validate, decision == 0 & ml_decision == 0)),
                             fp = nrow(filter(iteration_validate, decision == 0 & ml_decision == 1)),
                             fn = nrow(filter(iteration_validate, decision == 1 & ml_decision == 0))) %>%
      mutate(precision = tp / (tp + fp),
             f1 = (2 * precision * recall)/(precision + recall))
    
    results_val <- rbind(results_val, result_val)
  }
  
  write.csv(ml_scores, paste0("screening/validation/output/ml_scores_", sys_date, ".csv"),  row.names = F)
  write.csv(results, paste0("screening/validation/output/result_calibrate_", sys_date, ".csv"), row.names = F)
  write.csv(results_best, paste0("screening/validation/output/result_calibrate_best_", sys_date, ".csv"), row.names = F)
  write.csv(results_val, paste0("screening/validation/output/result_validate_", sys_date, ".csv"), row.names = F)
}