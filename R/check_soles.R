#' Check and optionally delete redundant tables in the database
#'
#' This function checks whether the column names of each table in the database
#' match the expected column names. Optionally, it can delete tables that are
#' redundant (not expected). It also checks the format of the scopus and wos
#' uids in the relevant columns, as well as the doi column in each table. Providing
#' the user with an option to update them to the correct format where necessary.
#'
#' @param con A database connection object.
#' @param delete_redundant Logical. If TRUE, delete redundant tables after
#'   saving them as .fst files.
#'
#' @return The function doesn't return anything, but prints messages indicating
#'   whether column names match and actions taken.
#'
#' @examples
#' \dontrun{
#' # Connect to the database
#' con <- pool::dbPool(RPostgres::Postgres(),
#'                     dbname = Sys.getenv("ad_soles_dbname"),
#'                     host = Sys.getenv("ad_soles_host"),
#'                     port = 5432,
#'                     user = Sys.getenv("ad_soles_user"),
#'                     password = Sys.getenv("ad_soles_password"))
#'
#' # Check tables and delete redundant ones
#' check_tables(con, delete_redundant = FALSE)
#'
#' # Close the database connection
#' poolClose(con)
#' }
#'
#' @import DBI
#' @import dplyr
#' @import fst
#' @import crayon
#' @importFrom utils menu
#' @export
check_tables <- function(con, delete_redundant = FALSE) {
  
  # Get the list of tables in the database
  dbtables <- dbListTables(con)
  
  # Define expected tables
  expected_tables <- c("unique_citations", "retrieved_citations",
                       "study_classification", "ml_performance",
                       "full_texts", "oa_tag", "rob_tag",
                       "pico_tag", "pico_ontology",
                       "pico_dictionary", "open_data_tag",
                       "citation_source_match", "article_type")
  
  # Define the expected column names for each table
  expected_colnames <- list(
    unique_citations = c(
      "uid", "doi", "author", "year", "journal", "title", "abstract",
      "volume", "number", "pages", "isbn", "keywords", "url", "date",
      "issn", "source", "author_country", "author_affiliation", "ptype",
      "pmid", "secondarytitle"
    ),
    article_type = c("doi", "ptype", "language", "method"),
    retrieved_citations = c("uid", "label"),
    study_classification = c("uid", "name", "decision", "score", "date", "cid", "type"),
    ml_performance = c("threshold", "utility", "specificity", "sensitivity",
                       "precision", "f1", "balanced_accuracy", "cid"),
    full_texts = c("status", "doi", "path"),
    open_data_tag = c("doi", "is_open_data", "open_data_category", "is_open_code",
                      "open_data_statements", "open_code_statements", "method"),
    oa_tag = c("doi", "is_oa", "oa_status", "method"),
    
    rob_tag = c("is_blind", "is_random", "is_exclusion", "is_welfare", "is_interest",
                "blind_prob", "interest_prob", "welfare_prob", "random_prob", "exclusion_prob", "doi"
    ),
    pico_ontology = c(
      "regex_id", "type", "main_category", "sub_category1", "sub_category2"
    ),
    pico_dictionary = c("id", "name", "regex"),
    pico_tag = c("uid", "regex_id", "method", "frequency", "strings"),
    citation_source_match = c("uid", "doi", "pmid", "wos_accession", "scopus_accession")
  )
  
  # Function to check column names for a table
  check_tables_exist <- function() {
    
    if(all(expected_tables %in% dbtables)==TRUE) {
      
      cat(green("All required tables present!\n"))}
    
    else {
      missing_tables <- dbtables[which(!expected_tables %in% dbtables)]
      
      cat(red("Missing tables: ", missing_tables, "\n"))
    }}
  
  # Function to check and correct NAs
  check_and_correct_na <- function(table_name) {
    
    # Check if the table exists in the database
    if (dbExistsTable(con, table_name)) {
      
      # Read the table
      table <- dbReadTable(con, table_name)
      
      # Check for problematic values
      problematic_values <- c("na", "", " ")
      has_issues <- any(sapply(table, function(column) any(column %in% problematic_values, na.rm = TRUE)))
      
      if (has_issues) {
        
        cat(red("The table", table_name, "contains problematic values ('na', '', ' ').\n"))
        
        change_values <- utils::menu(c("Yes", "No"),
                              title = message("Would you like to correct these values by replacing them with NA?"))
        
        if (change_values == 1) {
          
          # Backup the table before changes
          if (!file.exists("db_tables_legacy")) {
            dir.create("db_tables_legacy")
          }
          cat("Saving unchanged", table_name, "table to fst file in db_tables_legacy folder...\n")
          
          legacy_path <- paste0("db_tables_legacy/", table_name, ".fst")
          
          if (!file.exists(legacy_path)) {
            write.fst(table, legacy_path)
          }
          
          # Correct the problematic values
          table_updated <- table %>%
            mutate_all(~ifelse(. %in% problematic_values, NA, .))
          
          # Update the database with the modified table
          dbWriteTable(con, name = table_name, value = table_updated, overwrite = TRUE)
          
          cat("Problematic values in", table_name, "corrected successfully.\n")
          
          # Log the changes
          log_message <- paste(Sys.time(), ": Corrected problematic NA/blank values in", table_name)
          write(log_message, file = "db_tables_legacy/change_log.txt", append = TRUE)
          cat("Updating log file...\n")
          
        } else {
          
          cat("No changes made.\n")
          
        }
      } else {
        
        cat(green("No problematic values found in", table_name, ".\n"))
        
      }
    } else {
      
      cat(red("Table", table_name, "does not exist in the database.\n"))
      
    }
  }
  
  # Function to check column names for a table
  check_column_names <- function(table_name) {
    if (table_name %in% expected_tables) {
      
      actual_column_names <- tbl(con, table_name) %>% colnames()
      expected_colnames_table <- expected_colnames[[table_name]]
      
      if (all(actual_column_names %in% expected_colnames_table) &&
          all(expected_colnames_table %in% actual_column_names)) {
        cat(green("Column names for", table_name, "match the expected column names.\n"))
      } else {
        cat(red("Column names for", table_name, "do NOT match the expected column names.\n"))
      }
    } else {
      cat("Table", table_name, "not currently required for SOLES. \n")
    }
  }
  
  # Function to check format of uids
  check_uids <- function(table_name){
    
    if (table_name %in% expected_tables) {
      
      if ("uid" %in% expected_colnames[[i]]){
        
        table <- dbReadTable(con, table_name)
        
        uids <- table$uid
        
        # Count number of incorrect wos ids
        wos_uids_count <- sum(grepl("^wos:", uids))
        
        if (wos_uids_count > 0) {
          cat(red("Number of uids starting with 'wos:' in", table_name, ":", wos_uids_count, "\n"))
          
          change_uids <- urils::menu(c("Yes", "No"),
                              title = message("Do you want to change all wos uids starting with 'wos:' to 'wos-'?"))
          
          if (change_uids == 1) {
            
            # Check to see if folder exists
            if (!file.exists("db_tables_legacy")) {
              dir.create("db_tables_legacy")
            }
            
            cat("Saving", table_name, "table to fst file in db_tables_legacy folder...\n")
            
            # Write unchanged table to legacy folder
            legacy_path <- paste0("db_tables_legacy/", table_name, ".fst")
            write.fst(table, legacy_path)
            
            # Change uids
            table$uid <- gsub("^wos:", "wos-", uids)
            
            # Update database with the modified table
            dbWriteTable(con, name = table_name, value = table, overwrite = TRUE)
            
            cat(wos_uids_count, "wos uids in", table_name,  "updated successfully.\n")
            
            # Log the changes
            log_message <- paste(Sys.time(), ": Changed", wos_uids_count, "wos uids in", table_name, "from 'wos:' to 'wos-'.")
            write(log_message, file = "db_tables_legacy/change_log.txt", append = TRUE)
            cat("Updating log file...\n")
            
          } else {
            
            cat("No changes made.\n")
          }
        } else {
          
          cat(green("uids for wos match the expected format.\n"))
        }
        
        # Connect to table again as it may have been updated above
        table <- dbReadTable(con, table_name)
        
        uids <- table$uid
        
        # Count number of incorrect scopus ids
        scopus_uids_count <- sum(grepl("^2-", uids))
        
        if (scopus_uids_count > 0){
          
          cat(red("Number of uids starting with '2-' in", table_name, ":", scopus_uids_count, "\n"))
          
          change_uids <- utils::menu(c("Yes", "No"),
                              title = message("Do you want to change all scopus uids starting with '2-' to 'scopus-2-'?"))
          
          if (change_uids == 1) {
            
            if (!file.exists("db_tables_legacy")) {
              dir.create("db_tables_legacy")
            }
            
            cat("Saving", table_name, "table to fst file in db_tables_legacy folder...\n")
            
            legacy_path <- paste0("db_tables_legacy/", table_name, ".fst")
            
            # Check to see if legacy file for this table already exists, if it does exist then leave it.
            if (!file.exists(legacy_path)) {
              
              write.fst(table, legacy_path)
              
            }
            
            # Change uids
            table$uid <- gsub("^2-", "scopus-2-", uids)
            
            # Update database with the modified table
            dbWriteTable(con, name = table_name, value = table, overwrite = TRUE)
            
            cat(scopus_uids_count, "scopus uids in", table_name,  "updated successfully.\n")
            
            # Log the changes
            log_message <- paste(Sys.time(), ": Changed", scopus_uids_count, "scopus uids in", table_name, "from '2-' to 'scopus-2-'.")
            write(log_message, file = "db_tables_legacy/change_log.txt", append = TRUE)
            cat("Updating log file...\n")
            
          } else {
            
            cat("No changes made.\n")
          }
          
        } else {
          cat(green("uids for scopus match the expected format.\n"))
        }
      }
    }
  }
  
  check_dois <- function(table_name){
    if (table_name %in% expected_tables) {
      
      if ("doi" %in% expected_colnames[[i]]){
        
        table <- dbReadTable(con, table_name)
        
        table_updated <- format_doi(table)
        
        doi_check <- table_updated %>%
          filter(!doi %in% table$doi)
        
        if (nrow(doi_check) > 0){
          
          cat(red("There are", nrow(doi_check), "dois in", table_name, "which are formatted incorrectly\n"))
          
          change_dois <- utils::menu(c("Yes", "No"),
                              title = message("Would you like to reformat them all using format_doi()"))
          
          if (change_dois == 1) {
            
            if (!file.exists("db_tables_legacy")) {
              dir.create("db_tables_legacy")
            }
            cat("Saving unchanged", table_name, "table to fst file in db_tables_legacy folder...\n")
            
            legacy_path <- paste0("db_tables_legacy/", table_name, ".fst")
            
            if (!file.exists(legacy_path)) {
              
              write.fst(table, legacy_path)
              
            }
            
            # Update database with the modified table
            dbWriteTable(con, name = table_name, value = table_updated, overwrite = TRUE)
            
            cat(nrow(doi_check), "dois in", table_name,  "reformatted successfully.\n")
            
            # Log the changes
            log_message <- paste(Sys.time(), ": Reformatted", nrow(doi_check), "dois in", table_name)
            write(log_message, file = "db_tables_legacy/change_log.txt", append = TRUE)
            cat("Updating log file...\n")
            
          } else{
            
            cat("No changes made.\n")
            
          }
        } else {
          
          cat(green("All dois formatted correctly\n"))
          
        }
      }
    }
  }
  
  
  # Check tables exist
  check_tables_exist()
  
  # Check column names for each table
  for(i in dbtables){
    
    check_column_names(i)
    check_uids(i)
    check_dois(i)
    check_and_correct_na(i)
  }
  
  if(delete_redundant == TRUE){
    
    message(paste("Warning: This will DELETE database tables that are redundant in the current SOLES workflow"))
    
    user_input <- utils::menu(c("Yes", "No"),
                       title= paste("Are you sure you want to proceed?"))
    
    
    if (user_input == "1") {
      
      redundant_tables <- dbtables[!dbtables %in% expected_tables]
      
      # Loop through the tables and write to fst files
      for (table_name in redundant_tables) {
        query <- paste("SELECT * FROM", table_name)
        df <- dbGetQuery(con, query)
        
        # Write the dataframe to an fst file
        fst::write_fst(df, paste0(table_name, ".fst"))
        message("saving a backup of", table_name, "as a .fst file before deleting...")
        
        # Remove the table from the database
        dbRemoveTable(con, table_name)
        message("removed", table_name, "from soles database")
        
      }
      
    } else {
      return("No tables have been removed.")}
    
  }
}