#' Format DOIs in a DataFrame
#'
#' This function formats Digital Object Identifiers (DOIs) in a DataFrame by
#' converting them to uppercase and standardizing their representation. It
#' performs various replacements to ensure consistent formatting for DOIs.
#'
#' @import dplyr
#'
#' @param df A DataFrame containing a column named 'doi' with DOI strings.
#'
#' @return A DataFrame with the 'doi' column formatted for consistency.
#' @export
format_doi <- function(df){

  df$doi <- tolower(df$doi)
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("%28", "(", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("%29", ")", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("http://dx.doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("https://doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("https://dx.doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("http://doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi: ", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi:", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi", "", x)))
  return(df)
}

#' Check and Summarize NA Counts in Each Column
#'
#' This function calculates and summarizes the count of NAs for each column in a data frame.
#' It groups the data by each column and provides the count of NAs within each group.
#'
#' @param df A data frame.
#'
#' @return A list containing data frames, each representing the count of NAs for a specific column.
#'
#' @details
#' This function iterates through each column in the data frame and groups the data by the values in that column.
#' It then calculates the count of NAs for each group and stores the results in a list. The list is printed, displaying
#' the NA counts for each column.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data <- data.frame(
#'   source = c("A", "B", "A", "B", "A"),
#'   col1 = c(1, 2, NA, 4, 5),
#'   col2 = c(NA, 2, 3, 4, 5)
#' )
#' check_na_cols(data)
#' }
#'
#' @import dplyr
#' @importFrom rlang sym
#'
#' @export
check_na_cols <- function(df){

  # Initialize an empty list to store the results for each column
  na_counts_list <- list()

  # Iterate through each column in the data frame
  for (col_name in colnames(df)) {
    # Group by the current column and count NAs
    na_counts <- df %>%
      group_by(source) %>%
      summarize(na_count = sum(is.na(!!sym(col_name))))

    # Add the result to the list
    na_counts_list[[col_name]] <- na_counts
  }

  # Print the results for each column
  for (col_name in names(na_counts_list)) {
    cat("NA counts for column '", col_name, "':\n", sep = "")
    print(na_counts_list[[col_name]])
    cat("\n")
  }
}

#' Convert a RISmed::Medline object into a SOLES pubmed search data frame
#'
#' @param M an object of class MedLine (package "RISmed")
#'
#' @return A dataframe formatted for the SOLES workflow
#'
#'

medline2df <- function(M){
  # get authors from medline object
  authors <- tibble(author = RISmed::Author(M),
                    pmid = RISmed::PMID(M)) %>%
    tidyr::unnest(author) %>%
    group_by(pmid) %>%
    tidyr::unite(c("LastName","ForeName") , col = "author", sep = ", ") %>%
    mutate(author = paste(author, collapse = "; ")) %>%
    select(pmid, author) %>%
    unique()

  # get all other metadata from medline object
  df <- tibble(keywords = RISmed::Keywords(M),
               abstract = RISmed::AbstractText(M), author_country = RISmed::Country(M),
               title = RISmed::ArticleTitle(M), pages = RISmed::MedlinePgn(M),
               issue = RISmed::Issue(M), volume = RISmed::Volume(M),
               year = RISmed::YearArticleDate(M), pmid = RISmed::PMID(M),
               doi = RISmed::DOI(M), issn = RISmed::ISSN(M),
               journal = RISmed::Title(M),
               url= paste0("https://www.ncbi.nlm.nih.gov/pubmed/", pmid),
               uid = paste0("pubmed-", pmid),
               source = "pubmed",
               date = format(Sys.Date(), "%d%m%y")
  ) %>%
    # reformat columns into soles format
    tidyr::unnest_longer(keywords) %>%
    mutate(keywords = stringr::str_trim(keywords, side = "both")) %>%
    group_by(pmid) %>%
    mutate(keywords = paste(keywords, collapse = "; ")) %>%
    unique() %>%
    select(-c(keywords_id)) %>%
    # add author metadata
    left_join(authors, by="pmid") %>%
    relocate(author)

  df[df == "NA"] <- NA
  return(df)

}

#' Get a sample of citations for screening in SyRF
#'
#' This function takes a dataframe and returns a random sample of specified size.
#' Optionally, it can filter the dataframe to include only rows with abstracts.
#' This function also writes the resulting dataframe to a csv file and saves in working directory.
#'
#' @import dplyr
#' @param df A dataframe containing citation data.
#' @param sample_size The size of the random sample to be obtained.
#' @param abstracts_only Logical, indicating whether to filter only rows with abstracts.
#'
#' @return A dataframe containing the random sample with specified columns.
#'
#' @examples
#' \dontrun{
#' # Get a random sample without removing rows containing no abstract
#' result <- get_syrf_sample(my_data, sample_size = 1000, abstracts_only = FALSE)
#' }
#'
#' @export
get_syrf_sample <- function(df, sample_size = 2000, abstracts_only = TRUE){

  date <- format(Sys.Date(), "%d%m%y")

  if (abstracts_only) {

    df_with_abstracts <- df %>%
      filter(!is.na(abstract)) %>%
      filter(!abstract == "")

    abstracts_only_sample <- df_with_abstracts[sample(nrow(df_with_abstracts), sample_size), ]

    syrf_sample <- abstracts_only_sample %>%
      rename(Authors = author,
             Title = title,
             Abstract = abstract,
             Year = year,
             DOI= doi,
             PublicationName = journal) %>%
      mutate(Url = "",
             AuthorAddress = "",
             AlternateName = "",
             ReferenceType = "",
             CustomId = uid,
             Keywords = "",
             PdfRelativePath = paste0(uid, ".pdf")) %>%
      select(Title,
             Authors,
             PublicationName,
             AlternateName,
             Abstract,
             Url,
             AuthorAddress,
             Year,
             DOI,
             ReferenceType,
             Keywords,
             PdfRelativePath,
             CustomId)
  } else{

    all_citations_sample <-  df[sample(nrow(df), sample_size), ]
    syrf_sample <- all_citations_sample %>%
      rename(Authors = author,
             Title = title,
             Abstract = abstract,
             Year = year,
             DOI= doi,
             PublicationName = journal)  %>%
      mutate(Url = "",
             AuthorAddress = "",
             AlternateName = "",
             ReferenceType = "",
             CustomId = uid,
             Keywords = "",
             PdfRelativePath = paste0(uid, ".pdf")) %>%
      select(Title,
             Authors,
             PublicationName,
             AlternateName,
             Abstract,
             Url,
             AuthorAddress,
             Year,
             DOI,
             ReferenceType,
             Keywords,
             PdfRelativePath,
             CustomId)

  }

  utils::write.csv(syrf_sample, paste0("syrf_sample_", date, ".csv"), row.names = F, quote=TRUE, na="")
  message("file syrf_sample_date.csv written to working directory!")
  return(syrf_sample)
}

#' Write the screening decision from SyRF to the database
#'
#' This function reads a CSV file containing SyRF screening decisions and uploads
#' the data to a specified database. If the data includes annotations, a message
#' is displayed, indicating that manual cleaning is required.
#'
#' @import dplyr
#' @importFrom janitor clean_names
#' @importFrom lubridate ymd
#' @importFrom DBI dbWriteTable
#'
#' @param con A connection to the target database.
#' @param file The file path to the SyRF screening data in CSV format.
#' @param with_annotations Logical, indicating whether the SyRF file contains annotations (TRUE/FALSE).
#' @param classifier_name The name of the classifier to train the data on.
#'
#' @return The function does not return a value but writes the processed data to the database.
#'
#' @examples
#' \dontrun{
#'   syrf_decisions_to_db(con = your_database_connection,
#'                        file = "path/to/your/data.csv",
#'                        with_annotations = FALSE,
#'                        classifier_name = "my-project")
#' }
#'
#' @export
syrf_decisions_to_db <- function(con,
                                 file = "",
                                 with_annotations = FALSE,
                                 classifier_name = ""){

  if (with_annotations == FALSE){

    training_data <- read.csv(file) %>%
      janitor::clean_names() %>%
      select(uid = custom_id, date = date_time_of_screening, decision = screening_status) %>%
      mutate(uid = tolower(uid),
             decision = tolower(decision),
             date = substr(date, 1, 10),
             date = lubridate::ymd(date),
             type = "human_reviewer",
             name = classifier_name,
             score = NA_real_,
             cid = 101) %>%
      mutate(decision = ifelse(decision == "included", "include", decision)) %>%
      mutate(decision = ifelse(decision == "excluded", "exclude", decision)) %>%
      filter(decision %in% c("include", "exclude")) %>%
      group_by(uid, decision) %>%
      slice_head()


    message("\033[31m", "Do you want to append the training data to the study_classification table in the database?  (yes/no): ", "\033[0m", appendLF = FALSE)

    # Read user input
    user_input <- readline()

    # Check the user's input
    if (tolower(user_input) == "yes") {

      message("Writing human decisions to study classification table in database")
      dbWriteTable(con, "study_classification", training_data, append = TRUE)

    } else {

      message("Aborting function, data has not be written to database")

      return()
    }

  } else if (with_annotations == TRUE){

    message("Data must be cleaned manually and written to study_classification")

  }
}

#' Check and optionally delete redundant tables in the database.
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

          change_uids <- menu(c("Yes", "No"),
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

          change_uids <- menu(c("Yes", "No"),
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

          change_dois <- menu(c("Yes", "No"),
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
  }

  if(delete_redundant == TRUE){

    message(paste("Warning: This will DELETE database tables that are redundant in the current SOLES workflow"))

    user_input <- menu(c("Yes", "No"),
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

#' Complete PICO Tagging Function
#'
#' This function checks how many studies are still to be tagged, then runs the pico_tag function multiple times until there are no more studies to tag.
#'
#' @param con A database connection object.
#' @param tag_method The tagging method to use, either "fulltext" or "tiabkw".
#' @param tag_type The type of PICO tag to apply (e.g. "intervention", "species" etc).
#' @param tag_main_category The main category of the PICO tag. Default is "all".
#'
#' @return The function does not explicitly return a value, but it updates the connected database with new pico tags.
#'
#' @details
#' This function retrieves studies that are already tagged using the specified tagging method, and then tags the remaining studies that meet
#' the specified criteria. It supports two tagging methods: "fulltext" and "tiabkw". The tagging process
#' involves using regular expressions to extract relevant information from the full text or title/abstract/keywords, depending on
#' the chosen method.
#'
#' @examples
#' \dontrun{
#' Tag studies using full text regex
#' complete_pico_tag(con, tag_method = "fulltext", tag_type = "intervention", tag_main_category = "all")
#'}
#' @import dplyr
#' @import dbplyr
#' @export
complete_pico_tag <- function(con, tag_method = "", tag_type = "", tag_main_category = "all"){

  #browser()
  got_ft <- tbl(con, "full_texts") %>% filter(status=="found") %>% select(doi, path)

  # Get included studies
  included <- tbl(con, "study_classification") %>% filter(decision == "include")

  # Get included relevant studies + metadata
  included_studies <- tbl(con, "unique_citations") %>%
    select(uid, title, abstract, keywords, doi) %>%
    semi_join(included, by="uid") %>%
    left_join(got_ft, by="doi") %>%
    collect()

  if (tag_method == "fulltext"){

    method <- "fulltext_regex"

    message(paste0("Retrieving studies already tagged using full text regex..."))

    if (tag_main_category == "all"){

      tagged <- tbl(con, "pico_ontology") %>%
        filter(type == tag_type) %>%
        left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
        filter(method == method) %>%
        select(uid, method, regex_id) %>%
        collect()

    }else{

      # Retrieve studies already tagged by full text
      tagged <- tbl(con, "pico_ontology") %>%
        filter(type == tag_type) %>%
        filter(main_category %in% tag_main_category) %>%
        left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
        filter(method == method) %>%
        select(uid, method, regex_id) %>%
        collect()
    }

    to_tag <- included_studies %>%
      filter(!uid %in% tagged$uid) %>%
      filter(!path == "")

    message(paste0("Total number of studies still to tag using full text: ", length(to_tag$uid)))
    pre_to_tag <- to_tag
    new_tags <- 0
    total_new_tags <- 0

    # Run loop until all remaining studies are tagged
    while (new_tags < length(to_tag$uid)){

      try(pico_tag(con, tag_type = tag_type,
                   tag_main_category = tag_main_category,
                   tag_method = tag_method,
                   extract_strings = FALSE))

      if (tag_main_category == "all"){

        post_tagged <- tbl(con, "pico_ontology") %>%
          filter(type == tag_type) %>%
          left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
          filter(method == method) %>%
          select(uid, method, regex_id) %>%
          collect()

      }else{

        # Retrieve studies already tagged by full text
        post_tagged <- tbl(con, "pico_ontology") %>%
          filter(type == tag_type) %>%
          filter(main_category %in% tag_main_category) %>%
          left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
          filter(method == method) %>%
          select(uid, method, regex_id) %>%
          collect()
      }

      post_to_tag <- included_studies %>%
        filter(!uid %in% post_tagged$uid) %>%
        filter(!path == "")

      # Calculate how many studies are still to be tagged
      new_tags <- length(pre_to_tag$uid) - length(post_to_tag$uid)
      total_new_tags <- total_new_tags + new_tags
      message(paste0("Number of studies to tag by ", tag_method, " : ", length(post_to_tag$uid)))

      # If there are no new studies still to be tagged exit the loop
      if (new_tags == 0){

        break

      }

      pre_to_tag <- post_to_tag

    }

    message(paste0(total_new_tags, " out of a possible ", length(to_tag$uid), " tagged for ", tag_type))

  } else if (tag_method == "tiabkw_regex"){

    pico_tag_method <- "tiabkw"

    message(paste0("Retrieving studies already tagged using tiabkw..."))

    if (tag_main_category == "all"){

      tagged <- tbl(con, "pico_ontology") %>%
        filter(type == tag_type) %>%
        left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
        filter(method == method) %>%
        select(uid, method, regex_id) %>%
        collect()

    }else{

      # Retrieve studies already tagged by full text
      tagged <- tbl(con, "pico_ontology") %>%
        filter(type == tag_type) %>%
        filter(main_category %in% tag_main_category) %>%
        left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
        filter(method == method) %>%
        select(uid, method, regex_id) %>%
        collect()
    }

    to_tag <- included_studies %>%
      filter(!uid %in% tagged$uid) %>%
      select(uid, title, abstract, keywords)

    message(paste0("Total number of studies still to tag using tiabkw: ", length(to_tag$uid)))
    pre_to_tag <- to_tag
    new_tags <- 0
    total_new_tags <- 0

    # Run loop until all remaining studies are tagged
    while (total_new_tags < length(to_tag$uid)){

      message("Running pico_tag for tiabkw...")
      try(pico_tag(con, tag_type = tag_type,
                   tag_main_category = tag_main_category,
                   tag_method = tag_method,
                   extract_strings = FALSE))

      if (tag_main_category == "all"){

        post_tagged <- tbl(con, "pico_ontology") %>%
          filter(type == tag_type) %>%
          left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
          filter(method == method) %>%
          select(uid, method, regex_id) %>%
          collect()

      }else{

        # Retrieve studies already tagged by full text
        post_tagged <- tbl(con, "pico_ontology") %>%
          filter(type == tag_type) %>%
          filter(main_category %in% tag_main_category) %>%
          left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
          filter(method == method) %>%
          select(uid, method, regex_id) %>%
          collect()
      }

      post_to_tag <- included_studies %>%
        filter(!uid %in% post_tagged$uid) %>%
        select(uid, title, abstract, keywords)

      # Calculate how many new studies have been tagged
      new_tags <- length(pre_to_tag$uid) - length(post_to_tag$uid)
      total_new_tags <- total_new_tags + new_tags
      message(paste0("Number of studies to still to tag by ", tag_method, " : ", length(post_to_tag$uid)))

      # If there are no new studies still to be tagged then exit the loop
      if (new_tags == 0){

        break

      }
      pre_to_tag <- post_to_tag
    }
    message(paste0(total_new_tags, " out of a possible ", length(to_tag$uid), " tagged for ", tag_type))
  }
}

#' Update Open Access, Open Data, and Risk of Bias tags
#'
#' This function tags all of the citations available for open access, open data and risk of bias.
#'
#' @param con A database connection object.
#' @param rob_max_file_size Maximum file size for RoB tagging.
#' @param email Email address for using Unpaywall.
#' @return None. Database tables will be updated.
#' @export
#'
#' @examples
#' \dontrun{
#' tag_update_oa_od_rob(rob_max_file_size = 500000, email = "example/email.com")
#' }
#' @details
#' The function retrieves studies to tag for OA and OD, and tags them using respective services.
#' It also tags studies for RoB based on PDF files within a specified size range.
#' The tagging process continues until no new studies are tagged or an error occurs.
#'
#'
#' @import dplyr
#' @import tidyr
#' @import DBI
#'
#'
tag_update_oa_od_rob <- function(con, rob_max_file_size = 500000, email = ""){

  # Tag all remaining untagged included studies for open access ------------------

  # Get records already tagged
  tagged <- DBI::dbReadTable(con, "oa_tag")

  # Included studies still to be tagged
  study_to_tag <- tbl(con, "unique_citations") %>%
    left_join(tbl(con, "study_classification"), by = "uid") %>%
    filter(decision == "include") %>%
    select(doi) %>% # select doi first to make collect quicker
    collect() %>%
    distinct() %>%
    filter(!doi %in% tagged$doi) %>%
    filter(!is.na(doi))

  message(paste0("Total number of studies to tag for Open Access: ", length(study_to_tag$doi)))
  total_new_oa_tags <- 0
  while (total_new_oa_tags < length(study_to_tag$doi)){


    # Papers tagged before running function
    pre_oa_tag <- DBI::dbReadTable(con, "oa_tag")

    try(oa_tag(con, email = email, n = 250))
    try(oa_tag(con, email = email, n = 250))

    # Papers tagged after running function
    post_oa_tag <- DBI::dbReadTable(con, "oa_tag")

    # If no new studies are tagged then exit the loop
    running_new_tags <- length(post_oa_tag$doi) - length(pre_oa_tag$doi)
    if (running_new_tags == 0){

      break

    }

    total_new_oa_tags <- length(post_oa_tag$doi) - length(tagged$doi)
    message(paste0("Total citations tagged for Open Access: ", total_new_oa_tags))

  }


  message(paste0(total_new_oa_tags, " out of a possible ", length(study_to_tag$doi), " tagged for Open Access Status"))



  # Tag all remaining included studies for Open Data---------------------

  tagged <- DBI::dbReadTable(con, "open_data_tag")
  path <- "full_texts/"
  text_files <- list.files(path = path, pattern = ".txt", full.names = TRUE)
  text_files <- gsub(paste0(path,"/"), "", text_files)
  text_files <- gsub("\\.txt", "", text_files)
  text_files <- gsub("%2F", "\\/", text_files)
  text_files  <- gsub("%3C", "<", text_files)
  text_files  <- gsub("%3E", ">", text_files)
  text_files  <- gsub("%3A", ":", text_files)
  text_files  <- gsub("%22", '"', text_files)
  text_files  <- gsub("%7C", "\\|", text_files)
  text_files  <- gsub("%3F", "\\?", text_files)
  text_files  <- gsub("%2A", "\\*", text_files)

  # Identify full texts that require tag
  ft_to_tag <- dbReadTable(con, "full_texts") %>% filter(status == "found") %>%
    filter(!doi %in% tagged$doi) %>%
    filter(doi %in% text_files)

  message(paste0("Total number of studies to tag for Open Data: ", length(ft_to_tag$doi)))
  total_new_od_tags <- 0
  while (total_new_od_tags < length(ft_to_tag$doi)){

    # Papers tagged before running function
    pre_od_tag <- DBI::dbReadTable(con, "open_data_tag")

    try(ods_tag(con, path = "full_texts/"))
    try(ods_tag(con, path = "full_texts/"))

    # Papers tagged before running function
    post_od_tag <- DBI::dbReadTable(con, "open_data_tag")

    # If no new studies are tagged then exit the loop
    running_od_tags <- length(post_od_tag$doi) - length(pre_od_tag$doi)
    if (running_od_tags == 0){

      break
    }

    total_new_od_tags <- length(post_od_tag$doi) - length(tagged$doi)
    message(paste0("Total citations tagged for Open Data: ", total_new_od_tags))
  }

  message(paste0(total_new_od_tags, " out of a possible ", length(ft_to_tag$doi), " tagged for Open Data"))



  # Tag all remaining included studies for Risk of Bias------------------

  rob_max_file_size <- rob_max_file_size
  rob_tagged <- dbReadTable(con, "rob_tag")

  # Get list of citations with pdfs
  pdfs_checked <- dbReadTable(con, "full_texts")

  # get list of smaller files
  all_files <- list.files(path="full_texts/", pattern=".txt", full.names= T)
  all_files <- gsub("\\/\\/", "/", all_files)


  # filter files based on size
  max_file_size <- rob_max_file_size
  all_files <- all_files[file.info(all_files)$size < max_file_size]
  all_files <- all_files[file.info(all_files)$size > 10]

  dir <- getwd()

  # get citations to tag
  citations_to_tag <- pdfs_checked %>%
    filter(status == "found") %>%
    rename(id = doi) %>%
    filter(!id %in% rob_tagged$doi) %>%
    select(path,id) %>%
    filter(path %in% all_files) %>%
    mutate(path = paste0(dir, "/", path))

  message(paste0("Total number of studies to tag for RoB: ", length(citations_to_tag$id)))
  total_new_rob_tags <- 0
  while (total_new_rob_tags < length(citations_to_tag$id)){

    # Papers tagged before running function
    pre_rob_tag <- DBI::dbReadTable(con, "rob_tag")

    try(rob_tag(con, max_file_size = max_file_size))
    try(rob_tag(con, max_file_size = max_file_size))

    # Papers tagged before running function
    post_rob_tag <- DBI::dbReadTable(con, "rob_tag")

    # If no new studies are tagged then exit the loop
    running_rob_tag <- length(post_rob_tag$doi) - length(pre_rob_tag$doi)
    if (running_rob_tag == 0){

      break
    }

    total_new_rob_tags <- length(post_rob_tag$doi) - length(rob_tagged$doi)
    message(paste0("Total citations tagged for Risk of Bias: ", total_new_rob_tags))

  }

  message(paste0(total_new_rob_tag, " out of a possible ", length(citations_to_tag$doi), " tagged for Risk of Bias"))


  message(paste0("Total citations tagged for Open Access: ", total_new_oa_tags))
  message(paste0("Total citations tagged for Open Data: ", total_new_od_tags))
  message(paste0("Total citations tagged for Risk of Bias: ", total_new_rob_tags))

}

#' Create Regular Expressions
#'
#' This function reads a file containing names and their corresponding alternate names,
#' then creates regular expressions based on these names and alternate names.
#'
#' @param file Path to the input Excel file.
#'
#' @return A new Excel file with additional columns containing regular expressions.
#'
#' @import readxl
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' create_regex("input_file.xlsx")
#' }
#'
create_regex <- function(file = ""){

  # Read in file
  file_for_regex <- read.xlsx(file)

  # Splits the df in to 2, no_alternate_names and with_alternate_names
  no_alternate_names <- file_for_regex %>%
    filter(is.na(alternate_names) | alternate_names == "")

  # Function to create a regex from the "name" column
  name_to_regex <- function(string) {

    # Split the sentence into words
    words <- strsplit(string, "\\s")[[1]]

    # Initialize a vector to store transformed patterns
    transformed_patterns <- character(length = length(words))

    # Escape any punctuation characters in the words
    words <- gsub("([[:punct:]])", "\\\\\\1", words)

    # Iterate over each word
    for (i in seq_along(words)) {

      # Check if the word starts with [a-z] (case-insensitive)
      if (grepl("^[[:alpha:]]", words[i], ignore.case = TRUE)) {

        # Apply pattern transformation with word boundaries at both start and end
        pattern <- paste0("[", toupper(substr(words[i], 1, 1)), tolower(substr(words[i], 1, 1)), "]", substr(words[i], 2, nchar(words[i])))

      } else {

        # If the word doesn't start with [a-z], keep it as it is
        pattern <- words[i]

      }

      # Store the transformed pattern
      transformed_patterns[i] <- pattern

    }

    # Combine transformed patterns into a single string
    collapsed_pattern <- paste(transformed_patterns, collapse = "[\\s-]*")

    # Add word boundaries if the collapsed pattern starts with alphabetic character
    if (grepl("^\\[[[:alpha:]]", collapsed_pattern, ignore.case = TRUE)) {
      result <- paste0("\\b", collapsed_pattern, "\\b")
    } else {
      result <- collapsed_pattern
    }

    return(result)

  }

  # Apply the name_to_regex function to each name in 'no_alternate_names'
  no_alternate_names$regex <- sapply(no_alternate_names$name, name_to_regex)

  # Create a df that does have alternate names
  with_alternate_names <- file_for_regex %>%
    filter(!(is.na(alternate_names) | alternate_names == ""))

  # Apply the name_to_regex function to each name in 'with_alternate_names'
  with_alternate_names$name_regex <- sapply(with_alternate_names$name, name_to_regex)

  # Second function to convert alternate_names to regex and collapse on "|"
  convert_alternate_names <- function(names, separate_names_by = "\\|") {

    split_strings <- strsplit(names, separate_names_by)[[1]]
    remove_ws <- trimws(split_strings)
    words <- sapply(remove_ws, name_to_regex)
    join_words <- paste(words, collapse = "|")

    return(join_words)
  }

  # Apply convert_alternate_names function to the alternate names
  with_alternate_names$regex_alternate_names <- sapply(with_alternate_names$alternate_names, convert_alternate_names)

  # Concatenates the "name_regex" and "regex_alternate_names" in to 1, separated by "|"
  with_alternate_names$regex <- apply(with_alternate_names[, c("name_regex", "regex_alternate_names")], 1, function(x) paste(x, collapse = "|"))

  # Binds the 2 sections back together, no_alternate_names and with_alternate_names
  file_with_regex <- with_alternate_names %>%
    select(name, type, main_category, sub_category1, sub_category2, alternate_names, regex) %>%
    rbind(no_alternate_names)

  # Create a new file name
  new_file_name <- paste0("updated_regex_", file)
  write.xlsx(file_with_regex, new_file_name)

  message(paste0("File updated with regex and written to working directory"))
  message(paste0("File named: ", new_file_name))

}
