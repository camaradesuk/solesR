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
#'   screening_decisions <- get_screening_decisions(
#'   con = your_database_connection, 
#'   review_id = "your_project_plus_date")
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
