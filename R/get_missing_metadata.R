#' Fill in missing abstracts via CrossRef
#'
#' Search records in CrossRef using DOI to attempt to retrieve missing abstract text.
#'
#' @param citations a dataframe containing records missing abstract text data
#' @param abstract_col the name of the column that should contain the abstract text. Default: `abstract`
#' @param doi_col the name of the column that contains the DOI. Default: `doi`
#' @param id_col a unique identifier column. For SyRF output this could be `CustomId`. Default: `uid`
#' 
#' @return The same dataframe entered as citations with additional abstracts entered where these could be retrieved by CrossRef
#' 
#' @import rcrossref
#' @import stringr
#' @import rlang
#' @import dplyr
#' 
#' @export

get_missing_abstracts <- function(citations, abstract_col = "abstract", doi_col = "doi", id_col = "uid"){
  
  # Check the required column names exist and stop the function with error if not
  if(!abstract_col %in% colnames(citations)) {
    stop("Column ", abstract_col, " does not exist. Is the column name correct?")
  }
  
  if(!doi_col %in% colnames(citations)) {
    stop("Column ", doi_col, " does not exist. Is the column name correct?")
  }
  
  if(!id_col %in% colnames(citations)) {
    stop("Column ", id_col, " does not exist. Is the column name correct?")
  }
  
  # Make all NA real NAs
  citations[citations == "NA" ] <- NA
  citations[citations == "" ] <- NA
  
  # Check each record has an ID (no NA values)
  if(sum(is.na(citations[[id_col]])) > 0){
    stop(message("Column ", id_col, " does not contain a unique identifier for each record. Please ensure each record has a unique identifier."))
  }
  
  # Rename columns for internal function use
  citations <- citations %>%
    rename(abstract := {{ abstract_col }},
           doi := {{ doi_col }},
           uid := {{ id_col }})
  
  # Subset records that do not have an abstract
  new_unique_no_abstract <- citations %>%
    filter(stringr::str_length(citations$abstract) < 100 | is.na(citations$abstract))
  
  # Print number of records missing an abstract to the console
  message(nrow(new_unique_no_abstract), " records with no abstract.")
  
  # Check records missing an abstract have a DOI
  if (nrow(new_unique_no_abstract) > 1) {
    new_unique_no_abstract <- new_unique_no_abstract %>%
      # Remove records without a DOI
      filter(!is.na(doi)) %>%
      # Remove empty abstract column
      select(-abstract)
    
    # Print number of records with a DOI
    message(nrow(new_unique_no_abstract), " records with no abstract have a DOI.")
  }
  
  # # Stop function if there are no missing abstracts
  if (nrow(new_unique_no_abstract) < 1) {
    stop(message("No more abstracts to find!"))
  }
  
  # Print process to console
  message("Trying to retrieve abstracts from CrossRef using DOI...")
  
  # Run function over each row of data missing abstracts
  abstract_result <- lapply(seq_len(nrow(new_unique_no_abstract)), function(i) {
    # Get doi and id
    doi = new_unique_no_abstract$doi[i]
    uid = new_unique_no_abstract$uid[i]
    # Attempt to retrieve data from CrossRef
    tryCatch(
      {
        result <- rcrossref:: cr_abstract(doi)
        message(sprintf("Found abstract for DOI '%s'", doi))
        list(
          doi = doi,
          uid = uid,
          abstract = result,
          error = NA
        )
      },
      error = function(e) {
        message(sprintf("Error with DOI '%s': %s", doi, e$message))
        list(
          doi = doi,
          uid = uid,
          abstract = NA,
          error = e$message
        )
      }
    )
  })
  
  # Unlist result (call and dois)
  abstracts_df <- as.data.frame(do.call(rbind, abstract_result)) %>%
    # Remove error column
    select(-error) %>%
    # Remove results where abstract was not retrieved
    filter(!is.na(abstract)) %>%
    # Make all data character type
    mutate(across(everything(), as.character)) %>%
    # Remove additional white space from abstract
    mutate(abstract = str_squish(abstract)) %>%
    # Make DOI lower case
    mutate(doi = tolower(doi)) %>%
    left_join(new_unique_no_abstract, by = c("doi", "uid"))
  
  # Bind new data with existing
  if (nrow(abstracts_df) > 0) {
    citations_updated <- citations %>%
      filter(!uid %in% abstracts_df$uid) %>%
      rbind(abstracts_df)
  } else {
    citations_updated <- citations
  }
  
  # Rename columns for output
  citations_updated <- citations_updated %>%
    rename({{ abstract_col }} := abstract,
           {{ doi_col }} := doi,
           {{ id_col }} := uid)
  
  # Make sure all NA real NAs
  citations_updated[citations_updated == "NA" ] <- NA
  citations_updated[citations_updated == "" ] <- NA
  
  # Subset records still missing an abstract
  still_no_abstract <- citations_updated[which(stringr::str_length(citations_updated[[abstract_col]]) < 100 |
                                                 is.na(citations_updated[[abstract_col]])),]
  
  # Print number missing an abstract to the console
  message(length(still_no_abstract[[id_col]]), " records still with no abstract")
  
  
  # get studies with no abstract
  new_unique_no_abstract <- citations[which(stringr::str_length(citations[[abstract_col]]) < 100 |
                                              is.na(citations[[abstract_col]])),]
  
  message(nrow(new_unique_no_abstract), " records with no abstract")
  
  # Return dataset with added abstract text
  invisible(return(citations_updated))
  
}

#' Fill in missing DOIs
#'
#' Uses title matching to retrieve DOI information from OpenAlex
#'
#' @param citations citations you want to find dois for
#' @return Dataframes with additional DOIs found by OpenAlex
#' @export
#' @import fuzzyjoin
#' @importFrom plyr rbind.fill
#' @import openalexR
#' @import dplyr
#' @import stringr
get_missing_dois <- function(citations){
  
  # Check input is dataframe
  if(!is.data.frame(citations)){
    stop("Input is not a data frame")
  }
  
  # Check necessary column names exist
  if(!all(c("doi", "uid", "title", "author", "pages", "journal") %in% colnames(citations))) {
    stop("Data frame does not contain all necessary columns: doi, uid, title, authors, pages, journal")
  }
  
  # Subset rows where DOI is missing and title is non-specific or short
  citations_no_doi <- citations  %>%
    # Remove non-specific titles
    filter(!title %in% c("Preface", "Foreword")) %>%
    # Remove short titles
    filter(stringr::str_length(citations$title) >= 25) %>%
    # Remove no DOI
    filter(is.na(doi)|doi=="") %>%
    # Remove no title
    filter(!is.na(title)) 
  
  # Print number missing DOI
  message(length(citations_no_doi$uid), " records with no doi")
  
  # Exit if no missing DOI
  if(length(citations_no_doi$uid) < 1) {
    message("No records are missing DOI")
    return(citations)
  }
  
  # Print message
  message("Attempting to retrieve missing DOI information from OpenAlex...")
  
  # Set result object to NULL
  results <- NULL
  
  # Loop over unique records
  for(i in 1:length(citations_no_doi$uid)){
    
    # Try to retrieve information using title search
    try(new <- openalexR::oa_fetch(
      identifier = NULL,
      entity = "works",
      title.search = citations_no_doi$title[i]),silent=TRUE)
    
    # Add pause between requests for API limits
    Sys.sleep(time = 1)
    
    # Bind results together
    if(is.data.frame(new)){
      # Print success message to console
      message(sprintf("Success: Potential DOI fetched for title: '%s'", citations_no_doi$title[i]))
      # If records retrieved, bind result to object
      results <- plyr::rbind.fill(results, new)
    } else {
      # Print failure message to console
      message(sprintf("Error: No data fetched for title: '%s'", citations_no_doi$title[i]))
    }
  }
  
  # Print message
  if(is.null(results)){
    message("No additional DOIs found")
    return(citations)
  }
  
  # If both title and display_name columns exist then keep display_name
  if(all(c("title","display_name") %in% colnames(results))) {
    results <- results %>% select(-title)
  }
  
  # Perform fuzzy matching
  match <- fuzzyjoin::stringdist_left_join(citations_no_doi, results, max_dist = 4, by = c("title" = "display_name"), ignore_case=TRUE)
  
  # Try to match based on other metadata
  try(correct_doi <- match %>%
        # Select relevant
        select(title, author_orig = author, authorships, journal_orig = journal, journal_new = source_display_name, year_orig = year, year_new = publication_year, uid, doi_new = doi.y) %>%
        # Get author info
        tidyr::unnest(cols=authorships) %>%
        filter(author_position == "first") %>%
        rename(author_new = display_name) %>%
        # Select relevant
        select(title, author_orig, author_new, journal_orig, journal_new, year_orig, year_new, uid, doi_new) %>%
        unique()%>%
        # Get with DOI
        filter(!is.na(doi_new)) %>%
        # Check for year match
        mutate(year_match = ifelse(year_orig == year_new, "yes", "no")) %>%
        # Check for author match
        mutate(author_new = as.character(author_new)) %>%
        mutate(author_orig =  substr(author_orig,1,18)) %>%
        mutate(auth_match = stringdist::stringsim(author_orig, author_new, method="qgram")) %>%
        mutate(auth_match = ifelse(auth_match > 0.5, "yes", "no")) %>%
        # Check for journal match
        mutate(jour_match = stringdist::stringsim(journal_orig, journal_new, method="qgram")) %>%
        mutate(jour_match = ifelse(jour_match > 0.5, "yes", "no")) %>%
        # Check for two out of three match
        mutate(final_match = ifelse(year_match== "yes" & auth_match == "yes", "match",  "check")) %>%
        mutate(final_match = ifelse(year_match== "yes" & jour_match == "yes", "match", paste(final_match))) %>%
        mutate(final_match = ifelse(auth_match == "no" & jour_match == "no" & year_match == "no", "not_match", paste(final_match))) %>%
        filter(final_match == "match") %>%
        select(uid, doi_new) %>%
        rename(doi = doi_new) %>%
        # Remove any additional DOIs (e.g, elife versioning)
        mutate(doi = gsub("; .+$", "", doi)) %>%
        group_by(uid) %>%
        slice_head()
      , silent=TRUE)
  
  # Pint message
  if(!exists("correct_doi")){
    message("No DOIs found")
    return(citations)
  }
  
  # Format citations now with doi
  citations_with_doi <- citations_no_doi %>%
    select(-doi) %>%
    filter(uid %in% correct_doi$uid)
  citations_with_doi <- merge(citations_with_doi, correct_doi, by = "uid")
  
  # Remove from main df
  citations <- citations %>%
    filter(!uid %in% citations_with_doi$uid)
  
  message(length(citations_with_doi$uid), " missing DOIs found")
  
  # re-add to main df
  citations <- rbind(citations_with_doi, citations)
  
  # Return citations
  return(citations)
}

#' Retrieve Missing DOIs for Included Citations
#'
#' This function identifies included citations in the database with no DOI information, 
#' then retrieves DOIs using the OpenAlex API. Results are processed in batches with rate limiting and saved to disk.
#'
#' @param con A database connection object (e.g., from DBI::dbConnect).
#' @param batch_size Integer. Number of citations to process per batch. Default is 100.
#'   Batches are processed with a 2-minute delay between them to respect API rate limits.
#' @param study_type String. Type of study, based on the name column in study_classification table (i.e. "in-vivo", "clinical" etc)
#'
#' @return A data frame containing citations with successfully retrieved DOIs, or NULL
#'   if no DOIs are missing or none are found
#'
#' The function expects the database to contain:
#' \itemize{
#'   \item \code{study_classification} table with \code{uid} and \code{decision} columns
#'   \item \code{unique_citations} table with \code{uid}, \code{doi}, and \code{title} columns
#' }
#'
#' @section Output Files:
#' Results are saved to \code{doi_retrieval/doi_found_YYYY-MM-DD.fst} where the date
#' reflects when the function was run.
#'
#' @section API Usage:
#' This function uses the OpenAlex API via \code{solesR::get_missing_dois()}. The
#' 2-minute delay between batches helps ensure compliance with API rate limits.
#'
#' @examples
#' \dontrun{
#' # Use smaller batches for more conservative API usage
#' doi_results <- get_missing_dois_complete(con, batch_size = 50)
#' }
#' @seealso
#' \code{\link[solesR]{get_missing_dois}}
#'
#' @export
get_missing_dois_complete <- function(con, batch_size = 100, study_type = NULL) {
  
  # Create output directory
  fst_dir <- "doi_retrieval"
  if (!dir.exists(fst_dir)) {
    dir.create(fst_dir, recursive = TRUE)
  }
  
  # Dated file name
  date <- Sys.Date()
  dated_fst_file <- file.path(fst_dir, paste0("doi_found_", date, ".fst"))
  
  if (is.null(study_type)){
    
    # Find citations missing DOIs
    citations_no_doi <- tbl(con, "study_classification") %>%
      filter(decision == "include") %>%
      select(uid) %>%
      left_join(tbl(con, "unique_citations"), by = "uid") %>%
      collect() %>%
      filter(is.na(doi) | doi == "", !title %in% c("Preface", "Foreword"))
  } else{
    
    citations_no_doi <- tbl(con, "study_classification") %>%
      filter(decision == "include") %>%
      filter(name == study_type) %>% 
      select(uid) %>%
      left_join(tbl(con, "unique_citations"), by = "uid") %>%
      collect() %>%
      filter(is.na(doi) | doi == "", !title %in% c("Preface", "Foreword"))
    
  }
  
  # Check there are DOI missing
  if (nrow(citations_no_doi) == 0) {
    message("No DOIs missing")
    return()
  } else {
    message(paste0("Total number of DOIs to search for: ", nrow(citations_no_doi)))
  }
  
  # Split into batches
  batches <- split(citations_no_doi, ceiling(seq_len(nrow(citations_no_doi)) / batch_size))
  
  # Process each batch and store results in a list
  results_list <- lapply(seq_along(batches), function(i) {
    batch <- batches[[i]]
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- start_idx + nrow(batch) - 1
    
    # Fetch DOIs for this batch
    message(sprintf("DOIs found for citations %d-%d", start_idx, end_idx))
    result <- get_missing_dois(batch)
    
    # Sleep between batches (but not after the last one)
    if (i < length(batches)) {
      message("Waiting 2 minutes before next batch...")
      Sys.sleep(120)
    }
    
    # Return only if result is not NULL/empty
    if (!is.null(result) && nrow(result) > 0) {
      return(result)
    } else {
      return(NULL)
    }
  })
  
  if (length(results_list) == 0) {
    message("No new DOIs found in this run.")
    return()
  }
  
  # Bind all results
  all_citations_with_doi <- do.call(rbind, results_list)
  
  # Make sure only 1 DOI returned for each UID
  all_doi_found <- all_citations_with_doi 
  
  # Use soles function to format the doi
  all_doi_found <- solesR::format_doi(all_doi_found) %>% 
    filter(!is.na(doi) & doi != "") %>% 
    select(uid, doi) %>% 
    distinct()
  
  # Save and return only DOIs found
  write.fst(all_doi_found, dated_fst_file)
  message(paste0("Total DOI searched for: ", nrow(citations_no_doi)))
  message(paste0("Total DOI found: ", nrow(all_doi_found)))
  message("All DOIs saved to: ", dated_fst_file)
  
  return(all_doi_found)
}

#' Process and Update Found DOIs in the Database
#'
#' Imports newly identified DOIs from a `.fst` file, from the get_missing_dois_complete() function,
#' and updates matching records in the `unique_citations` table based on `uid`.
#'
#' The function checks alignment between supplied DOIs and database records
#' missing a DOI. If counts do not match, the user is prompted to either proceed
#' with automated updating or exit for manual inspection.
#'
#' When confirmed, existing DOI values for matching UIDs are replaced, all other
#' records are left unchanged, and the full `unique_citations` table is written
#' back to the database.
#'
#' @param con A database connection object (e.g., from \code{DBI::dbConnect})
#'   pointing to a database containing the \code{unique_citations} table.
#' @param dois_found_file Path to a `.fst` file containing at least the columns
#'   \code{uid} and \code{doi}.
#'
#' @return Invisibly returns \code{NULL}. Updates the \code{unique_citations} table.
#'
#' @examples
#' \dontrun{
#' process_found_doi(con, "doi_retrieval/doi_found_date.fst")
#' }
#'
#' @importFrom fst read.fst
#' @importFrom dplyr select distinct filter left_join
#' @importFrom DBI dbWriteTable
#'
#' @export
process_found_doi <- function(con, dois_found_file = NULL) {
  
  # Check for valid file path
  if (missing(dois_found_file) || !file.exists(dois_found_file)) {
    stop("Please provide a valid path to a doi_found file (.fst).")
  }
  
  
  # Bring in DOI found file
  dois_found <- fst::read.fst(dois_found_file) 
  
  # If UID == NA then exit function
  if (any(is.na(dois_found$uid))) {
    stop("UID column contains NA values — cannot safely process DOIs.")
  }
  
  message(paste("Processing", nrow(dois_found), "DOIs from the doi_found file..."))
  
  # Search unique_citations for these studies
  studies_found <- tbl(con, "unique_citations") %>% 
    filter(uid %in% dois_found$uid) %>% 
    collect()
  
  # Check they are still missing a DOI
  studies_found_no_doi <- studies_found %>% 
    filter(is.na(doi) | doi == "") 
  
  message(paste("Number of linked studies in the database:", nrow(studies_found)))
  message(paste("Number of linked studies in the database missing a DOI:", nrow(studies_found_no_doi)))
  
  
  
  # If they do not match up exactly then tell the user
  if (nrow(dois_found) != nrow(studies_found_no_doi)){
    
    answer <- menu(
      c("Yes", "No, check manually"),
      title = paste0("Number of new DOIs for processing differs to the number of linked studies in the database. Meaning possible duplicate DOIs or UIDs.",
                     "\n",
                     "Would you like to continue automated processing?")
    )
    
    if (answer == 2){
      message("Manual check required — exiting function.")
      return(invisible(NULL))
    }
  } else {
    
    # If they do match up, then confirm with the user to update the database table
    answer <- menu(
      c("Yes", "No"),
      title = paste0("Number of new DOIs for processing matches the number of linked studies in the database.",
                     "\n",
                     "Would you like to update the unique_citations table?")
    )
    
    if (answer == 2){
      message("Database not updated - exiting function.")
      return(invisible(NULL))
    }
    
  }
  
  message(paste("Updating unique_citations tables with", nrow(studies_found), "new DOIs found..."))
  
  # Remove the studies which are NOT to be updated
  remove_found <- tbl(con, "unique_citations") %>% 
    filter(!uid %in% dois_found$uid) %>% 
    collect()
  
  # Update the found studies
  updated_studies <- studies_found %>% 
    select(-doi) %>% 
    left_join(dois_found, by = "uid")
  
  # Put them back together
  unique_citations <- rbind(remove_found, updated_studies)
  
  # Write to database
  dbWriteTable(con, "unique_citations", unique_citations, overwrite = T)
  
  message("unique_citations table updated successfully.")
  
}
