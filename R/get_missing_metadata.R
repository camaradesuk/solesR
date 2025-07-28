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
    # Remove no DOI
    filter(is.na(doi)|doi=="") %>%
    # Remove no title
    filter(!is.na(title)) %>%
    # Remove non-specific titles
    filter(!title %in% c("Preface", "Foreword")) %>%
    # Remove short titles
    filter(stringr::str_length(citations$title) >= 25) %>%
  
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
        tidyr::unnest(cols=author.y) %>%
        filter(author_position == "first") %>%
        rename(author_orig = author.x,
               new_doi=doi.y) %>%
        select(title, au_display_name, author_orig, pages, first_page, last_page, journal, so, uid, new_doi) %>%
        unique()%>%
        filter(!is.na(new_doi)) %>%
        tidyr::unite(pages_new, first_page, last_page, sep = "-", na.rm=TRUE) %>%
        mutate(page_match = ifelse(pages == pages_new, "yes", "no")) %>%
        mutate(au_display_name = as.character(au_display_name)) %>%
        mutate(author_orig =  substr(author_orig,1,18)) %>%
        mutate(auth_match = stringdist::stringsim(author_orig, au_display_name, method="qgram")) %>%
        mutate(auth_match = ifelse(auth_match > 0.5, "yes", "no")) %>%
        mutate(jour_match = stringdist::stringsim(journal, so, method="qgram")) %>%
        mutate(jour_match = ifelse(jour_match > 0.5, "yes", "no")) %>%
        mutate(final_match = ifelse(page_match== "yes" & auth_match == "yes", "match",  "check")) %>%
        mutate(final_match = ifelse(page_match== "yes" & jour_match == "yes", "match", paste(final_match))) %>%
        mutate(final_match = ifelse(auth_match == "yes" & jour_match == "yes", "match",  paste(final_match))) %>%
        mutate(final_match = ifelse(auth_match == "no" & jour_match == "no" & page_match == "no", "not_match", paste(final_match))) %>%
        filter(final_match == "match") %>%
        select(uid, new_doi) %>%
        rename(doi = new_doi), silent=TRUE)
  
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
