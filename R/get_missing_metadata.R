#' Fill in missing abstracts via crossref
#'
#' This function pulls in missing abstracts
#'
#' @param citations citations you want to find abstracts for
#' @param abstract_col the name of the column that contains the abstract information. Default: `abstract`
#' @param doi_col the name of the column that contains the doi information. Default: `doi`
#' @param id_col a unique identifier column. For SyRF output this could be `CustomId`. Default: `uid`
#' @return citations dataframe with abstracts (where feasible)
#' @export
#' @import rcrossref
#' @import stringr

get_missing_abstracts <- function(citations, abstract_col = "abstract", doi_col = "doi", id_col = "uid"){
  
  # column name checks
  if(!abstract_col %in% colnames(citations)) {
    stop("Column ", abstract_col, " does not exist. Is the column name correct?")
  }
  
  if(!doi_col %in% colnames(citations)) {
    stop("Column ", doi_col, " does not exist. Is the column name correct?")
  }
  
  if(!id_col %in% colnames(citations)) {
    stop("Column ", id_col, " does not exist. Is the column name correct?")
  }
  
  # get studies with no abstract
  new_unique_no_abstract <- citations[which(stringr::str_length(citations[[abstract_col]]) < 100 |
                                              is.na(citations[[abstract_col]])),]
  
  message(nrow(new_unique_no_abstract), " records with no abstract")
  
  # get vector of DOIs
  dois <- new_unique_no_abstract[[doi_col]]
  
  # Print message
  message("Attempting to retrieve missing abstract information from CrossRef...")
  
  # get abstracts from crossref
  abstract_result <- lapply(dois, function(z) tryCatch(rcrossref::cr_abstract(z), error = function(e) e))
  
  # format abstract dataframe
  abstracts_df <- do.call(rbind, abstract_result)
  abstracts_df <- as.data.frame(abstracts_df)
  abstracts_df <- cbind(abstracts_df, dois)
  abstracts_df <- as.data.frame(abstracts_df)
  
  abstracts_df <- abstracts_df %>%
    mutate(abstract = ifelse(call == "NULL", NA, paste0(call))) %>%
    mutate(abstract = ifelse(call == "nchar(hh)", NA, paste0(abstract))) %>%
    mutate(doi = as.character(dois)) %>%
    dplyr::select(abstract, doi)
  
  abstracts_df <- abstracts_df %>% 
    rename({{ abstract_col }} := abstract) %>% 
    rename({{ doi_col }} := doi)
  
  # bind new abstracts to existing df
  new_unique_no_abstract <- new_unique_no_abstract %>%
    dplyr::select(-{{ abstract_col }})
  
  new_unique_no_abstract <- suppressWarnings(left_join(abstracts_df,
                                                       new_unique_no_abstract,
                                                       by = quo_name(doi_col))) 
  
  new_unique_no_abstract <- unique(new_unique_no_abstract)
  
  citations <- citations[which(!citations[[id_col]] %in% new_unique_no_abstract[[id_col]]),]
  
  citations <- rbind(citations, new_unique_no_abstract)
  
  # make all NA real NAs
  citations[citations == "NA" ] <- NA
  citations[citations == "" ] <- NA
  
  still_no_abstract <- citations[which(stringr::str_length(citations[[abstract_col]]) < 100 |
                                         is.na(citations[[abstract_col]])),]
  
  
  message(length(still_no_abstract[[id_col]]), " records still with no abstract")
  
  return(citations)
}

#' Fill in missing DOIs
#'
#' This function pulls in missing dois from OpenAlex
#'
#' @param citations citations you want to find dois for
#' @return citations with dois (where feasible)
#' @export
#' @import fuzzyjoin
#' @importFrom plyr rbind.fill
#' @import openalexR
#' @import dplyr
get_missing_dois <- function(citations){
  
  # Check input is dataframe
  if(!is.data.frame(citations)){
    stop("Input is not a data frame")
  }
  
  # Check necessary column names exist
  if(!all(c("doi", "uid", "title", "author", "pages", "journal") %in% colnames(citations))) {
    stop("Data frame does not contain all necessary columns: doi, uid, title, authors, pages, journal")
  }
  
  citations_no_doi <- citations  %>%
    # Remove no DOI
    filter(is.na(doi)|doi=="") %>%
    # Remove no title
    filter(!is.na(title)) %>%
    # Remove non-specific titles
    filter(!title %in% c("Preface", "Foreword"))
  
  # Print number missing DOI
  message(length(citations_no_doi$uid), " records with no doi")
  
  # Exit if no missing DOI
  if(length(citations_no_doi$uid) < 1) {
    message("No records are missing DOI")
    return(citations)
  }
  
  # Print message
  message("Attempting to retrieve missing abstract information from OpenAlex...")
  
  # Set result object to NULL
  res <- NULL
  
  # Loop over unique records
  for(i in 1:length(citations_no_doi$uid)){
    
    # Try to retrieve information using title search
    try(new <- openalexR::oa_fetch(
      identifier = NULL,
      entity = "works",
      title.search = citations_no_doi$title[i]),silent=TRUE)
    
    # Bind results together
    if(is.data.frame(new)){
      res <- plyr::rbind.fill(res, new)
    }
  }
  
  # Print message
  if(is.null(res)){
    message("No additional DOIs found")
    return(citations)
  }
  
  # If both title and display_name columns exist then keep display_name
  if(all(c("title","display_name") %in% colnames(res))) {
    res <- res %>% select(-title)
  }
  
  # Perform fuzzy matching
  match <- fuzzyjoin::stringdist_left_join(citations_no_doi, res, max_dist = 4, by = c("title" = "display_name"), ignore_case=TRUE)
  
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
