#' Retrieve retraction information from Crossref
#'
#' This function checks DOIs against the Crossref retraction database.
#'
#' @param con connection to db
#' @param retraction_db dataframe containing CrossRef retraction database; will retrieve if NULL
#'
#' @import DBI
#' @import dplyr
#' @import rcrossref
#' @return Updated "retraction_tag" table
#' @export
#' @examples
#' \dontrun{
#' # Example usage:
#' get_crossref_retractions(con)
#' }

get_crossref_retractions <- function(con, retraction_db = NULL){
  
  # Check con contains connection info
  if(!inherits(con, "PqConnection")){
    stop("'con' is not a valid database connection")
  }
  
  # If db input given, check valid
  if(!is.null(retraction_db)){
    # Check db input is dataframe
    if(!is.data.frame(retraction_db)){
      stop("'retraction_db' should be a dataframe")
    }
    # Check db input has doi columns
    if(!all(c("OriginalPaperDOI", "RetractionDOI") %in% colnames(retraction_db))) {
      stop("'retraction_db' does not contain necessary columns 'OriginalPaperDOI' and 'RetractionDOI'")
    }
  }
  
  # Read in retraction database
  if(is.null(retraction_db)){
    retraction_db <- read.csv("https://api.labs.crossref.org/data/retractionwatch?name@email.org")
  }
  
  # Make sure doi is lower case for matching
  retraction_db$OriginalPaperDOI <- tolower(retraction_db$OriginalPaperDOI)
  retraction_db$RetractionDOI <- tolower(retraction_db$RetractionDOI)
  
  # Get unique_citations data with doi
  citations <- tbl(con, "unique_citations") %>%
    select(uid, doi) %>%
    left_join(tbl(con, "study_classification")) %>%
    filter(decision == "include") %>%
    select(doi, uid) %>%
    filter(!is.na(doi)) %>%
    collect()
  
  # Check dois against retraction database
  citations_retracted <- citations %>%
    filter(doi %in% retraction_db$OriginalPaperDOI | doi %in% retraction_db$RetractionDOI) %>%
    mutate(is_retracted = TRUE,
           method = "Crossref",
           date = Sys.Date()) %>%
    select(doi, is_retracted, method, date)
  
  # Get not retracted data
  citations_not_retracted <- citations %>%
    filter(!doi %in% citations_retracted$doi) %>%
    mutate(is_retracted = FALSE,
           method = "Crossref",
           date = Sys.Date()) %>%
    select(doi, is_retracted, method, date)
  
  # Bind rows
  citations_updated <- rbind(citations_retracted, citations_not_retracted)
  
  # Create table if doesn't exist
  if (!dbExistsTable(con, "retraction_tag")) {
    
    retraction <- data.frame(doi = as.character(), 
                             is_retracted = as.logical(), 
                             method = as.character(),
                             date = as.Date(character()))
    
    dbWriteTable(con, "retraction_tag", retraction)
    message("Created retraction_tag table.")
    
  }
  
  # Overwrite table
  dbWriteTable(con, "retraction_tag", citations_updated, overwrite = T)
}

