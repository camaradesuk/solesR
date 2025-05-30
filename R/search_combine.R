#' Combine SOLES-compatible search results into one dataframe
#'
#' @description
#' Combine results from manual and / or API searches in a SOLES-compatible format.
#' Each data frame should represent search results and can have different sets of columns. 
#' The function automatically aligns these data frames by column names, appending missing columns as `NA`.
#' SOLES columns in output are: "uid", "source", "author", "year", "journal", "doi", "title",
#' "pages", "volume", "abstract", "isbn", "keywords",
#' "secondarytitle", "url", "date", "issn", "pmid", "ptype", 
#' "source", "author_country", "number", "author_affiliation".
#' 
#' @param ... one or more data frame objects to be combined
#' @examples
#' \dontrun{
#' combined_result <- combine_searches(pubmed, wos, scopus)
#' }
#' @importFrom plyr rbind.fill
#' @import dplyr
#' @return dataframe of combined search results with SOLES-compatible column names
#' @export
#' 
combine_searches <- function(...){
  
  # Check inputs appear valid
  check_inputs <- function(...){
    # List inputs
    inputs <- list(...)
    # Check all inputs are dataframes
    if(!all(sapply(inputs, is.data.frame))){
      stop("Error: not all inputs are dataframes")
    }
    # Check that all inputs at least contain a uid column
    if(!all(sapply(inputs, function(df) "uid" %in% names(df)))){
      stop("Error: 'uid' column not found in all inputs. Are you sure your inputs are SOLES-compatible?")
    }
  }
  
  # Merge all searches in one dataframe
  combined <-  plyr::rbind.fill(...) 
  
  # Rename issue number
  combined$number <- combined$issue
  
  # Define relevant columns for SOLES
  x <- c("uid", "source", "author", "year", "journal", "doi", "title",
         "pages", "volume", "abstract", "isbn", "keywords",
         "secondarytitle", "url", "date", "issn", "pmid", "ptype", 
         "author_country", "number", "author_affiliation")
  
  # Append missing relevant columns to data frame
  combined[x[!(x %in% colnames(combined))]] = NA
  
  # Select relevant columns
  combined <- combined %>%
    dplyr::select(.data$uid, .data$source, .data$author, .data$year, 
                  .data$journal, .data$doi, .data$title, .data$pages, 
                  .data$volume, .data$abstract, .data$isbn, 
                  .data$keywords, .data$secondarytitle, .data$url, 
                  .data$date, .data$issn, .data$pmid, .data$ptype, 
                  .data$author_country, .data$number, .data$author_affiliation)
  
  # Return result
  return(combined)
  
}

#' Update retrieved citations table in SOLES AWS
#'
#' This function is part of the SOLES architexture and requires a valid SOLES AWS set up.
#' Check what records have been retrieved previously and add the newly retrieved citations to the `retrieved_citations` table in SOLES AWS.
#'
#' @param con connection to an AWS database
#' @param citations a dataframe of all new citations retrieved
#' @return dataframe of new citations, new citations appended to retrieved_citations AWS table 
#' @export
#' 
check_if_retrieved <- function(con, citations){
  
  # Check citations is a dataframe
  if(is.data.frame(citations) == FALSE){
    stop("Error: input for 'citations' is not a dataframe")
  }
  
  # Get table of retrieved uids
  retrieved <- tryCatch(
    {
      # Read from relevant table
      retrieved <- DBI::dbReadTable(con, "retrieved_citations")
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in DBI::dbReadTable", conditionMessage(e))
    }
  )
  
  # Remove already retrieved citations
  new_citations <- citations %>%
    filter(!.data$uid %in% retrieved$uid)
  
  # Select and rename columns for appending to existing table
  new_citations_write <- new_citations %>%
    select(.data$uid, .data$date) %>%
    rename(label = .data$date)
  
  # Append to existing table
  dbWriteTable(con, "retrieved_citations", new_citations_write, append=TRUE)
  
  # Print result
  message(nrow(new_citations_write), " new records found and appended to 'retrieved_citations' table")
  
  # Return new citations as a dataframe
  return(new_citations)
}
