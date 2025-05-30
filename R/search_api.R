#' Search Scopus and retrieve bibliographic data using scopusAPI
#'
#' @description
#' A wrapper function for scopusAPI. Search Scopus using a query and retrieve results programmatically. Requires an API key from Scopus (http://dev.elsevier.com/).
#' Use the function `usethis::edit_r_environ()` to add the key to your ~/.Renviron file.
#'
#' @param query a character string containing a correctly syntaxed Scopus search 
#' @param api_key a working Scopus API key
#' @param retMax The maximum number of records to retrieve, default is 2000, maximum is 5000
#' @param format_soles boolean, if set to TRUE will format search results for the SOLES workflow, default is TRUE
#' @return a dataframe containing Scopus search results
#' @example 
#' \dontrun
#' # api_key should be stored in your r environment and not shared
#' query <- "TITLE-ABS-KEY(dementia OR \"memory loss\")"
#' scopus_result <- scopus_search(query, api_key, retMax = 500, format_soles = FALSE)
#' \dontrun
#' @import dplyr
#' @import scopusAPI
#' @export
#' 
scopus_search <- function(query = NULL, api_key = NULL, retMax=2000, format_soles = TRUE){
  
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop(message("Error: you have not entered a search query"))
  }
  
  # Check for API key and exit if NULL
  if (is.null(api_key)) {
    stop(message("Error: you have not entered an API key"))
  }
  
  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax%%1 != 0 | retMax < 0){
    stop(message("Error: retMax is not a whole number"))
  }
  
  #Check retMax and exit if above maximum
  if(retMax > 5000){
    stop(message("Error: retMax is too high"))
  }
  
  # Check format_soles is boolean and exit if not
  if(is.logical(format_soles) == FALSE){
    stop(message("Error: format_soles should be set to TRUE or FALSE, default is TRUE"))
  }
  
  # Print message
  message("Running Scopus search...")
  
  # Try running search query using scopusAPI R package
  scopus_results <- tryCatch(
    {
      # Try getting results
      result <- scopusAPI::search_scopus(string = query,
                                         api_key = api_key,
                                         retMax = retMax)
      # Return results if successful
      message("Retrieved ", nrow(result), "records from Scopus")
      return(result)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling scopusAPI::search_scopus()", conditionMessage(e))
    }
  )
  
  # Format for SOLES workflow if format_soles == TRUE
  if (format_SOLES == TRUE) {
    # Print message
    message("Formatting records for SOLES...")
    # Rename and create columns for SOLES
    scopus_results <- scopus_results %>%
      # Rename columns
      dplyr::rename(author = authors,
                    author_affiliation = affiliations,
                    author_country = countries,
                    title = articletitle) %>%
      dplyr::mutate(source = "scopus",
                    # Create unique identifier
                    uid = paste0(source, "-", scopusID),
                    # Format author column
                    author = gsub("\\|", ";", author),
                    # Format search date as character in format DDMMYY
                    date = format(Sys.Date(), "d%m%y%")) %>%
      # Remove rows with no ID
      dplyr::filter(!is.na(scopusID))
  }
  
  # Change all "NA" to real NA
  scopus_results[scopus_results == "NA" ] <- NA
  # Change all blanks to NA
  scopus_results[scopus_results == "" ] <- NA
  
  # Make DOI lowercase
  scopus_results$doi <- tolower(scopus_results$doi)
  
  # Return search results
  return(scopus_results)
}

#' Search Web of Science Core Collection and retrieve bibliographic data using rwoslite
#' 
#' @description
#' A wrapper function for rwoslite / WOS Lite. Search the Web of Science Core Collection using a query and retrieve results programmatically. 
#' Requires an API key from the Clarivate Developer Portal (https://developer.clarivate.com/apis/woslite).
#' Use the function `usethis::edit_r_environ()` to add the key to your ~/.Renviron file.
#' The timespan should be formatted as a number followed by the word "week" or "month", e.g. "1month" or "2week".
#' One week is calculated as 7 days and one month is calculated as 31 days.
#'	
#' @param query a character string containing a correctly syntaxed Web of Science search
#' @param timespan a formatted character string defining the timespan you want to search
#' @param format_soles boolean, if set to TRUE will format search results for the SOLES workflow, default is TRUE
#' @return a dataframe containing Web of Science search results
#' @example 
#' \dontrun
#' # api_key should be stored in your r environment and not shared
#' query <- "TS=(dementia OR \"memory loss\")"
#' wos_result <- wos_search(query, timespan = "1week", format_soles = FALSE)
#' \dontrun
#' @import dplyr
#' @import rwoslite
#' 
#' @export	
#' 

wos_search <- function(query = NULL, timespan = NULL, format_soles = TRUE){
  
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop(message("Error: you have not entered a search query"))
  }
  
  # Check for API key and exit if NULL
  if (is.null(timespan)) {
    stop(message("Error: you have not entered a timespan for the search"))
  }
  
  # Check format_soles is boolean and exit if not
  if(is.logical(format_soles) == FALSE){
    stop(message("Error: format_soles should be set to TRUE or FALSE, default is TRUE"))
  }
  
  if (grepl("^(?i)\\d+(week|month)$", timespan) == FALSE){
    stop(message("Error: timespan format incorrect"))
  }
  
  # Define timespan for search
  if (grepl("(?i)week", timespan) == TRUE){
    # Get number of weeks by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of weeks before today's date
    min_date_char <- Sys.Date() - 7 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  } else if (grepl("(?i)month", timespan) == TRUE){
    # Get number of months by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of months before today's date
    min_date_char <- Sys.Date() - 31 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  }
  
  # Append timespan to user query to define final query
  full_query <- paste0("(",query, ") AND LD=(", min_date_char, "/", max_date_char,")")
  
  # Set database to search as "WOS"
  database <- "WOS"
  
  # Print message
  message("Running Web of Science Core Collection search...")
  
  # Try seeing how many records are captured by query
  n_records <- tryCatch(
    {
      # Check number of records
      n_records <- rwoslite::wos_search(full_query, database)
      # Return results if successful
      return(n_records)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling scopusAPI::wos_search()", conditionMessage(e))
    }
  )
  
  # Exit function if number of records is 0
  if(n_records == 0){
    stop(message("Error: no search results were found from query: ", query))
  }
  
  # Try running search query using rwoslite R package
  wos_results <- tryCatch(
    {
      # Try getting results
      result <- rwoslite::wos_get_records(full_query)
      
      # Return results if successful
      message("Retrieved ", nrow(result), "records from Web of Science Core Collection")
      return(result)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling rwoslite::search_wos()", conditionMessage(e))
    }
  )
  
  # Format for SOLES workflow if format_soles == TRUE
  if (format_SOLES == TRUE){
    # Print message
    message("Formatting records for SOLES...")
    # Rename and create columns for SOLES
    wos_results <- wos_results %>%
      # Rename columns
      dplyr::rename(author = authors,
                    journal = source,
                    year = published_year) %>%
      dplyr::mutate(source = "scopus",
                    # Create unique identifier
                    uid = tolower(ut),
                    # Format author column
                    author = gsub('[[:space:]]\\|[[:space:]]', '; ', author),
                    # Format journal column
                    journal = tolower(journal),
                    journal = tools::toTitleCase(journal),
                    journal = as.character(journal),
                    # Add empty abstract column
                    abstract = NA,
                    # Format search date as character in format DDMMYY
                    date = format(Sys.Date(), "d%m%y%")) %>%
      # Remove rows with no ID
      dplyr::filter(!is.na(scopusID))
      
  }
  
  # Change all "NA" to real NA
  wos_results[wos_results == "NA" ] <- NA
  # Change all blanks to NA
  wos_results[wos_results == "" ] <- NA
  
  # Make DOI lowercase
  scopus_results$doi <- tolower(scopus_results$doi)
  
  # Return search results
  return(wos_results)	
}	


#' Search PubMed and retrieve bibliographic data using RISmed
#'
#' @description
#' A wrapper function for RISmed. Search PubMed using a query and retrieve results programmatically.
#' The timespan should be formatted as a number followed by the word "week" or "month", e.g. "1month" or "2week".
#' One week is calculated as 7 days and one month is calculated as 31 days.
#' Currently both SOLES_format - TRUE and FALSE give the same SOLES compatible format.
#'
#' @param query a character string containing a correctly syntaxed PubMed search
#' @param timespan a formatted character string defining the timespan you want to search
#' @param retMax The maximum number of records to retrieve, default is 5000, maximum is 5000
#' @param format_soles boolean, if set to TRUE will format search results for the SOLES workflow, default is TRUE
#' @return a dataframe containing Web of Science search results
#' @example 
#' \dontrun
#' # api_key should be stored in your r environment and not shared
#' query <- "(dementia[tiab] OR \"memory loss\"[tiab])"
#' pubmed_result <- pubmed_search(query, timespan = "1week", format_soles = FALSE)
#' \dontrun
#' @import dplyr
#' @import RISmed
#' 
#' @export
#' 
pubmed_search <- function(query, timespan, retMax=5000, format_soles = TRUE){
  
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop(message("Error: you have not entered a search query"))
  }
  
  # Check for API key and exit if NULL
  if (is.null(timespan)) {
    stop(message("Error: you have not entered a timespan for the search"))
  }
  
  # Check format_soles is boolean and exit if not
  if(is.logical(format_soles) == FALSE){
    stop(message("Error: format_soles should be set to TRUE or FALSE, default is TRUE"))
  }
  
  # Check if format_soles is FALSE, currently only SOLES format is supported
  if(format_soles == FALSE){
    stop(message("Error: currently only SOLES compatible formatting is available"))
  }
  
  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax%%1 != 0 | retMax < 0){
    stop(message("Error: retMax is not a whole number"))
  }
  
  #Check retMax and exit if above maximum
  if(retMax > 5000){
    stop(message("Error: retMax is too high"))
  }
  
  if (grepl("^(?i)\\d+(week|month)$", timespan) == FALSE){
    stop(message("Error: timespan format incorrect"))
  }
  
  # Define timespan for search
  if (grepl("(?i)week", timespan) == TRUE){
    # Get number of weeks by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of weeks before today's date
    min_date_char <- Sys.Date() - 7 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  } else if (grepl("(?i)month", timespan) == TRUE){
    # Get number of months by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of months before today's date
    min_date_char <- Sys.Date() - 31 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  }
  
  # Print message
  message("Running PubMed search...")
  
  # Try getting summary of NCBI EUtils query
  pubmed_search <- tryCatch(
    {
      # Try getting results
      search <- RISmed::EUtilsSummary(query, 
                                      retmax=retMax,
                                      mindate = paste0(format(min_date_char, "%Y/%m/%d")),
                                      maxdate=paste0(format(max_date_char, "%Y/%m/%d")),
                                      type="esearch", 
                                      db="pubmed")
      # Get summary
      summary <- RISmed::summary(search)
      
      # Return results if successful
      return(search)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling RISmed::EUtilsSummary()", conditionMessage(e))
    }
  )
  
  # Try running search query using RISmed R package
  records <- tryCatch(
    {
      # Try getting results
      result <- RISmed::EUtilsGet(pubmed_search)
      # Return results as Medline object if successful
      return(result)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling RISmed::EUtilsGet()", conditionMessage(e))
    }
  )
  
  # Create an empty vector to store author information
  authors <- vector()
  # Format author information
  for(n in 1:length(records@Author)){
    if(exists("tmp")){rm(tmp)}
    if(exists("tmp2")){rm(tmp2)}
    try(tmp<-paste0(records@Author[[n]]$LastName,", ",records@Author[[n]]$Initials), silent=TRUE)
    try(tmp2<-paste(tmp, collapse="; "), silent=TRUE)
    if(exists("tmp2")){
      if(length(tmp2)>0){
        authors<-append(authors,tmp2)
      } else{
        authors<-append(authors,NA)
      }
    } else{
      authors<-append(authors,NA)
    }
  }
  
  # Create an empty vector to store author country information
  country <- vector()
  # Format author country information
  for(n in 1:length(records@Country)){
    if(exists("tmp")){rm(tmp)}
    if(exists("tmp2")){rm(tmp2)}
    try(tmp<-paste0(records@Country[[n]]), silent=TRUE)
    try(tmp2<-paste(tmp, collapse="; "), silent=TRUE)
    if(exists("tmp2")){
      if(length(tmp2)>0){
        country<-append(country,tmp2)
      } else{
        country<-append(country,NA)
      }
    } else{
      country<-append(country,NA)
    }
  }
  
  # Create an empty vector for keyword information
  keywords <- vector()
  # Format keywords information
  for(n in 1:length(records@Keywords)){
    if(exists("tmp")){rm(tmp)}
    if(exists("tmp2")){rm(tmp2)}
    try(tmp<-paste0(records@Keywords[[n]]), silent=TRUE)
    try(tmp2<-paste(tmp, collapse="; "), silent=TRUE)
    if(exists("tmp2")){
      if(length(tmp2)>0){
        keywords<-append(keywords,tmp2)
      } else{
        keywords<-append(keywords,NA)
      }
    } else{
      keywords<-append(keywords,NA)
    }
  }
  
  # Format medline object into a dataframe
  pubmed_results <- data.frame("author" = authors,
                               "keywords" = keywords,
                               "abstract" = AbstractText(records),
                               "author_country" = country,
                               "title" = ArticleTitle(records),
                               "pages" = MedlinePgn(records),
                               "issue" = Issue(records), 
                               "volume" = Volume(records),
                               "year" = YearPubmed(records),
                               "pmid" = PMID(records),
                               "doi" = DOI(records),
                               "issn" = ISSN(records),
                               "journal" = MedlineTA(records))
  
  # Format dataframe for SOLES
  if (format_soles == TRUE){
    pubmed_results <- pubmed_results %>%
      # Change factors to characters
      dplyr::mutate_if(is.factor, as.character) %>%
      # reate URL column
      dplyr::mutate(url = paste0("https://www.ncbi.nlm.nih.gov/pubmed/",
                                 pmid),
                    source = "pubmed",
                    # reate unique identifier
                    uid = paste0(source, "-", pmid),
                    # Format search date as character in format DDMMYY
                    date = format(Sys.Date(), "d%m%y%")) %>%
      # Remove rows with no ID
      dplyr::filter(!is.na(pmid))
  }
  
  # Change all "NA" to real NA
  pubmed_results[pubmed_results == "NA" ] <- NA
  # Change all blanks to NA
  pubmed_results[pubmed_results == "" ] <- NA
  
  # Make DOI lowercase
  scopus_results$doi <- tolower(scopus_results$doi)
  
  # Print number of records retrieved
  message("Retrieved ", nrow(pubmed_results), "records from PubMed")
  
  # Return results
  return(pubmed_results)
}
