#' Get Scopus results
#'
#' This function retrieves search results from Scopus
#'
#' @param query search query 
#' @param api_key scopus api key 
#' @param retMax maximum number of citations retrieved, specify based on timeframe 
#' @return dataframe of search results
#' @import dplyr
#' @export
#' 
scopus_search <- function(query, api_key, retMax=2000){
  
  scopus_results <- scopusAPI::search_scopus(string = query, api_key=api_key, retMax=retMax)
  
  date <- format(Sys.Date(), "%d%m%y")
  # add column with date of search
  scopus_results$date <- date
  
  # Remove single row of NA values if 0 results found
  scopus_results <- scopus_results %>% 
    filter(!uid == "scopus-NA" | !is.na(doi)) 
  
  return(scopus_results)
}

#' Get Web of Science results
#' This function retrieves search results from pubmed	#' This function retrieves search results from Web of Science, using the WOS Lite API. A personal API key 
#' must be requested from the Clarivate developer portal (https://developer.clarivate.com/apis/woslite) 
#' and stored as an R Environment variable.
#' 
#' Use the function `usethis::edit_r_environ()` to add the key to your ~/.Renviron file: 
#' 
#'	
#' @param query search query 
#' @param timespan time frame for search, options are: 1week, 2week, 1month	
#' @return dataframe of search results	
#' 
#' @import dplyr
#' @import rwoslite
#' 
#' @export	
#' 

wos_search <- function(query, timespan){
  
  # define timespan
  if(grepl("week", timespan) == TRUE){
    
    weeks <- as.numeric(gsub("\\D", "", timespan))
    min_date_char <- Sys.Date() - 7*weeks
    max_date_char <- Sys.Date()
    
    message(paste0("searching from ", min_date_char, " to ", max_date_char))
  }
  
  else if(grepl("month", timespan) == TRUE){
    
    months <- as.numeric(gsub("\\D", "", timespan))
    min_date_char <- Sys.Date() - 31*months
    max_date_char <- Sys.Date()
    
    message(paste0("searching from ", min_date_char, " to ", max_date_char))
  }
  
  # append timespan to user query to define final query
  full_query <- paste0(query, " AND LD=(", min_date_char, "/", max_date_char,")")
  
  database <- "WOS"
  
  # create empty df with same columns as output df
  
  wos_empty <- tibble(ut = character(), doc_type = character(), title = character(), authors = character(), 
                      book_authors = character(), book_group_authors = character(), keywords = character(), 
                      source = character(), volume = character(), issue = character(), pages = character(), 
                      no_article = character(), published_date = character(), published_year = character(),
                      supplement_number = character(), special_issue = character(), book_series_title = character(),
                      doi = character(), eissn = character(), issn = character(), isbn = character(), date = character()
  )
  
  # check number of records
  n_records <- rwoslite::wos_search(full_query, database)
  
  try({
    
    # Return empty df in case of an error
    if (inherits(n_records, "try-error")) {
      wos_results <- wos_empty
      message("Empty dataframe returned due to error")
      
    }
    
    # return empty df if no results are found
    else if (n_records == 0) {
      wos_results <- wos_empty
      message("Empty dataframe returned as 0 results found")
      
    } else {
      
      wos_results <- try(rwoslite::wos_get_records(full_query))
      
    }
  })
  # format columns to be compatible with soles
  wos_results <- wos_results %>% 
    mutate(date = format(Sys.Date(), "%d%m%y")) %>% 
    rename(uid = ut) %>% 
    rename(author = authors) %>% 
    rename(journal = source) %>%
    rename(year = published_year) %>% 
    mutate(source = "wos") %>% 
    mutate(author = gsub('[[:space:]]\\|[[:space:]]', '; ', author)) %>% 
    mutate(uid = tolower(uid)) %>% 
    mutate(abstract = NA) %>% 
    mutate(journal = tolower(journal)) %>% 
    mutate(journal = tools::toTitleCase(journal)) %>% 
    mutate(journal = as.character(journal))
  
  # show number of retrieved records
  print(paste0(length(wos_results$uid), " citations identified"))
  
  
  date <- format(Sys.Date(), "%d%m%y")
  
  if (length(wos_results) > 0){
    wos_results$date <- date	
  }
  
  return(wos_results)	
}	


#' Get pubmed results
#'
#' This function retrieves search results from pubmed
#'
#' @param query search query 
#' @param timespan time frame for search options e.g. 1week, 2week, 1month
#' @param retMax maximum number of citations to retrieve, maximum is 9999
#' @return pubmed search results
#' @export
#' 
pubmed_search <- function(query, timespan, retMax=5000){
  if(grepl("week", timespan) == TRUE){
    
    weeks <- as.numeric(gsub("\\D", "", timespan))
    
    min_date_char <- Sys.Date() - 7*weeks
    max_date_char <- Sys.Date()
    
    message(paste0("searching from ", min_date_char, " to ", max_date_char))
  }
  
  else if(grepl("month", timespan) == TRUE){
    
    months <- as.numeric(gsub("\\D", "", timespan))
    
    min_date_char <- Sys.Date() - 31*months
    max_date_char <- Sys.Date()
    
    message(paste0("searching from ", min_date_char, " to ", max_date_char))
    
    
  }
  
  pubmed_search <- RISmed::EUtilsSummary(query, retmax=retMax,
                                         mindate = paste0(format(min_date_char, "%Y/%m/%d")),
                                         maxdate=paste0(format(max_date_char, "%Y/%m/%d")),
                                         type="esearch", db="pubmed")
  
  pubmed_summary <- RISmed::summary(pubmed_search)
  
  pubmed_results <- try(
    RISmed::EUtilsGet(pubmed_search), 
    silent = TRUE
  )
  
  # create empty df with pubmed output columns
  pubmed_empty <- tibble(author = character(), keywords = character(), abstract = character(),
                         author_country = character(), title = character(), pages = character(),
                         issue = character(), volume = character(), year = numeric(), pmid = character(),
                         doi = character(), issn = character(), journal = character(), url = character(),
                         uid = character(), source = character(), date = character())
  
  try({
    if (pubmed_search@count < 1) {
      # return empty df if no results are found
      pubmed_results <- pubmed_empty
    } else if (inherits(pubmed_results, "Medline")) {
      # return warning if result is a Medline object
      warning("A Medline object is returned instead of a dataframe. For output compatible with the SOLES workflow, update the RISmed package from: https://github.com/kaitlynhair/RISmed")
    }
  })
  
  # so the function doesn't break when output is a Medline object
  try({
    date <- format(max_date_char, "%d%m%y")
    pubmed_results$date <- date
    
    print(paste0(length(pubmed_results$uid), " pubmed citations identified"))
  })
  
  return(pubmed_results)
}
