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

#' Combine searches
#'
#' This function combines search results across searches
#' @param ... One or more data frame objects to be combined. Each data frame should represent search results
#'        and can have different sets of columns. The function automatically aligns these data frames by column names,
#'        appending missing columns as `NA`.
#'        
#' @importFrom plyr rbind.fill
#' @import dplyr
#' @return combined search results with all the correct columns
#' @export
combine_searches <- function(...){
  
  # merge all searches in one dataframe
  combined <-  plyr::rbind.fill(...) 
  combined$number <- combined$issue
  # define relevant columns for soles
  x <- c("uid", "source", "author", "year", "journal", "doi", "title",
         "pages", "volume", "abstract", "isbn", "keywords",
         "secondarytitle", "url", "date", "issn", "pmid", "ptype", 
         "source", "author_country", "number", "author_affiliation")
  # append missing relevant columns to data frame
  combined[x[!(x %in% colnames(combined))]] = NA
  # select relevant columns
  combined <- combined %>%
    dplyr::select(uid, source, author, year, journal, doi, title,
                  pages, volume, abstract, isbn, keywords,
                  secondarytitle, url, date, issn, pmid, ptype, 
                  source, author_country, number, author_affiliation) 
  
}

#' Update retrieved citations table
#'
#' This function updates the retrieved citations table with newly retrieved studies
#'
#' @param con connection to db
#' @param citations df of all new citations retrieved
#' @return update of the retrieved_citations table 
#' @export
#' 
check_if_retrieved <- function(con, citations){
  
  # get table of retrieved uids
  retrieved <- DBI::dbReadTable(con, "retrieved_citations")
  
  # remove already retrieved citations
  new_citations <- citations %>%
    filter(!uid %in% retrieved$uid)
  
  new_citations_write <- new_citations %>%
    select(uid, date) %>%
    rename(label = date)
  
  print(paste0(length(new_citations_write$uid), " new citations identified and written to retrieved_citations table"))
  dbWriteTable(con, "retrieved_citations", new_citations_write, append=TRUE)
  
  return(new_citations)
}

#' Load Citations from Manual Source (WoS, PubMed, Embase)
#'
#' This function loads citations from a specified source (WoS, PubMed, or Embase), 
#' processes the data, and returns a dataframe of the citations with standardized 
#' formatting.
#'
#' @import RefManageR
#' @import dplyr
#' @import parallel
#' @import bibliometrix
#' @importFrom utils read.csv read.table
#' @param paths A character vector of file paths to the citations files.
#' @param source The source database for the citations (choices: "wos", "pubmed", "embase").
#'
#' @return A list of dataframes, each containing the citations from the specified source.
#'
#' @export
manual_upload <- function(paths, source){
  
    if(source == "xml"){
    
    # Run in parallel for each path
    processed_data <- parallel::mclapply(paths, process_xml, mc.cores = parallelly::availableCores())
    
    # Combine the results into one list
    combined_data <- do.call(rbind, processed_data)
    
    return(combined_data)
  }
    
    else if(source == "wos"){
    
    # Run in parallel for each path
    processed_data <- parallel::mclapply(paths, process_wos, mc.cores = parallelly::availableCores())
    
    # Combine the results into one list
    combined_data <- do.call(rbind, processed_data)
    
    return(combined_data)
    
  } else if(source == "pubmed"){
    
    # Run in parallel for each path
    processed_data <- parallel::mclapply(paths, process_pubmed, mc.cores = parallelly::availableCores())
    
    # Combine the results into one list
    combined_data <- do.call(rbind, processed_data)
    
    return(combined_data)
    
  }  else if(source == "embase"){
    
    # Run in parallel for each path
    processed_data <- parallel::mclapply(paths, process_embase, mc.cores = parallelly::availableCores())
    
    # Combine the results into one list
    combined_data <- do.call(rbind, processed_data)
    combined_data$uid <- tolower(combined_data$uid)
    return(combined_data)
    
  }  else if(source == "psychinfo"){
    
    # Run in parallel for each path
    processed_data <- parallel::mclapply(paths, process_psychinfo, mc.cores = parallelly::availableCores())
    
    # Combine the results into one list
    combined_data <- do.call(rbind, processed_data)
    combined_data$uid <- tolower(combined_data$uid)
    return(combined_data)
    
  }  else if(source == "medline"){
    
    # Run in parallel for each path
    processed_data <- parallel::mclapply(paths, process_medline, mc.cores = parallelly::availableCores())
    
    # Combine the results into one list
    combined_data <- do.call(rbind, processed_data)
    
    return(combined_data)
    
  } else if(source == "scopus"){
    
    # Run in parallel for each path
    processed_data <- parallel::mclapply(paths, process_scopus, mc.cores = parallelly::availableCores())
    
    # Combine the results into one list
    combined_data <- do.call(rbind, processed_data)
    
    return(combined_data)
    
  }  else {
    
    message("input type not currently supported")
    return()
  }}



#' Format Columns for SOLES Search Data
#'
#' This function processes a dataframe to retain relevant columns required for SOLES and standardizes the case of 
#' specific columns to ensure consistent formatting.
#'
#' @import dplyr
#'
#' @param df A dataframe to be formatted for SOLES search data.
#'
#' @return A formatted dataframe containing the required SOLES columns with standardized case.
#' 
format_cols <- function(df){
  
  # cols required for soles
  x <- c("uid", "author", "year", "journal", "doi", "title",
         "pages", "volume", "abstract", "isbn", "keywords",
         "secondarytitle", "url", "date", "issn", "pmid", "ptype", 
         "source", "author_country", "number", "author_affiliation")
  
  title_case_cols <- c("author","journal", "secondarytitle", "author_country", "author_affiliation")
  
  sentence_case_cols <- c("title",
                          "abstract")
  
  lower_case_cols <- c("uid", "doi",  "keywords", "ptype", 
                       "source")
  
  df[x[!(x %in% colnames(df))]] = NA
  df <- df %>%
    select(all_of(x)) %>%
    mutate(across(all_of(sentence_case_cols), ~stringr::str_to_sentence(.))) %>%
    mutate(across(all_of(lower_case_cols), ~stringr::str_to_lower(.))) %>%
    mutate(across(all_of(title_case_cols), ~stringr::str_to_title(.))) %>%
    mutate_at(vars(x), ~ gsub(";", "; ", .))
  
  df$pages <- lapply(df$pages, function(x) gsub("--", "-", x))
  df$date  <-  format(Sys.Date(), "%d%m%y")
  
  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'issn')
  df[cols_to_modify] <- lapply(df[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))
  return(df)
}

#' Process Embase references
#'
#' This function processes references from Embase.
#'
#' @param path The file path of the data.
#'
#' @return A processed data frame.
#'
#' @details
#' This function reads references from the specified file using the "ovid" tag naming convention. 
#' It performs various transformations on the data, such as uniting title and booktitle columns,
#' formatting pages, creating a unique identifier (uid) based on the article number (AN), and 
#' setting the data source to "embase". 
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' embase_data <- process_embase("path/to/embase_data.csv")
#' }
#'
#' @importFrom synthesisr read_refs
#' @importFrom tidyr unite
#' @import dplyr
process_embase <- function(path){
  
  newdat <- synthesisr::read_refs(path, tag_naming = "ovid")
  
  newdat$number <- newdat$issue
  newdat$author_affiliation <- newdat$address
  
  if("booktitle" %in% colnames(newdat)) {
    newdat <-newdat %>%
      tidyr::unite(title, booktitle, na.rm = TRUE)
  }
  
  if("start_page" %in% colnames(newdat) &
     "end_page" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(pages, start_page, end_page, sep="-", na.rm=TRUE)
  } else{
    
    newdat <- newdat %>%
      mutate(pages = start_page)
  }
  
  newdat$date  <-  format(Sys.Date(), "%d%m%y")
  
  if (!"article_id" %in% colnames(newdat)) {
    newdat <- newdat %>%
      mutate(article_id = NA)
  }
  
  # print(newdat$article_id)
  
  newdat <- newdat %>%
    mutate(pmid = ifelse(!is.na(article_id), gsub("\\[.*", "", article_id), "")) %>%
    mutate(uid = paste0("embase-", AN)) 
  
  if("PT" %in% colnames(newdat)) {
    newdat$ptype <- newdat$PT
  }
  
  newdat$source <- "embase"
  
  newdat <- format_doi(newdat)
  newdat <- format_cols(newdat)
  
  return(newdat)
}


#' Process PsychInfo references
#'
#' This function processes references from PsychInfo.
#'
#' @param path The file path of the data.
#'
#' @return A processed data frame.
#'
#' @details
#' This function reads references from the specified file using the "ovid" tag naming convention. 
#' It performs various transformations on the data, such as uniting title and booktitle columns,
#' formatting pages, creating a unique identifier (uid) based on the article_id, and setting
#' the data source to "psychinfo". 
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' psychinfo_data <- process_psychinfo("path/to/psychinfo_data.csv")
#' }
#'
#' @importFrom synthesisr read_refs
#' @importFrom tidyr unite
#' @import dplyr
process_psychinfo <- function(path){
  
  newdat <- synthesisr::read_refs(path, tag_naming = "ovid")
  
  newdat$number <- newdat$issue
  newdat$ptype <- newdat$PT
  newdat$url <- newdat$L2
  newdat$author_affiliation <- newdat$M2
  
  if("booktitle" %in% colnames(newdat)) {
    newdat <-newdat %>%
      tidyr::unite(title, booktitle, na.rm = TRUE)
  }
  
  else if("start_page" %in% colnames(newdat) &
          "end_page" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(pages, start_page, end_page, sep="-", na.rm=TRUE)
  } else{
    
    newdat <- newdat %>%
      mutate(pages = start_page)
  }
  
  newdat <- newdat %>%
    mutate(pages = ifelse(pages == "No-Specified", NA_character_, paste(pages)))
  
  newdat$date  <-  format(Sys.Date(), "%d%m%y")
  
  if (!"article_id" %in% colnames(newdat)) {
    newdat <- newdat %>%
      mutate(article_id = NA)
  }
  
  print(newdat$article_id)
  
  newdat <- newdat %>%
    mutate(pmid = "") %>%
    mutate(uid = paste0("psychinfo-", article_id)) 
  
  newdat$source <- "psychinfo"
  
  newdat <- format_doi(newdat)
  newdat <- format_cols(newdat)
  
  return(newdat)
}

#' Process Medline references
#'
#' This function processes references from Medline.
#'
#' @param path The file path of the data.
#'
#' @return A processed data frame.
#'
#' @details
#' This function reads references from the specified file using the "ovid" tag naming convention. 
#' It performs various transformations on the data, such as uniting title and booktitle columns,
#' formatting pages, creating a unique identifier (uid) based on the article_id, and setting
#' the data source to "medline".
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' medline_data <- process_medline("path/to/medline_data.csv")
#' }
#'
#' @importFrom synthesisr read_refs
#' @importFrom tidyr unite
#' @import dplyr
process_medline <- function(path){
  
  newdat <- synthesisr::read_refs(path, tag_naming = "ovid")
  
  newdat$number <- newdat$issue
  newdat$ptype <- newdat$PT
  newdat$url <- newdat$L2
  newdat$author_affiliation <- newdat$M2
  
  if("booktitle" %in% colnames(newdat)) {
    newdat <-newdat %>%
      tidyr::unite(title, booktitle, na.rm = TRUE)
  }
  
  if("start_page" %in% colnames(newdat) &
     "end_page" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(pages, start_page, end_page, sep="-", na.rm=TRUE)
  } else{
    
    newdat <- newdat %>%
      mutate(pages = start_page)
  }
  
  newdat$date  <-  format(Sys.Date(), "%d%m%y")
  
  if (!"article_id" %in% colnames(newdat)) {
    newdat <- newdat %>%
      mutate(article_id = NA)
  }
  
  print(newdat$article_id)
  
  newdat <- newdat %>%
    mutate(pmid = article_id) %>%
    mutate(uid = paste0("medline-", article_id)) 
  
  newdat$source <- "medline"
  
  newdat <- format_doi(newdat)
  newdat <- format_cols(newdat)
  
  return(newdat)
}

#' Process Web of Science references
#'
#' This function processes references from Web of Science.
#'
#' @param path The file path of the data.
#'
#' @return A processed data frame.
#'
#' @details
#' This function reads references from the specified file in BibTeX or RIS format using the "wos" database source.
#' The function checks the file extension before processing accordingly.  
#' It adjusts column names according to the field codes, removes blank columns, adds source information, 
#' creates a unique identifier (uid) by modifying the existing uid column, and sets the data source to "wos".
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' wos_data <- process_wos("path/to/wos_data.bib")
#' }
#'
#' @importFrom bibliometrix convert2df
#' @importFrom tidyr unite
#' @import dplyr
process_wos <- function(path){
  
  # Extract file extension
  file_extension <- tools::file_ext(path)
  
  # Check file extension
  if (file_extension %in% c("ris")) {
    
    # Extract data
    newdat <- synthesisr::read_refs(path, tag_naming = "ovid")
    
    # Look for column names
    if("start_page" %in% colnames(newdat) &
       "end_page" %in% colnames(newdat)) {
      newdat <- newdat %>%
        tidyr::unite(pages, start_page, end_page, sep="-", na.rm=TRUE)
    } else{
      
      newdat <- newdat %>%
        mutate(pages = start_page)
    }
    
    if (!"secondarytitle" %in% colnames(newdat)) {
      newdat <- newdat %>%
        mutate(secondarytitle = NA)
    }
    if (!"isbn" %in% colnames(newdat)) {
      newdat <- newdat %>%
        mutate(isbn = NA)
    }
    if (!"pmid" %in% colnames(newdat)) {
      newdat <- newdat %>%
        mutate(pmid = NA)
    }
    if (!"author_country" %in% colnames(newdat)) {
      newdat <- newdat %>%
        mutate(author_country = NA)
    }
    
    # Add source, date, url info
    newdat$source <- "wos"
    newdat$date  <-  format(Sys.Date(), "%d%m%y")
    newdat$url <- paste0("https://www.webofscience.com/wos/woscc/full-record/", newdat$uid)
    
    # Sort out column names
    newdat <- newdat %>% 
      select(uid = AN, author = AU, year = PY, journal = T2, doi, title = TI, 
             pages, volume, abstract = AB, 
             isbn, keywords, secondarytitle, url, date, issn, pmid, ptype = source_type,
             source, number = issue, author_country, author_affiliation = PA)
    
    # Format
    newdat <- format_doi(newdat)
    newdat <- format_cols(newdat)
    
  } else if (file_extension %in% c("bib")) {
    
    # read in 
    newdat <- bibliometrix::convert2df(path, dbsource = "wos", format="bibtex")
    
    # sort out naming
    lookup_table <- setNames(field_codes_wos$Field, field_codes_wos$Abbreviation)
    colnames(newdat) <- lookup_table[colnames(newdat)]
    
    # remove columns that are blank
    keep.cols <- names(newdat) %in% NA
    newdat <- newdat [! keep.cols]
    rownames(newdat) <- 1:nrow(newdat)
    
    # adding source info
    newdat$source <- "wos"
    newdat['uid'] <- lapply(newdat['uid'], function(x) gsub("WOS", "wos:", x))
    newdat$author_country <- stringr::str_extract(newdat$author_country, "\\b(\\w+)\\b$")
    newdat$author_country  <- tools::toTitleCase(newdat$author_country)
    newdat$ptype  <- newdat$article_type
    
    newdat <- format_doi(newdat)
    newdat <- format_cols(newdat)
    
  }
  
  return(newdat)
  
}

#' Process PubMed references
#'
#' This function processes references from PubMed.
#'
#' @param path The file path of the data.
#'
#' @return A processed data frame.
#'
#' @details
#' This function reads references from the specified file in PubMed format using the "pubmed" database source. 
#' It adjusts column names according to the field codes, removes blank columns, adds source information, 
#' creates a unique identifier (uid) based on the record_id, sets the data source to "pubmed", and 
#' formats the doi column based on available information.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' pubmed_data <- process_pubmed("path/to/pubmed_data.bib")
#' }
#'
#' @importFrom bibliometrix convert2df
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @import dplyr
process_pubmed <- function(path){
  # try wos format
  newdat <- bibliometrix::convert2df(path, dbsource = "pubmed", format="pubmed")
  
  # sort out naming
  lookup_table <- setNames(field_codes_pubmed$Field, field_codes_pubmed$Abbreviation)
  colnames(newdat) <- lookup_table[colnames(newdat)]
  
  # remove columns that are blank
  keep.cols <- names(newdat) %in% NA
  newdat <- newdat [! keep.cols]
  rownames(newdat) <- 1:nrow(newdat)
  
  newdat$source <- "pubmed"
  colnames(newdat) <- gsub("^keywords_plus.*$", "keywords_plus", colnames(newdat))
  newdat$ptype <- newdat$document_type
  newdat <- newdat %>%
    mutate(uid = paste0("pubmed-", record_id)) %>%
    mutate(pmid = record_id) %>%
    mutate(doi = ifelse(is.na(doi), stringr::str_extract(article_ids, "\\b10\\.\\d{4,}\\/[\\S]+(?=\\s\\[DOI\\])"), doi))
  
  newdat <- format_doi(newdat)
  newdat <- format_cols(newdat)
  
  return(newdat)
}

#' Process Scopus references
#'
#' This function processes references from Scopus.
#'
#' @param path The file path of the data.
#'
#' @return A processed data frame.
#'
#' @details
#' This function reads references from the specified file using the "ovid" tag naming convention. 
#' It performs various transformations on the data, such as uniting title and booktitle columns,
#' formatting pages, creating a unique identifier (uid) based on the Scopus EID, and setting
#' the data source to "scopus". Additionally, it prints the values of the article_id column.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' scopus_data <- process_scopus("path/to/scopus_data.csv")
#' }
#'
#' @importFrom synthesisr read_refs
#' @importFrom tidyr unite
#' @import dplyr
process_scopus <- function(path){
  
  newdat <- synthesisr::read_refs(path, tag_naming = "scopus")
  
  newdat$number <- newdat$issue
  
  # affiliation
  if("address" %in% colnames(newdat)) {
    newdat$author_affiliation <- newdat$address
  } else if("affiliations" %in% colnames(newdat)) {
    newdat$author_affiliation <- newdat$affiliations
  }
  
  # journal name
  if("source" %in% colnames(newdat) &
     isFALSE(grepl("scopus", newdat$source, ignore.case = T))) {
    newdat$journal <- newdat$source
  } else if("source_title" %in% colnames(newdat)) {
    newdat$journal <- newdat$source_title
  }
  
  # publication type
  if("source_type" %in% colnames(newdat)) {
    newdat$ptype <- newdat$source_type
  } else if("document_type" %in% colnames(newdat)) {
    newdat$ptype <- newdat$document_type
  }
  
  if("booktitle" %in% colnames(newdat)) {
    newdat <-newdat %>%
      tidyr::unite(title, booktitle, na.rm = TRUE)
  }
  
  # pages
  if("start_page" %in% colnames(newdat) &
     "end_page" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(pages, start_page, end_page, sep="-", na.rm=TRUE)
    
  } else if("page_start" %in% colnames(newdat) &
            "page_end" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(pages, page_start, page_end, sep="-", na.rm=TRUE)
    
  } else {
    
    newdat <- newdat %>%
      mutate(pages = start_page)
  }
  
  # date
  newdat$date  <-  format(Sys.Date(), "%d%m%y")
  
  # identifier
  if (!"article_id" %in% colnames(newdat)) {
    newdat <- newdat %>%
      mutate(article_id = NA)
  }
  
  pattern <- "eid=(2-s2\\.0-\\d+)"
  
  if(!"url" %in% colnames(newdat) &
     "link" %in% colnames(newdat)) {
    newdat <- newdat %>% rename(url = link)
  }
  
  newdat <- newdat %>%
    ungroup() %>%
    mutate(uid = ifelse(!is.na(url), stringr::str_extract(url, pattern), 
                        paste0("unknown-accession-", floor(runif(n(), min=100, max=10000000))))) %>%
    dplyr::mutate(uid = gsub("eid=", "", uid)) %>%
    dplyr::mutate(pmid = pubmed_id) %>%
    dplyr::mutate(uid = paste0("scopus-", uid))
  
  # Check for duplicate IDs - extremely unlikely
  dup_id <- newdat %>% 
    dplyr::group_by(uid) %>%
    dplyr::count() %>%
    filter(n > 1)
  
  # If duplicates are found, return an error message
  if (nrow(dup_id) > 0) {
    stop("There's been an error in generating identifiers for SCOPUS. Please retry.")
  }
  
  newdat$source <- "scopus"
  
  # additional formatting for potential new scopus export
  if(!"author" %in% colnames(newdat) &
     "ef_bb_bf_author" %in% colnames(newdat)) {
    newdat <- newdat %>% rename(author = ef_bb_bf_author)
  }
  
  if(!"keywords" %in% colnames(newdat) &
     "author_keywords" %in% colnames(newdat)) {
    newdat <- newdat %>% rename(keywords = author_keywords)
  }
  
  newdat <- format_doi(newdat)
  newdat <- format_cols(newdat)
  
  return(newdat)
}

#' Process XML File
#'
#' This function parses an XML file containing bibliographic information and extracts relevant fields.
#'
#' @param path The file path to the XML file.
#' @return A data frame containing parsed bibliographic information.
#' @import XML
#' @import dplyr
#' @export
process_xml <- function(path){
  
  newdat <- XML::xmlParse(path)
  x <- XML::getNodeSet(newdat, "//record")
  
  xpath2 <- function(x, ...) {
    y <- XML::xpathSApply(x, ...)
    y <- gsub(",", "", y)
    ifelse(length(y) == 0, NA, paste(y, collapse = ", "))
  }
  
  newdat <- data.frame(author = sapply(x, xpath2, ".//author", xmlValue),
                       year = sapply(x, xpath2, ".//dates/year", xmlValue),
                       journal = sapply(x, xpath2, ".//periodical/full-title",  xmlValue),
                       doi = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
                       title = sapply(x, xpath2, ".//titles/title",  xmlValue),
                       pages = sapply(x, xpath2, ".//pages", xmlValue),
                       volume = sapply(x, xpath2, ".//volume", xmlValue),
                       number = sapply(x, xpath2, ".//number",  xmlValue),
                       abstract = sapply(x, xpath2, ".//abstract",  xmlValue),
                       keywords = sapply(x, xpath2, ".//keywords/keyword",  xmlValue),
                       record_id = sapply(x, xpath2, ".//rec-number", xmlValue),
                       isbn = sapply(x, xpath2, ".//isbn", xmlValue),
                       secondary_title = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
                       pmid = sapply(x, xpath2, ".//custom2", xmlValue),
                       label = sapply(x, xpath2, ".//label", xmlValue),
                       database = sapply(x, xpath2, ".//remote-database-name", xmlValue),
                       accession = sapply(x, xpath2, ".//accession-num", xmlValue),
                       url = sapply(x, xpath2, ".//url", xmlValue))
  
    pattern <- "eid=(2-s2\\.0-\\d+)"
    
    newdat <- newdat %>%
      mutate(source = case_when(
        grepl("scopus", url, ignore.case = TRUE) ~ "scopus",
        grepl("Embase", database, ignore.case = TRUE) ~ "embase",
        grepl("pubmed", database, ignore.case = TRUE) ~ "pubmed",
        grepl("BIOSIS", url, ignore.case = TRUE) ~ "biosis",
        grepl("BCI:", url, ignore.case = TRUE) ~ "biosis",
        grepl("wos", accession, ignore.case = TRUE) ~ "wos",
        grepl("medline", database, ignore.case = TRUE) ~ "medline",
        TRUE ~ "unknown"  # Keep the original source if none of the conditions match
      )) %>%
      ungroup()
    
    newdat <- newdat %>%
      rowwise() %>%
      mutate(accession = ifelse(source %in% "scopus", regmatches(url, regexpr(pattern, url)), accession)) %>%
      ungroup() %>%
      mutate(accession = ifelse(is.na(accession), paste0("noid:", 1000 + row_number()), accession)) %>%
      mutate(accession = gsub("eid=", "", accession)) %>%
      mutate(accession = gsub("WOS:", "", accession)) %>%
      mutate(uid = paste0(source, "-", accession)) %>%
      mutate(uid = gsub("unknown-noid:", "unknown-", uid))  
  

  format_cols(newdat)
  format_doi(newdat)
  
  return(newdat)
}


