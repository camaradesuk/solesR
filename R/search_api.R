#' Search Scopus and retrieve bibliographic data using scopusAPI
#'
#' @description
#' A wrapper function for scopusAPI. Search Scopus using a query and retrieve results programmatically.
#' Requires `devtools::install_github(kaitlynhair/scopusAPI)`.
#' Requires an API key from Scopus (http://dev.elsevier.com/).
#' Use the function `usethis::edit_r_environ()` to add the key to your ~/.Renviron file.
#' The timespan should be formatted as a number followed by the word "week" or "month", e.g. "1month" or "2week".
#'
#' @param query a character string containing a correctly syntaxed Scopus search
#' @param api_key a working Scopus API key
#' @param timespan a formatted character string defining the timespan you want to search
#' @param retMax The maximum number of records to retrieve, default is 5000, maximum is 5000
#' @param format_soles boolean, if set to TRUE will format search results for the SOLES workflow, default is TRUE
#' @return a dataframe containing Scopus search results
#' @examples
#' \dontrun{
#' # api_key should be stored in your r environment and not shared
#' query <- "TITLE-ABS-KEY(dementia OR \"memory loss\")"
#' scopus_result <- scopus_search(query, api_key = api_key, retMax = 500, format_soles = FALSE)
#' }
#' @import dplyr
#' @import scopusAPI
#' @export
#'
scopus_search <- function(query = NULL, api_key = NULL, timespan = NULL, retMax = 5000, format_soles = TRUE) {
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop("Error: you have not entered a search query")
  }
  
  # Check for API key and exit if NULL
  if (is.null(api_key)) {
    stop("Error: you have not entered an API key")
  }
  
  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax %% 1 != 0 | retMax < 0) {
    stop("Error: retMax is not a whole number")
  }
  
  # Check retMax and exit if above maximum
  if (retMax > 5000) {
    stop("Error: retMax is too high")
  }
  
  # Check format_soles is boolean and exit if not
  if (is.logical(format_soles) == FALSE) {
    stop("Error: format_soles should be set to TRUE or FALSE, default is TRUE")
  }
  
  # Check for API key and exit if NULL
  if (is.null(timespan)) {
    stop("Error: you have not entered a timespan for the search")
  }
  
  if (grepl("^(?i)\\d+(week|month)s?$", timespan) == FALSE) {
    stop("Error: timespan format incorrect")
  }
  
  # Define timespan for search
  if (grepl("(?i)week", timespan) == TRUE) {
    # Get number of weeks by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of weeks before today's date
    min_date_char <- Sys.Date() - 7 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  } else if (grepl("(?i)month", timespan) == TRUE) {
    # Get number of months by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of months before today's date
    min_date_char <- Sys.Date() - 31 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  }
  
  # Minus one day from min date (because scopus only searches after this date)
  min_date_char <- min_date_char - 1
  
  # Append timespan to user query to define final query
  full_query <- paste0("(", query, ") AND ORIG-LOAD-DATE > ", min_date_char)
  
  # Print message
  message("Running Scopus search...")
  
  # Try running search query using scopusAPI R package
  scopus_results <- tryCatch(
    {
      # Try getting results
      scopus_results <- scopusAPI::search_scopus(
        string = full_query,
        api_key = api_key,
        retMax = retMax
      )
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling scopusAPI::search_scopus()", conditionMessage(e))
    }
  )
  
  # Return results if successful
  message("Retrieved ", nrow(scopus_results), " records from Scopus")
  
  # Format for SOLES workflow if format_soles == TRUE
  if (format_soles == TRUE) {
    # Print message
    message("Formatting records for SOLES...")
    # Rename and create columns for SOLES
    scopus_results <- scopus_results %>%
      # Format search date as character in format DDMMYY
      dplyr::mutate(date = format(Sys.Date(), "%d%m%y")) %>%
      # Remove rows with no ID
      dplyr::filter(!is.na(.data$scopusID))
    # Print message
    message("Formatted!")
  }
  
  # Change no abstract available to NA
  scopus_results$abstract <- gsub("^\\[No abstract available\\]$", "", scopus_results$abstract)
  # Change all "NA" to real NA
  scopus_results[scopus_results == "NA"] <- NA
  # Change all blanks to NA
  scopus_results[scopus_results == ""] <- NA
  
  # Make DOI lowercase
  scopus_results$doi <- tolower(scopus_results$doi)
  
  # Remove any additional DOIs (e.g., elife versioning)
  scopus_results$doi <- gsub("; .+$", "", scopus_results$doi)
  
  # Return search results
  return(scopus_results)
}

#' Search Web of Science Core Collection and retrieve bibliographic data
#'
#' @description
#' Search the Web of Science Core Collection using a query and retrieve results programmatically,
#' via the Clarivate Web of Science Starter API (\url{https://developer.clarivate.com/apis/wos-starter}).
#' Requires an API key from the Clarivate Developer Portal, stored in the \code{WOS_KEY} environment
#' variable (e.g. via \code{Sys.setenv(WOS_KEY = "your-key")} or in your \code{.Renviron} file).
#' The timespan should be formatted as a number followed by the word "week" or "month", e.g. "1month" or "2week".
#' One week is calculated as 7 days and one month is calculated as 31 days.
#'
#' @details
#' Records are retrieved via \code{wos_get_records()}, which pages through the WoS Starter API's
#' \code{/documents} endpoint (\url{https://api.clarivate.com/apis/wos-starter/v1/documents}),
#' up to a maximum of 2000 records per call. If no records are found for the given query and
#' timespan, the function prints a message and returns an empty data frame rather than erroring.
#'
#' @param query a character string containing a correctly syntaxed Web of Science search
#' @param timespan a formatted character string defining the timespan you want to search
#' @param format_soles boolean, if set to TRUE will format search results for the SOLES workflow, default is TRUE
#' @return a dataframe containing Web of Science search results
#' @examples
#' \dontrun{
#' # api_key should be stored in your r environment and not shared
#' query <- "TS=(dementia OR \"memory loss\")"
#' wos_result <- wos_search(query, timespan = "1week", format_soles = FALSE)
#' }
#' @import dplyr
#'
#' @export
#'
wos_search <- function(query = NULL, timespan = NULL, format_soles = TRUE) {
  
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop("Error: you have not entered a search query")
  }
  
  # Check for API key and exit if NULL
  if (is.null(timespan)) {
    stop("Error: you have not entered a timespan for the search")
  }
  
  # Check format_soles is boolean and exit if not
  if (is.logical(format_soles) == FALSE) {
    stop("Error: format_soles should be set to TRUE or FALSE, default is TRUE")
  }
  
  if (Sys.getenv("WOS_KEY") == "") {
    stop("Error: no API key found. Set your key with Sys.setenv(WOS_KEY = \"your-key\") or in your .Renviron file.")
  }
  
  if (grepl("^\\d+(week|month)s?$", timespan, ignore.case = TRUE) == FALSE) {
    stop("Error: timespan format incorrect")
  }
  
  # Define timespan for search
  if (grepl("week", timespan, ignore.case = TRUE) == TRUE) {
    # Get number of weeks by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of weeks before today's date
    min_date_char <- Sys.Date() - 7 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  } else if (grepl("month", timespan, ignore.case = TRUE) == TRUE) {
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
  full_query <- paste0("(", query, ") AND LD=(", min_date_char, "/", max_date_char, ")")
  
  # Print message
  message("Running Web of Science Core Collection search...")
  
  # Try running search query using wos_get_records() helper
  wos_results <- tryCatch(
    {
      wos_results <- wos_get_records(
        query = full_query,
        api_key = Sys.getenv("WOS_KEY"), 
        max_records = 2000
      )
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling wos_get_records():", conditionMessage(e))
    }
  )
  
  # Return results if successful
  message("Retrieved ", nrow(wos_results), " records from Web of Science Core Collection")
  
  # Exit function if number of records is 0
  if (nrow(wos_results) == 0) {
    message("No search results were found from query.")
    return(data.frame())
    
  }
  
  # Format for SOLES workflow if format_soles == TRUE
  if (format_soles == TRUE) {
    # Print message
    message("Formatting records for SOLES...")
    
    # Rename and create columns for SOLES
    wos_results <- wos_results %>%
      mutate(
        uid                = tolower(safe_col(wos_results, "uid")),
        journal            = safe_col(wos_results, "source", "sourceTitle"),
        journal            = tools::toTitleCase(tolower(journal)),
        year               = safe_col(wos_results, "source", "publishYear"),
        doi                = safe_col(wos_results, "identifiers", "doi"),
        author             = sapply(names$authors, function(a) {
          paste(a$displayName, collapse = "; ")
        }),
        pages              = safe_col(wos_results, "source", "pages", "range"),
        # pages              = NA_character_,
        volume             = safe_col(wos_results, "source", "volume"),
        abstract           = NA_character_,
        isbn               = NA_character_,
        keywords           = sapply(keywords$authorKeywords, paste, collapse = "; "),
        secondarytitle     = NA_character_,
        url                = safe_col(wos_results, "links", "record"),
        date               = format(Sys.Date(), "%d%m%y"),
        issn               = safe_col(wos_results, "identifiers", "issn"),
        pmid               = safe_col(wos_results, "identifiers", "pmid"),
        ptype              = tolower(sapply(types, paste, collapse = "; ")),
        author_country     = NA_character_,
        number             = safe_col(wos_results, "source", "issue"),
        author_affiliation = NA_character_,
        source             = "wos"
      ) %>%
      select(
        uid, source, author, year, journal, doi, title, pages, volume,
        abstract, isbn, keywords, secondarytitle, url, date, issn, pmid,
        ptype, author_country, number, author_affiliation
      )
    
    # Change "no abstract available" placeholder text to NA
    wos_results$abstract <- gsub("^\\[No abstract available\\]$", "", wos_results$abstract)
    # Change all "NA" strings to real NA
    wos_results[wos_results == "NA"] <- NA_character_
    # Change all blanks to NA
    wos_results[wos_results == ""] <- NA_character_
    
    # Format DOI
    wos_results <- format_doi(wos_results)
    
    # Remove any additional DOIs (e.g., elife versioning)
    wos_results$doi <- gsub("; .+$", "", wos_results$doi)
    
    # Format the UID for SOLES
    wos_results$uid <- gsub("^wos:", "wos-", wos_results$uid)
    
    # Print message
    message("Formatted!")
  }
  
  # Return search results
  return(wos_results)
}


#' Search PubMed and retrieve bibliographic data using RISmed
#'
#' @description
#' A wrapper function for RISmed (https://github.com/kaitlynhair/RISmed). Search PubMed using a query and retrieve results programmatically.
#' The timespan should be formatted as a number followed by the word "week" or "month", e.g. "1month" or "2week".
#' One week is calculated as 7 days and one month is calculated as 31 days.
#'
#' @param query a character string containing a correctly syntaxed PubMed search
#' @param timespan a formatted character string defining the timespan you want to search
#' @param retMax The maximum number of records to retrieve, default is 5000, maximum is 5000
#' @param format_soles boolean, if set to TRUE will format search results for the SOLES workflow, default is TRUE
#' @return a dataframe containing Web of Science search results
#' @examples
#' \dontrun{
#' query <- "(dementia[tiab] OR \"memory loss\"[tiab])"
#' pubmed_result <- pubmed_search(query, timespan = "1week", format_soles = FALSE)
#' }
#' @import dplyr
#' @import RISmed
#'
#' @export
#'
pubmed_search <- function(query, timespan, retMax = 5000, format_soles = TRUE) {
  
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop("Error: you have not entered a search query")
  }
  
  # Check for API key and exit if NULL
  if (is.null(timespan)) {
    stop("Error: you have not entered a timespan for the search")
  }
  
  # Check format_soles is boolean and exit if not
  if (is.logical(format_soles) == FALSE) {
    stop("Error: format_soles should be set to TRUE or FALSE, default is TRUE")
  }
  
  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax %% 1 != 0 | retMax < 0) {
    stop("Error: retMax is not a whole number")
  }
  
  # Check retMax and exit if above maximum
  if (retMax > 5000) {
    stop("Error: retMax is too high")
  }
  
  if (grepl("^(?i)\\d+(week|month)s?$", timespan) == FALSE) {
    stop("Error: timespan format incorrect")
  }
  
  # Define timespan for search
  if (grepl("(?i)week", timespan) == TRUE) {
    # Get number of weeks by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of weeks before today's date
    min_date_char <- Sys.Date() - 7 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  } else if (grepl("(?i)month", timespan) == TRUE) {
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
  
  # Add dates to query
  full_query <- paste0("(", query, ") AND ", paste0(format(min_date_char, "%Y/%m/%d")), ":3000/12/31[Date - Create]")
  
  # Get summary of NCBI EUtils query
  pubmed_search <- RISmed::EUtilsSummary(
    full_query,
    retmax = retMax,
    type  = "esearch",
    db    = "pubmed"
  )
  
  pubmed_summary <- RISmed::summary(pubmed_search)
  
  
  # If summary is empty, return an empty data frame
  if (length(pubmed_summary) == 0) {
    
    message("No search results were found from query.")
    return(data.frame())
  }
  
  # Try running search query using RISmed R package
  pubmed_results <- tryCatch(
    {
      # Try getting results
      pubmed_results <- RISmed::EUtilsGet(pubmed_search)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling RISmed::EUtilsGet()", conditionMessage(e))
    }
  )
  
  
  # Check if correct package is used
  try({
    if (pubmed_search@count < 1) {
      # Exit if no records found
      stop("No records found")
    } else if (inherits(pubmed_results, "Medline")) {
      # Return warning if result is a Medline object
      warning("Error: A Medline object is returned instead of a dataframe. For output compatible with the SOLES workflow, update the RISmed package from: https://github.com/kaitlynhair/RISmed")
    }
  })
  
  # Format dataframe for SOLES
  if (format_soles == TRUE) {
    pubmed_results <- pubmed_results %>%
      # Format search date as character in format DDMMYY
      dplyr::mutate(date = format(Sys.Date(), "%d%m%y")) %>%
      # Remove rows with no ID
      dplyr::filter(!is.na(.data$pmid))
  }
  
  # Change no abstract available to NA
  pubmed_results$abstract <- gsub("^\\[No abstract available\\]$", "", pubmed_results$abstract)
  # Change all "NA" to real NA
  pubmed_results[pubmed_results == "NA"] <- NA
  # Change all blanks to NA
  pubmed_results[pubmed_results == ""] <- NA
  
  # Make DOI lowercase
  pubmed_results$doi <- tolower(pubmed_results$doi)
  
  # Remove any additional DOIs (e.g., elife versioning)
  pubmed_results$doi <- gsub("; .+$", "", pubmed_results$doi)
  
  # Print number of records retrieved
  message("\nRetrieved ", nrow(pubmed_results), " records from PubMed")
  
  # Return results
  return(pubmed_results)
}

#' Search EuropePMC and retrieve bibliographic data using europepmc
#'
#' @description
#' A wrapper function for europepmc. Search EuropePMC using a query and retrieve results programmatically.
#'
#' @param query a character string containing a correctly syntaxed EuropePMC search
#' @param timespan a formatted character string defining the timespan you want to search
#' @param retMax The maximum number of records to retrieve, default is 5000, maximum is 5000
#' @param format_soles boolean, if set to TRUE will format search results for the SOLES workflow, default is TRUE
#' @return a dataframe containing EuropePMC search results
#' @examples
#' \dontrun{
#' query <- "TITLE_ABS:(dementia OR \"memory loss\")"
#' epmc_result <- epmc_search(query, timespan, retMax = 500, format_soles = FALSE)
#' }
#' @import dplyr
#' @import europepmc
#' @export
#'
epmc_search <- function(query = NULL, timespan, retMax = 5000, format_soles = TRUE) {
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop("Error: you have not entered a search query")
  }
  
  # Check for API key and exit if NULL
  if (is.null(timespan)) {
    stop("Error: you have not entered a timespan for the search")
  }
  
  # Check format_soles is boolean and exit if not
  if (is.logical(format_soles) == FALSE) {
    stop("Error: format_soles should be set to TRUE or FALSE, default is TRUE")
  }
  
  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax %% 1 != 0 | retMax < 0) {
    stop("Error: retMax is not a whole number")
  }
  
  # Check retMax and exit if above maximum
  if (retMax > 5000) {
    stop("Error: retMax is too high")
  }
  
  if (grepl("^(?i)\\d+(week|month)s?$", timespan) == FALSE) {
    stop("Error: timespan format incorrect")
  }
  
  # Define timespan for search
  if (grepl("(?i)week", timespan) == TRUE) {
    # Get number of weeks by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of weeks before today's date
    min_date_char <- Sys.Date() - 7 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  } else if (grepl("(?i)month", timespan) == TRUE) {
    # Get number of months by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Assign min date as x number of months before today's date
    min_date_char <- Sys.Date() - 31 * x
    # Assign max date as today
    max_date_char <- Sys.Date()
    # Print search dates
    message("Searching from ", min_date_char, " to ", max_date_char)
  }
  
  # Add timespan to query
  query <- paste0("(", query, ") AND FIRST_IDATE:[", as.character(min_date_char), " TO ", as.character(max_date_char),"]")
  
  # Print message
  message("Running EuropePMC search...")
  
  # Try running search query using scopusAPI R package
  epmc_results <- tryCatch(
    {
      # Try getting results
      europepmc::epmc_search(query, limit = retMax)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling europepmc::epmc_search", conditionMessage(e))
    }
  )
  
  # Return results if successful
  message("Retrieved ", nrow(epmc_results), "records from EuropePMC")
  
  # Format for SOLES workflow if format_soles == TRUE
  if (format_soles == TRUE) {
    # Print message
    message("Formatting records for SOLES...")
    # Rename and create columns for SOLES
    epmc_results <- epmc_results %>%
      dplyr::mutate(uid = paste0("epmc-", tolower(id)),
                    source = "epmc",
                    journal = NA,
                    pages = NA,
                    volume = NA,
                    abstract = NA,
                    isbn = NA,
                    keywords = NA,
                    secondarytitle = NA,
                    url = NA,
                    issn = NA,
                    pmid = NA,
                    author_country = NA,
                    number = NA,
                    author_affiliation = NA) %>%
      dplyr::select(uid, source, doi, title, author = authorString,
                    year = pubYear, ptype = pubType, journal, pages,
                    volume, abstract, isbn, keywords, secondarytitle,
                    url, issn, pmid, author_country, number, author_affiliation) %>%
      # Format search date as character in format DDMMYY
      dplyr::mutate(date = format(Sys.Date(), "%d%m%y")) %>%
      # Remove rows with no ID
      dplyr::filter(!is.na(.data$uid))
    # Print message
    message("Formatted!")
  }
  
  # Change no abstract available to NA
  epmc_results$abstract <- gsub("^\\[No abstract available\\]$", "", epmc_results$abstract)
  # Change all "NA" to real NA
  epmc_results[epmc_results == "NA"] <- NA
  # Change all blanks to NA
  epmc_results[epmc_results == ""] <- NA
  
  # Make DOI lowercase
  epmc_results$doi <- tolower(epmc_results$doi)
  
  # Remove any additional DOIs (e.g., elife versioning)
  epmc_results$doi <- gsub("; .+$", "", epmc_results$doi)
  
  # Return search results
  return(epmc_results)
}


#' Retrieve records from the Web of Science Starter API
#'
#' @description
#' Queries the Web of Science Starter API and automatically pages through
#' results, combining them into a single data frame. 
#'
#' @param query a character string containing a correctly syntaxed Web of Science search query
#' @param api_key character string, your Clarivate WoS Starter API key
#' @param database character string, the WoS database to search (default
#'   \code{"WOS"})
#' @param limit integer, number of records to request per page. Must be 50 or 
#'   less, the maximum page size allowed by the WoS Starter API;
#'   the function will error if a larger value is supplied
#' @param detail character string, level of record detail to request from
#'   the API (default \code{"full"})
#' @param max_records numeric, maximum number of records to retrieve across
#'   all pages. Defaults to \code{Inf}, i.e. retrieve all matching records
#'
#' @return a data frame of combined results from all downloaded pages, with
#'   the true total number of matching records (before any \code{max_records}
#'   truncation) attached as the attribute \code{"total_wos_results"}
#'
#' @export
#'
wos_get_records <- function(query,
                            api_key,
                            database = "WOS",
                            limit = 50,
                            detail = "full",
                            max_records = Inf) {
  
  # WoS Starter API caps page size at 50 records; warn the caller if their
  # requested limit exceeds this and will be silently reduced
  if (limit > 50) {
    stop(paste0("`limit` cannot exceed 50, the maximum page size allowed by the WoS Starter API. You requested ", limit, "."))
  }
  
  # Fetch page 1 first: this both returns the first batch of records and
  # tells us (via metadata$total) how many pages we'll need in total
  first_page <- get_page(1, api_key = api_key, database = database, 
                         query = query, limit = limit, detail = detail)
  
  # Total number of records matching the query, as reported by the API
  total_wos_results <- first_page$metadata$total
  
  message("Found ", total_wos_results, " records")
  
  # Don't retrieve more than max_records, even if more results exist
  n_records <- min(total_wos_results, max_records)
  
  
  # Number of pages needed to cover n_records at the given page size
  n_pages <- ceiling(n_records / limit)
  
  # Seed the results list with the page we've already downloaded
  results <- list(first_page$hits)
  
  # Only loop for remaining pages if there are any; start at page 2 since
  # page 1 has already been fetched and stored above
  if (n_pages > 1) {
    
    for (p in 2:n_pages) {
      
      message("Downloading page ", p, " of ", n_pages)
      
      page_res <- get_page(p, api_key = api_key, database = database, 
                           query = query, limit = limit, detail = detail)
      
      results[[p]] <- page_res$hits
      
      Sys.sleep(0.1)
    }
  }
  
  # Combine all pages into a single data frame; fills missing columns
  # with NA where record structures differ slightly between pages
  wos_results <- dplyr::bind_rows(results)
  
  # Trim to exactly n_records in case the last page overshoots
  # (each page can return up to page_size rows, which may exceed n_records)
  wos_results <- wos_results[seq_len(min(nrow(wos_results), n_records)), ]
  
  # Attach the true total as an attribute, so callers can
  # tell whether results were capped by max_records
  attr(wos_results, "total_wos_results") <- total_wos_results
  
  return(wos_results)
}

#' Fetch a single page of results from the Web of Science Starter API
#'
#' @description
#' Performs a single GET request against the Web of Science Starter API
#' documents endpoint and returns the parsed JSON response. Intended as an
#' internal helper used by \code{wos_get_records()} to retrieve one page of
#' results at a time; handles a single HTTP request only and does not
#' paginate itself.
#'
#' @param page integer, the page number to retrieve
#' @param endpoint character string, the API endpoint URL to query.
#'   Defaults to the WoS Starter API documents endpoint
#' @param api_key character string, your Clarivate WoS Starter API key
#' @param database character string, the WoS database to search (e.g.
#'   \code{"WOS"})
#' @param query character string, a correctly syntaxed Web of Science
#'   search query
#' @param limit integer, number of records to request for this page.
#'   Must not exceed 50, the maximum page size allowed by the WoS Starter
#'   API
#' @param detail character string, level of record detail to request from
#'   the API (e.g. \code{"full"})
#'
#' @return a list containing the parsed JSON response body, with nested
#'   JSON simplified into data frames/vectors where possible. Includes
#'   \code{$metadata} (containing the total record count, among other
#'   fields) and \code{$hits} (the records for this page)
#'
#' @export
get_page <- function(page, 
                     endpoint = "https://api.clarivate.com/apis/wos-starter/v1/documents", 
                     api_key,
                     database,
                     query,
                     limit,
                     detail) {
  
  # Build and send the GET request to the WoS Starter API documents
  # endpoint, authenticating via the X-ApiKey header
  resp <- httr2::request(
    endpoint
  ) %>%
    httr2::req_headers(
      accept = "application/json",
      "X-ApiKey" = api_key
    ) %>%
    # Attach query parameters: db/q define the search, limit/page control
    # pagination, and detail controls how much record data is returned
    httr2::req_url_query(
      db = database,
      q = query,
      limit = limit,
      page = page,
      detail = detail
    ) %>%
    httr2::req_perform()
  
  # simplifyVector = TRUE turns the nested JSON into data frames/vectors
  # where possible, which is what the downstream dplyr code expects
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Safely extract a (possibly nested) column from a data frame
#'
#' @description
#' Returns a vector for the requested column from \code{df}, defaulting to
#' a vector of \code{NA} (of length \code{nrow(df)}) if the column, or a
#' nested sub-field (up to two levels deep), does not exist. This guards
#' against the WoS Starter API omitting fields for some records.
#'
#' @param df a data frame
#' @param col character string, the top-level column name to extract
#' @param subcol optional character string, a sub-field to extract from a
#'   nested data frame column (e.g. \code{"source"} -> \code{"pages"})
#' @param sub2col optional character string, a further nested sub-field
#'   within \code{subcol} (e.g. \code{"source"} -> \code{"pages"} ->
#'   \code{"range"}). Ignored if \code{subcol} is \code{NULL}.
#' @param default value to fill with if the requested column/sub-field is
#'   missing (default \code{NA_character_})
#'
#' @return a vector of length \code{nrow(df)}
#' @export
safe_col <- function(df, col, subcol = NULL, sub2col = NULL, default = NA_character_) {
  n <- nrow(df)
  
  # Top-level column missing entirely - return all-default
  if (!col %in% names(df)) {
    return(rep(default, n))
  }
  
  value <- df[[col]]
  
  # No sub-field requested - return the column as-is
  if (is.null(subcol)) {
    return(value)
  }
  
  # Sub-field missing within the nested column - return all-default
  if (!subcol %in% names(value)) {
    return(rep(default, n))
  }
  
  value <- value[[subcol]]
  
  # No second-level sub-field requested - return what we have so far
  if (is.null(sub2col)) {
    return(value)
  }
  
  # Second-level sub-field missing - return all-default
  if (!sub2col %in% names(value)) {
    return(rep(default, n))
  }
  
  value[[sub2col]]
}


# =============================================================================
# Search helpers: run each database's searches, keep track of how far back
# each database needs to search, and combine the results
# =============================================================================

# Where the search state (weeks to search back, per database) is saved, and
# the state used when there is no saved file yet
state_file <- "search/search_state.rds"
default_state <- list(pubmed = 1, wos = 1, scopus = 1)

#' Read the saved search state
#'
#' @description
#' Reads how many weeks back each database should search. If there is no
#' saved state yet, or the file can't be read, the default state is used
#' (1 week for each database).
#'
#' @param path path to the saved state file (default "search/search_state.rds")
#' @param default state to use if there is no readable saved state
#' @return a named list of weeks per database, e.g. list(pubmed = 1, wos = 2, scopus = 1)
#' @export
#'
get_state <- function(path = state_file, default = default_state) {
  if (file.exists(path)) {
    tryCatch(readRDS(path), error = function(e) default)
  } else {
    default
  }
}

#' Save the search state
#'
#' @description
#' Saves how many weeks back each database should search next time.
#' Creates the folder if it doesn't exist yet.
#'
#' @param state a named list of weeks per database, as returned by get_state()
#'   or run_database_search()$state
#' @param path path to the state file (default "search/search_state.rds")
#' @export
#'
save_state <- function(state, path = state_file) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(state, path)
}

#' Run one database's searches using its saved timespan
#'
#' @description
#' Runs every query for one database, searching back the number of weeks saved
#' in the state for that database.
#'
#' If ANY query errors, the whole database is treated as failed (even if other
#' queries succeeded): no results are returned and the timespan is increased by
#' one week, so the next run covers the missed period. The same happens if the
#' searches succeed but return no results. On full success, the timespan is
#' reset to 1 week.
#'
#' @param db_name name of the database in the state, e.g. "pubmed", "wos" or
#'   "scopus". A database not in the state starts at 1 week.
#' @param queries a character vector (or list) of queries for this database
#' @param search_fn the search function, e.g. pubmed_search or wos_search
#' @param state the current state, as returned by get_state()
#' @param ... further arguments passed to search_fn, e.g. api_key for scopus_search
#' @param override_weeks optional number of weeks to search instead of the
#'   saved timespan (e.g. for a one-off catch-up search)
#' @return a list with `result` (the combined, de-duplicated results, or NULL
#'   if the database failed or found nothing) and `state` (the updated state;
#'   save it with save_state())
#' @examples
#' \dontrun{
#' state <- get_state()
#' pm <- run_database_search("pubmed", pubmed_queries, pubmed_search, state)
#' state <- pm$state
#' save_state(state)
#' }
#' @export
#'
run_database_search <- function(db_name, queries, search_fn, state, ..., override_weeks = NULL) {
  weeks <- if (!is.null(override_weeks)) {
    override_weeks
  } else if (!is.null(state[[db_name]])) {
    state[[db_name]]
  } else {
    1   # database not in the saved state yet
  }
  timespan <- paste0(weeks, "week")
  
  results <- run_searches(queries, search_fn, timespan = timespan, ...)
  
  has_error <- any(vapply(results, inherits, logical(1), "try-error"))
  
  if (has_error) {
    warning(db_name, " search had at least one failed query (timespan: ", timespan,
            "). Next run will use ", weeks + 1, " weeks.", call. = FALSE)
    state[[db_name]] <- weeks + 1
    combined <- NULL
  } else {
    combined <- combine_results(results)
    if (is.null(combined)) {
      warning(db_name, " search returned no results (timespan: ", timespan,
              "). Next run will use ", weeks + 1, " weeks.", call. = FALSE)
      state[[db_name]] <- weeks + 1
    } else {
      state[[db_name]] <- 1
    }
  }
  
  list(result = combined, state = state)
}

#' Run each query through a search function
#'
#' @description
#' Runs each query with search_fn, catching errors so one bad query doesn't
#' stop the rest. Failed queries are returned as "try-error" objects.
#'
#' @param queries a character vector (or list) of queries
#' @param search_fn the search function, e.g. pubmed_search
#' @param ... further arguments passed to search_fn, e.g. timespan
#' @return a list with one element per query: a data frame of results, or a
#'   "try-error" object if that query failed
#' @export
#'
run_searches <- function(queries, search_fn, ...) {
  lapply(queries, function(q) try(search_fn(q, ...)))  # try() each query, extra args passed to search_fn
}

#' Combine the results of several queries
#'
#' @description
#' Drops failed and empty results and combines the rest into one
#' de-duplicated data frame.
#'
#' @param results_list a list of results, as returned by run_searches()
#' @return a de-duplicated data frame, or NULL if no query returned any results
#' @importFrom dplyr bind_rows distinct
#' @export
#'
combine_results <- function(results_list) {
  ok <- Filter(function(df) is.data.frame(df) && nrow(df) > 0, results_list)  # keep only real, non-empty results
  if (length(ok) == 0) return(NULL)  # nothing usable
  dplyr::bind_rows(ok) %>% dplyr::distinct()  # stack results, remove duplicate rows
}

#' Combine the results from each database into one data frame
#'
#' @description
#' Combines per-database results (e.g. pubmed/wos/scopus) into one final data
#' frame using combine_searches(). Takes object NAMES (as strings) rather than
#' the objects themselves, so it can safely handle databases whose results were
#' never assigned (e.g. due to an earlier error).
#'
#' @param names character vector of object names, e.g. c("pubmed", "wos", "scopus")
#' @param envir environment to look for the objects in (default: where the
#'   function is called from)
#' @return a de-duplicated data frame, or NULL if none of the objects exist or
#'   are data frames
#' @export
#'
combine_existing_searches <- function(names, envir = parent.frame()) {
  
  # NULL for anything missing
  dfs <- mget(names, envir = envir, ifnotfound = list(NULL))
  # drop NULLs/non-data frames
  dfs <- Filter(function(x) !is.null(x) && is.data.frame(x), dfs)
  
  # nothing to combine
  if (length(dfs) == 0)
    return(NULL)
  
  # merge remaining data frames, dedup
  do.call(combine_searches, dfs) %>% dplyr::distinct()
}

#' Summarise a search result for reporting
#'
#' @description
#' Returns the number of records found, or a short status if there is no
#' usable result. Safe to call on objects that were never created.
#'
#' @param x a search result
#' @param null_msg text to return if x doesn't exist or is NULL
#' @return the number of rows if x is a data frame; otherwise null_msg,
#'   "Error" (for a "try-error" object) or "Unknown"
#' @export
#'
df_res_check <- function(x, null_msg = "Not available") {
  x <- tryCatch(x, error = function(e) NULL)   # catches objects that were never created
  if (is.null(x)) return(null_msg)
  if (inherits(x, "try-error")) return("Error")
  if (is.data.frame(x)) return(nrow(x))
  "Unknown"
}

#' Summarise one database's search result for reporting
#'
#' @description
#' As df_res_check(), but a missing result is reported as a failed search
#' whose timespan has been increased for the next run.
#'
#' @param x a database's search result, e.g. run_database_search()$result
#' @return the number of rows, or a status message
#' @export
#'
db_res_check <- function(x) {
  df_res_check(x, null_msg = "Search Failed - 1 week added to state.")
}