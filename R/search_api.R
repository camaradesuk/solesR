#' Search Scopus and retrieve bibliographic data using scopusAPI
#'
#' @description
#' A wrapper function for scopusAPI. Search Scopus using a query and retrieve results programmatically.
#' Requires `devtools::install_github(kaitlynhair/scopusAPI)`.
#' Requires an API key from Scopus (http://dev.elsevier.com/).
#' Use the function `usethis::edit_r_environ()` to add the key to your ~/.Renviron file.
#'
#' @param query a character string containing a correctly syntaxed Scopus search
#' @param api_key a working Scopus API key
#' @param retMax The maximum number of records to retrieve, default is 2000, maximum is 5000
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
scopus_search <- function(query = NULL, api_key = NULL, retMax = 2000, format_soles = TRUE) {
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop(message("Error: you have not entered a search query"))
  }

  # Check for API key and exit if NULL
  if (is.null(api_key)) {
    stop(message("Error: you have not entered an API key"))
  }

  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax %% 1 != 0 | retMax < 0) {
    stop(message("Error: retMax is not a whole number"))
  }

  # Check retMax and exit if above maximum
  if (retMax > 5000) {
    stop(message("Error: retMax is too high"))
  }

  # Check format_soles is boolean and exit if not
  if (is.logical(format_soles) == FALSE) {
    stop(message("Error: format_soles should be set to TRUE or FALSE, default is TRUE"))
  }

  # Print message
  message("Running Scopus search...")

  # Try running search query using scopusAPI R package
  scopus_results <- tryCatch(
    {
      # Try getting results
      scopus_results <- scopusAPI::search_scopus(
        string = query,
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
  message("Retrieved ", nrow(scopus_results), "records from Scopus")

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
  }

  # Change all "NA" to real NA
  scopus_results[scopus_results == "NA"] <- NA
  # Change all blanks to NA
  scopus_results[scopus_results == ""] <- NA

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
#' @examples
#' \dontrun{
#' # api_key should be stored in your r environment and not shared
#' query <- "TS=(dementia OR \"memory loss\")"
#' wos_result <- wos_search(query, timespan = "1week", format_soles = FALSE)
#' }
#' @import dplyr
#' @import rwoslite
#'
#' @export
#'

wos_search <- function(query = NULL, timespan = NULL, format_soles = TRUE) {
  # Check for query and exit if NULL
  if (is.null(query)) {
    stop(message("Error: you have not entered a search query"))
  }

  # Check for API key and exit if NULL
  if (is.null(timespan)) {
    stop(message("Error: you have not entered a timespan for the search"))
  }

  # Check format_soles is boolean and exit if not
  if (is.logical(format_soles) == FALSE) {
    stop(message("Error: format_soles should be set to TRUE or FALSE, default is TRUE"))
  }

  if (grepl("^(?i)\\d+(week|month)$", timespan) == FALSE) {
    stop(message("Error: timespan format incorrect"))
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

  # Append timespan to user query to define final query
  full_query <- paste0("(", query, ") AND LD=(", min_date_char, "/", max_date_char, ")")

  # Set database to search as "WOS"
  database <- "WOS"

  # Print message
  message("Running Web of Science Core Collection search...")

  # Try seeing how many records are captured by query
  n_records <- tryCatch(
    {
      # Check number of records
      n_records <- rwoslite::wos_search(full_query, database)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling rwoslite::wos_search()", conditionMessage(e))
    }
  )

  # Exit function if number of records is 0
  if (n_records == 0) {
    stop(message("Error: no search results were found from query: ", query))
  }

  # Try running search query using rwoslite R package
  wos_results <- tryCatch(
    {
      # Try getting results
      wos_results <- rwoslite::wos_get_records(full_query)
    },
    error = function(e) {
      # Print error message and exit if error occurred
      stop("Error in calling rwoslite::search_wos()", conditionMessage(e))
    }
  )

  # Return results if successful
  message("Retrieved ", nrow(wos_results), " records from Web of Science Core Collection")

  # Format for SOLES workflow if format_soles == TRUE
  if (format_soles == TRUE) {
    # Print message
    message("Formatting records for SOLES...")
    # Rename and create columns for SOLES
    wos_results <- wos_results %>%
      # Rename columns
      dplyr::rename(
        author = .data$authors,
        journal = .data$source,
        year = .data$published_year
      ) %>%
      dplyr::mutate(
        source = "wos",
        # Create unique identifier
        uid = tolower(.data$ut),
        # Format author column
        author = gsub("[[:space:]]\\|[[:space:]]", "; ", .data$author),
        # Format journal column
        journal = tolower(.data$journal),
        journal = tools::toTitleCase(.data$journal),
        journal = as.character(.data$journal),
        # Add empty abstract column
        abstract = NA,
        # Format search date as character in format DDMMYY
        date = format(Sys.Date(), "%d%m%y")
      ) %>%
      # Remove rows with no ID
      dplyr::filter(!is.na(.data$ut))
  }

  # Change all "NA" to real NA
  wos_results[wos_results == "NA"] <- NA
  # Change all blanks to NA
  wos_results[wos_results == ""] <- NA

  # Make DOI lowercase
  wos_results$doi <- tolower(wos_results$doi)

  # Return search results
  return(wos_results)
}


#' Search PubMed and retrieve bibliographic data using RISmed
#'
#' @description
#' A wrapper function for RISmed. Search PubMed using a query and retrieve results programmatically.
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
    stop(message("Error: you have not entered a search query"))
  }

  # Check for API key and exit if NULL
  if (is.null(timespan)) {
    stop(message("Error: you have not entered a timespan for the search"))
  }

  # Check format_soles is boolean and exit if not
  if (is.logical(format_soles) == FALSE) {
    stop(message("Error: format_soles should be set to TRUE or FALSE, default is TRUE"))
  }

  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax %% 1 != 0 | retMax < 0) {
    stop(message("Error: retMax is not a whole number"))
  }

  # Check retMax and exit if above maximum
  if (retMax > 5000) {
    stop(message("Error: retMax is too high"))
  }

  if (grepl("^(?i)\\d+(week|month)$", timespan) == FALSE) {
    stop(message("Error: timespan format incorrect"))
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

  # Get summary of NCBI EUtils query
  pubmed_search <- RISmed::EUtilsSummary(
    query, 
    retmax=retMax,
    mindate = paste0(format(min_date_char, "%Y/%m/%d")),
    maxdate=paste0(format(max_date_char, "%Y/%m/%d")),
    type="esearch", db="pubmed")

  # Get summary
  pubmed_summary <- RISmed::summary(pubmed_search)

  # Try running search query using RISmed R package
  pubmed_records <- tryCatch(
    {
      # Try getting results
      records <- RISmed::EUtilsGet(pubmed_search)
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
      stop(message("No records found"))
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

  # Change all "NA" to real NA
  pubmed_results[pubmed_results == "NA"] <- NA
  # Change all blanks to NA
  pubmed_results[pubmed_results == ""] <- NA

  # Make DOI lowercase
  pubmed_results$doi <- tolower(pubmed_results$doi)

  # Print number of records retrieved
  message("\nRetrieved ", nrow(pubmed_results), " records from PubMed")

  # Return results
  return(pubmed_results)
}
