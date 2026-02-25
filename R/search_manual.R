#' Import Search Results from Manual Searches of Bibliographic Databases
#'
#' Read search results manually downloaded from bibliographic databases into R for use in SOLES workflows.
#' Columns are formatted for SOLES integration.
#'
#' Data can be imported from Ovid databases (Scopus, Embase, PsychINFO, or MEDLINE) in `.bib`, `.ris`, and `.txt` format;
#' Web of Science in `.bib` or `.ris` format; and PubMed in `.bib` format.
#' Additionally, data from EndNote can be imported via EndNote XML file. No other XML file formats are permitted.
#'
#'
#' @import RefManageR
#' @import dplyr
#' @import parallel
#' @import bibliometrix
#' @importFrom utils read.csv read.table
#' @param paths a character vector of file paths to the manual search result files
#' @param source the source database for the search results. Choices: "scopus", "embase", "psychinfo", "medline", "wos", "pubmed", or "endnote")
#'
#' @return a dataframe containing the search results from the specified source, formatted for SOLES
#'
#' @export
manual_upload <- function(paths, source) {
  # List all supported database sources
  sources <- c("scopus", "embase", "psychinfo", "medline", "wos", "pubmed", "endnote")
  
  # List the corresponding function names to process data from each source
  manual_process_functions <- list(
    scopus = process_scopus,
    embase = process_embase,
    psychinfo = process_psychinfo,
    medline = process_medline,
    wos = process_wos,
    pubmed = process_pubmed,
    endnote = process_endnote
  )
  
  # Check if source is supported
  if (!(source %in% sources)) {
    stop(message("Error: source type not supported. Supported options are: 'scopus', 'embase', 'psychinfo', 'medline', 'wos', 'pubmed' or 'endnote'."))
  }
  
  # Define function to handle manual file processing logic
  process_manual_upload <- function(process_function) {
    # Check operating system and apply function
    if (.Platform$OS.type == "unix") {
      # Run in parallel for Unix OS
      processed_results <- parallel::mclapply(paths,
                                              process_function,
                                              mc.cores = parallelly::availableCores()
      )
    } else {
      # Run sequentially for other OS
      processed_results <- lapply(paths, process_function)
    }
    
    # Combine results into one dataframe
    do.call(rbind, processed_results)
  }
  
  # Call the processing function based on the data source
  combined_data <- process_manual_upload(manual_process_functions[[source]])
  
  # Change no abstract available to NA
  combined_data$abstract <- gsub("^\\[No abstract available\\]$", "", combined_data$abstract)
  # Change all "NA" to real NA
  combined_data[combined_data == "NA"] <- NA
  # Change all blanks to NA
  combined_data[combined_data == ""] <- NA
  
  # Make DOI lowercase
  combined_data$doi <- tolower(combined_data$doi)
  
  # Remove any additional DOIs (e.g., elife versioning)
  combined_data$doi <- gsub("; .+$", "", combined_data$doi)
  
  # Return data
  return(combined_data)
}

#' Format Manual Upload Search Data Columns for SOLES
#'
#' Internal function used to process manual uploads used to format columns so they can be combined in SOLES workflows.
#'
#' @import dplyr
#'
#' @param df a manual upload file dataframe to be correctly formatted for SOLES
#'
#' @return a formatted dataframe containing the required SOLES columns with standardized case
#'
format_cols <- function(df) {
  # Create vector with columns required for SOLES
  x <- c(
    "uid", "author", "year", "journal", "doi", "title",
    "pages", "volume", "abstract", "isbn", "keywords",
    "secondarytitle", "url", "date", "issn", "pmid", "ptype",
    "source", "author_country", "number", "author_affiliation"
  )
  
  # Subset columns that require title case
  title_case_cols <- c("author", "journal", "secondarytitle", "author_country", "author_affiliation")
  
  
  # Subset columnns that require lower case
  lower_case_cols <- c(
    "uid", "doi", "keywords", "ptype",
    "source"
  )
  
  # If the column does not exist in upload file, fill in missing data with NA
  df[x[!(x %in% colnames(df))]] <- NA
  
  # Format correct letter case
  df <- df %>%
    select(all_of(x)) %>%
    mutate(across(all_of(lower_case_cols), ~ stringr::str_to_lower(.))) %>%
    mutate(across(all_of(title_case_cols), ~ stringr::str_to_title(.))) %>%
    # Add additional space after semi-colon
    mutate(across(all_of(x), ~ gsub(";", "; ", .)))
  
  # Replace double hyphens with single hyphen in pages column
  df$pages <- lapply(df$pages, function(x) gsub("--", "-", x))
  
  # Set date as today's date
  df$date <- format(Sys.Date(), "%d%m%y")
  
  # Remove additional line breaks and carriage returns
  cols_to_modify <- c("title", "year", "journal", "abstract", "doi", "number", "pages", "volume", "isbn", "issn")
  df[cols_to_modify] <- lapply(df[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))
  
  # Return correctly formatted dataframe
  return(df)
}

#' Process Manually Uploaded Search results from Embase
#'
#' Internal function used to process Embase search results an format for SOLES.
#'
#' @param path the file path to the search results
#'
#' @return a processed dataframe containing search results
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
process_embase <- function(path) {
  # Extract file extension
  file_extension <- tools::file_ext(path)
  
  # Check file extension
  if (!file_extension %in% c("ris", "bib", "txt")) {
    stop(message("Error: File type not supported"))
  }
  
  # Read in data using synthesisr package
  newdat <- synthesisr::read_refs(path, tag_naming = "ovid")
  
  # Rename columns for SOLES
  newdat$number <- newdat$issue
  newdat$author_affiliation <- newdat$address
  
  # If book, add booktitle to title column
  if ("booktitle" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(title, booktitle, na.rm = TRUE)
  }
  
  # If has both start and end page, combine into one column
  if ("start_page" %in% colnames(newdat) &
      "end_page" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(pages, start_page, end_page, sep = "-", na.rm = TRUE)
  } else {
    # Else take start page only
    newdat <- newdat %>%
      mutate(pages = start_page)
  }
  
  # Set date as today
  newdat$date <- format(Sys.Date(), "%d%m%y")
  
  # If no article identifer column, set as NA
  if (!"article_id" %in% colnames(newdat)) {
    newdat <- newdat %>%
      mutate(article_id = NA)
  }
  
  # Create unique article identifier for each record
  newdat <- newdat %>%
    mutate(pmid = ifelse(!is.na(article_id), gsub("\\[.*", "", article_id), "")) %>%
    mutate(uid = paste0("embase-", AN))
  
  # If publication type column exists, rename it for SOLES
  if ("PT" %in% colnames(newdat)) {
    newdat$ptype <- newdat$PT
  }
  
  # Set source as embase
  newdat$source <- "embase"
  
  # Run format DOI function to ensure DOIs are consistently formatted
  newdat <- format_doi(newdat)
  
  # Run format columns function to ensure columns are consistent and compatible for SOLES
  newdat <- format_cols(newdat)
  
  # Return formatted dataframe
  return(newdat)
}


#' Process Manually Uploaded Search results from PsychINFO
#'
#' Internal function used to process PsychINFO search results an format for SOLES.
#'
#' @param path the file path to the search results
#'
#' @return a processed dataframe containing search results
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
process_psychinfo <- function(path) {
  # Extract file extension
  file_extension <- tools::file_ext(path)
  
  # Check file extension
  if (!file_extension %in% c("ris", "bib", "txt")) {
    stop(message("Error: File type not supported"))
  }
  
  # Read in data using synthesir package
  newdat <- synthesisr::read_refs(path, tag_naming = "ovid")
  
  # Rename columns for SOLES
  newdat$number <- newdat$issue
  newdat$ptype <- newdat$PT
  newdat$url <- newdat$L2
  newdat$author_affiliation <- newdat$M2
  
  # If book, use booktitle as title
  if ("booktitle" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(title, booktitle, na.rm = TRUE)
  }
  
  # If has both start and end page, combine into one column
  if ("start_page" %in% colnames(newdat) &
      "end_page" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(pages, start_page, end_page, sep = "-", na.rm = TRUE)
  } else {
    # Else take start page only
    newdat <- newdat %>%
      mutate(pages = start_page)
  }
  
  # If no pages given, set as NA
  newdat <- newdat %>%
    mutate(pages = ifelse(pages == "No-Specified", NA_character_, paste(pages)))
  
  # Set date as tday
  newdat$date <- format(Sys.Date(), "%d%m%y")
  
  # If no article identifier, set as today
  if (!"article_id" %in% colnames(newdat)) {
    newdat <- newdat %>%
      mutate(article_id = NA)
  }
  
  # Create identifer columns
  newdat <- newdat %>%
    mutate(pmid = "") %>%
    mutate(uid = paste0("psychinfo-", article_id))
  
  # Set source
  newdat$source <- "psychinfo"
  
  # Run format DOI function to ensure DOIs are consistently formatted
  newdat <- format_doi(newdat)
  
  # Run format columns function to ensure columns are consistent and compatible for SOLES
  newdat <- format_cols(newdat)
  
  # Return formatted dataframe
  return(newdat)
}

#' Process Manually Uploaded Search results from MEDLINE
#'
#' Internal function used to process MEDLINE search results an format for SOLES.
#'
#' @param path the file path to the search results
#'
#' @return a processed dataframe containing search results
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
process_medline <- function(path) {
  # Extract file extension
  file_extension <- tools::file_ext(path)
  
  # Check file extension
  if (!file_extension %in% c("ris", "bib", "txt")) {
    stop(message("Error: File type not supported"))
  }
  
  # Read in data using synthesisr
  newdat <- synthesisr::read_refs(path, tag_naming = "ovid")
  
  # Rename columns for SOLES
  newdat$number <- newdat$issue
  newdat$ptype <- newdat$PT
  newdat$url <- newdat$L2
  newdat$author_affiliation <- newdat$M2
  
  # If book, use booktitle as title
  if ("booktitle" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(title, booktitle, na.rm = TRUE)
  }
  
  # If has both start and end page, combine both
  if ("start_page" %in% colnames(newdat) &
      "end_page" %in% colnames(newdat)) {
    newdat <- newdat %>%
      tidyr::unite(pages, start_page, end_page, sep = "-", na.rm = TRUE)
  } else {
    # Else take start page only
    newdat <- newdat %>%
      mutate(pages = start_page)
  }
  
  # Set dat as today
  newdat$date <- format(Sys.Date(), "%d%m%y")
  
  # If no article identifier, set as NA
  if (!"article_id" %in% colnames(newdat)) {
    newdat <- newdat %>%
      mutate(article_id = NA)
  }
  
  # Assign identifiers
  newdat <- newdat %>%
    mutate(pmid = article_id) %>%
    mutate(uid = paste0("medline-", article_id))
  
  # Set source
  newdat$source <- "medline"
  
  # Run format DOI function to ensure DOIs are consistently formatted
  newdat <- format_doi(newdat)
  
  # Run format columns function to ensure columns are consistent and compatible for SOLES
  newdat <- format_cols(newdat)
  
  # Return formatted dataframe
  return(newdat)
}

#' Process Manually Uploaded Search results from Web of Science
#'
#' Internal function used to process Web of Science search results an format for SOLES.
#'
#' @param path the file path to the search results
#'
#' @return a processed dataframe containing search results
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
#' @import ASySD
#' @import dplyr
process_wos <- function(path) {
  # Extract file extension
  file_extension <- tools::file_ext(path)
  
  # Check file extension
  if (file_extension %in% c("ris")) {
    # Extract data
    newdat <- synthesisr::read_refs(path, tag_naming = "ovid")
    
    # If start and end page give, combine both
    if ("start_page" %in% colnames(newdat) &
        "end_page" %in% colnames(newdat)) {
      newdat <- newdat %>%
        tidyr::unite(pages, start_page, end_page, sep = "-", na.rm = TRUE)
    } else {
      # Else use start page only
      newdat <- newdat %>%
        mutate(pages = start_page)
    }
    
    # If secondary title not given, create and set to NA
    if (!"secondarytitle" %in% colnames(newdat)) {
      newdat <- newdat %>%
        mutate(secondarytitle = NA)
    }
    
    # If ISBN not given, create and set to NA
    if (!"isbn" %in% colnames(newdat)) {
      newdat <- newdat %>%
        mutate(isbn = NA)
    }
    
    # If PMID not given, create and set to NA
    if (!"pmid" %in% colnames(newdat)) {
      newdat <- newdat %>%
        mutate(pmid = NA)
    }
    
    # If author country not given, create and set to NA
    if (!"author_country" %in% colnames(newdat)) {
      newdat <- newdat %>%
        mutate(author_country = NA)
    }
    
    # Set source
    newdat$source <- "wos"
    
    # Set date to today
    newdat$date <- format(Sys.Date(), "%d%m%y")
    
    # Create url
    newdat$url <- paste0("https://www.webofscience.com/wos/woscc/full-record/", newdat$uid)
    
    # Select correctly named column names for SOLES
    newdat <- newdat %>%
      select(
        uid = AN, author = AU, year = PY, journal = T2,
        doi, title = TI, pages, volume,
        abstract = AB, isbn, keywords, secondarytitle,
        url, date, issn, pmid, ptype = source_type,
        source, number = issue, author_country,
        author_affiliation = PA
      )
    
    # Run format DOI function to ensure DOIs are consistently formatted
    newdat <- format_doi(newdat)
    
    # Run format columns function to ensure columns are consistent and compatible for SOLES
    newdat <- format_cols(newdat)
    
  } else if (file_extension %in% c("bib")) {
    
    # read in 
    newdat <- bibliometrix::convert2df(path, dbsource = "wos", format="bibtex")

    # Use lookup tables for naming
    utils::data("field_codes", package = "solesR", envir = environment())
    lookup_table <- setNames(field_codes_wos$Field, field_codes_wos$Abbreviation)
    colnames(newdat) <- lookup_table[colnames(newdat)]
    
    # Remove columns that are blank
    keep.cols <- names(newdat) %in% NA
    newdat <- newdat[!keep.cols]
    rownames(newdat) <- 1:nrow(newdat)
    
    # Set source
    newdat$source <- "wos"
    
    # Create unique identifier
    newdat$uid <- gsub("WOS:", "wos:", newdat$uid)
    newdat$uid <- lapply(newdat$uid, function(x) gsub("WOS", "wos:", x))
    
    # Format author country
    newdat$author_country <- stringr::str_extract(newdat$author_country, "\\b(\\w+)\\b$")
    newdat$author_country  <- tools::toTitleCase(newdat$author_country)
    
    # Rename publication type column
    newdat$ptype <- newdat$article_type
    
    # Run format DOI function to ensure DOIs are consistently formatted
    newdat <- format_doi(newdat)
    
    # Run format columns function to ensure columns are consistent and compatible for SOLES
    newdat <- format_cols(newdat)
  } else {
    stop(message("Error: File type not supported"))
  }
  
  # Return processed dataframe
  return(newdat)
}

#' Process Manually Uploaded Search results from PubMed
#'
#' Internal function used to process PubMed search results an format for SOLES.
#'
#' @param path the file path to the search results
#'
#' @return a processed dataframe containing search results
#'
#' @details
#' This function reads references from the .nbib pubmed export (send to > citation manager > file.nbib) OR the PubMed text export (save > PubMed -> file.txt) You can access this by selecting "send to reference manager" for up to 10,000 records.  
#' It adjusts column names according to the field codes, removes blank columns, adds source information, 
#' creates a unique identifier (uid) based on the pmid, sets the data source to "pubmed", and 
#' formats the doi column based on available information.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' pubmed_data <- process_pubmed("path/to/pubmed_data.bib")
#' pubmed_data <- process_pubmed("path/to/pubmed_data.txt")
#' }
#'
#' @importFrom bibliometrix convert2df
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @import dplyr
process_pubmed <- function(path){
  
  # try wos format
  newdat <- bibliometrix::convert2df(path, dbsource = "pubmed", format="pubmed")
  
  # use lookup tables for naming
  utils::data("field_codes", package = "solesR", envir = environment())
  lookup_table <- setNames(field_codes_pubmed$Field, field_codes_pubmed$Abbreviation)
  colnames(newdat) <- lookup_table[colnames(newdat)]
  
  # remove columns that are blank
  keep.cols <- names(newdat) %in% NA
  newdat <- newdat [! keep.cols]
  rownames(newdat) <- 1:nrow(newdat)
  
  newdat$source <- "pubmed"
  colnames(newdat) <- gsub("^keywords_plus.*$", "keywords_plus", colnames(newdat))
  newdat$ptype <- newdat$document_type
  
  # DOI repair: keep valid DOI; otherwise try extracting from known fields (txt/nbib)
  doi_valid <- "^10\\.\\d{4,9}/\\S+"
  
  newdat <- newdat %>%
    dplyr::mutate(
      uid  = paste0("pubmed-", pmid),
      
      # keep doi only if it already looks valid; otherwise set NA so coalesce can fill it
      doi = dplyr::if_else(
        stringr::str_detect(doi, doi_valid),
        doi,
        NA_character_
      ),
      
      # fill from the first available extraction
      doi = dplyr::coalesce(
        doi,
        stringr::str_extract(article_ids, "\\b10\\.\\d{4,}/\\S+(?=\\s\\[DOI\\])"),
        stringr::str_extract(local_id,   "\\b10\\.\\d{4,}/\\S+(?=\\s\\[DOI\\])"),
        stringr::str_extract(source_2,   "(?<=DOI:\\s)10\\.\\d{4,}/\\S+")
      )
    )
  
  # Run format DOI function to ensure DOIs are consistently formatted
  newdat <- format_doi(newdat)
  
  # Run format columns function to ensure columns are consistent and compatible for SOLES
  newdat <- format_cols(newdat)
  
  
  return(newdat)
}

#' Process Manually Downloaded Scopus Search Results
#'
#' Internal function used to import and standardise Scopus search results
#' for use within SOLES.
#'
#' @param path Character scalar. File path to a Scopus CSV export.
#'
#' @return A processed data.frame containing harmonised Scopus search results.
#'
#' @details
#' This function reads a Scopus CSV export using
#' \code{bibliometrix::convert2df()} and converts it into the standardised
#' SOLES structure.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' scopus_data <- process_scopus("path/to/scopus_data.csv")
#' }
#'
#' @importFrom tidyr unite
#' @import dplyr
process_scopus <- function(path) {
  
  # Extract file extension
  file_extension <- tools::file_ext(path)
  
  # # Check file extension
  if (!file_extension %in% c("csv")) {
    stop(message("Error: File type not supported, csv required from scopus"))
  }
  
  newdat <- bibliometrix::convert2df(path, dbsource = "scopus", format="csv")
  
  # Bring in data from lookup tables
  utils::data("field_codes", package = "solesR", envir = environment())
  lookup_table <- setNames(field_codes_scopus$Field, field_codes_scopus$Abbreviation)
  colnames(newdat) <- lookup_table[colnames(newdat)]
  
  # remove columns that are blank
  keep.cols <- names(newdat) %in% NA
  newdat <- newdat [! keep.cols]
  rownames(newdat) <- 1:nrow(newdat)
  
  # tidy columns
  newdat$source <- "scopus"
  colnames(newdat) <- gsub("^keywords_plus.*$", "keywords_plus", colnames(newdat))
  newdat$ptype <- newdat$document_type
  
  # create pages column
  if (all(c("start_page", "end_page") %in% colnames(newdat))) {
    
    newdat <- newdat %>%
      tidyr::unite(pages, start_page, end_page, sep = "-", na.rm = TRUE) %>%
      mutate(
        pages = trimws(pages),
        pages = na_if(pages, ""),
        pages = na_if(pages, "-")
      )
    
  } else if ("start_page" %in% colnames(newdat)) {
    
    newdat <- newdat %>%
      mutate(
        pages = na_if(trimws(start_page), "")
      )
  }
  
  # create scopus uid
  pattern <- "eid=(2-s2\\.0-\\d+)"
  newdat <- newdat %>%
    mutate(
      uid = dplyr::case_when(
        !is.na(accession_number) & stringr::str_trim(accession_number) != "" ~ accession_number,
        !is.na(link)      & stringr::str_trim(link)      != "" ~ stringr::str_extract(link, pattern),
        TRUE ~ paste0("unknown-accession-", floor(runif(n(), 100, 10000000)))
      ),
      uid = gsub("eid=", "", uid),
      uid = paste0("scopus-", uid)
    )
  
  # Run format DOI function to ensure DOIs are consistently formatted
  newdat <- format_doi(newdat)
  
  # Run format columns function to ensure columns are consistent and compatible for SOLES
  newdat <- format_cols(newdat)
  
  # add NAs in empty cells
  newdat <- newdat %>% 
    mutate(across(
      where(is.character),
      ~ na_if(trimws(.), "")
    ))
  
  # Return processed dataframe
  return(newdat)
}

#' Process Manually Uploaded Search results from Endnote XML file
#'
#' Internal function used to process EndNote exports as XML an format for SOLES.
#'
#' @param path the file path to the search results
#'
#' @return a processed dataframe containing search results
#' @import XML
#' @import dplyr
#' @export
process_endnote <- function(path) {
  # Parse the XML file
  newdat <- XML::xmlParse(path)
  
  # Get the record node
  x <- XML::getNodeSet(newdat, "//record")
  
  # Define a function for getting required data
  xpath2 <- function(x, ...) {
    y <- XML::xpathSApply(x, ...)
    y <- gsub(",", "", y)
    ifelse(length(y) == 0, NA, paste(y, collapse = ", "))
  }
  
  # Get required data from EndNote XML
  newdat <- data.frame(
    author = sapply(x, xpath2, ".//author", xmlValue),
    year = sapply(x, xpath2, ".//dates/year", xmlValue),
    journal = sapply(x, xpath2, ".//periodical/full-title", xmlValue),
    doi = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
    title = sapply(x, xpath2, ".//titles/title", xmlValue),
    pages = sapply(x, xpath2, ".//pages", xmlValue),
    volume = sapply(x, xpath2, ".//volume", xmlValue),
    number = sapply(x, xpath2, ".//number", xmlValue),
    abstract = sapply(x, xpath2, ".//abstract", xmlValue),
    keywords = sapply(x, xpath2, ".//keywords/keyword", xmlValue),
    record_id = sapply(x, xpath2, ".//rec-number", xmlValue),
    isbn = sapply(x, xpath2, ".//isbn", xmlValue),
    secondary_title = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
    pmid = sapply(x, xpath2, ".//custom2", xmlValue),
    label = sapply(x, xpath2, ".//label", xmlValue),
    database = sapply(x, xpath2, ".//remote-database-name", xmlValue),
    accession = sapply(x, xpath2, ".//accession-num", xmlValue),
    url = sapply(x, xpath2, ".//url", xmlValue)
  )
  
  # Set pattern for Scopus identifiers
  pattern <- "eid=(2-s2\\.0-\\d+)"
  
  # Set source for each record depending on origin
  newdat <- newdat %>%
    mutate(source = case_when(
      grepl("scopus", url, ignore.case = TRUE) ~ "scopus",
      grepl("Embase", database, ignore.case = TRUE) ~ "embase",
      grepl("pubmed", database, ignore.case = TRUE) ~ "pubmed",
      grepl("BIOSIS", url, ignore.case = TRUE) ~ "biosis",
      grepl("BCI:", url, ignore.case = TRUE) ~ "biosis",
      grepl("wos", accession, ignore.case = TRUE) ~ "wos",
      grepl("medline", database, ignore.case = TRUE) ~ "medline",
      TRUE ~ "unknown" # Keep the original source if none of the conditions match
    )) %>%
    ungroup()
  
  # Set identifiers for each second depending on origin
  newdat <- newdat %>%
    rowwise() %>%
    mutate(accession = ifelse(source %in% "scopus", regmatches(url, regexpr(pattern, url)), accession)) %>%
    ungroup() %>%
    mutate(accession = ifelse(is.na(accession), paste0("noid:", 1000 + row_number()), accession)) %>%
    mutate(accession = gsub("eid=", "", accession)) %>%
    mutate(accession = gsub("WOS:", "", accession)) %>%
    mutate(uid = paste0(source, "-", accession)) %>%
    mutate(uid = gsub("unknown-noid:", "unknown-", uid))
  
  # Run format DOI function to ensure DOIs are consistently formatted
  newdat <- format_doi(newdat)
  
  # Run format columns function to ensure columns are consistent and compatible for SOLES
  newdat <- format_cols(newdat)
  
  # Return processed dataframe
  return(newdat)
}
