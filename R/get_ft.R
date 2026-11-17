#' Retrieve full text documents
#'
#' Retrieve full texts as PDF or XML for SOLES.
#'
#' @param con Database connection
#' @param doi_df A dataframe containing a DOI column
#' @param pdf_source where to retrieve PDFs from, options = c("unpaywall", "crossref", "wiley") OR NULL
#' @param xml_source where to retrieve XMLs from, options = c("epmc", "elsevier") OR NULL
#' @param ft_path folder where full texts should be downloaded to
#' @param retMax maximum number of full texts to attempt to retrieve; max = 500; default = 500
#' @param check_failed logical, whether to check DOIs that previously failed; default = FALSE
#' @param timespan timespan for check_failed in format digit(day/week/month) e.g. "2month"
#' @param wiley_token API token required to retrieve from Wiley
#' @param elsevier_token API token required to retrieve from Elsevier
#' @param unpaywall_email Your email, required to retrieve from Unpaywall
#'
#' @return summary of retrieval written to database
#' @export
#'
#' @import DBI
#' @import dplyr
#'
get_ft <- function(con = NULL, doi_df = NULL, pdf_source = c("unpaywall", "crossref", "wiley"), xml_source = c("epmc", "elsevier"), ft_path, retMax = 500, check_failed = FALSE, timespan = NULL, wiley_token = NULL, elsevier_token = NULL, unpaywall_email = NULL){
  
  # Column order used for every summary written to the database / log
  ft_cols <- c("status", "doi", "doi_encoded", "method", "path", "ft_ext", "date")
  
  # Check DOI source, from database or input df containing doi column
  if (!is.null(con)) {
    
    if (!inherits(con, "DBIConnection")) {
      stop("'con' is not a valid database connection")
    }
    
  } else {
    
    if (is.null(doi_df)) {
      stop("If 'con' is NULL, you must provide 'doi_df'")
    }
    
    if (!is.data.frame(doi_df)) {
      stop("'doi_df' must be a data.frame")
    }
    
    # Standardise column names
    doi_df <- janitor::clean_names(doi_df)
    
    if (!"doi" %in% colnames(doi_df)) {
      stop("'doi_df' must contain a 'doi' column")
    }
  }
  
  # Check pdf_source options are valid or NULL
  if(!is.null(pdf_source)){
    if (!(all(pdf_source %in% c("unpaywall", "crossref", "wiley")))) {
      stop("'pdf_source' is invalid, options are: 'unpaywall', 'crossref', 'wiley' or NULL")
    }
  }
  
  # Check xml_source options are valid or NULL
  if(!is.null(xml_source)){
    if (!(all(xml_source %in% c("epmc", "elsevier")))) {
      stop("'xml_source' is invalid, options are: 'epmc', 'elsevier' or NULL")
    }
  }
  
  # Check tokens and emails exist if required
  if("unpaywall" %in% pdf_source && is.null(unpaywall_email)){
    stop("Cannot retrieve using unpaywall if unpaywall_email argument is missing")
  }
  if("wiley" %in% pdf_source && is.null(wiley_token)){
    stop("Cannot retrieve using wiley if wiley_token argument is missing")
  }
  if("elsevier" %in% xml_source && is.null(elsevier_token)){
    stop("Cannot retrieve using elsevier if elsevier_token argument is missing")
  }
  
  # If ft_path does not exist, create it
  if(!dir.exists(ft_path)){
    dir.create(ft_path, recursive = TRUE)
    message("file path created for full texts: ", ft_path)
  }
  
  # Check if retMax is a positive whole number and exit if not
  if (!is.numeric(retMax) || length(retMax) != 1 || retMax %% 1 != 0 || retMax < 1) {
    stop("retMax must be a positive whole number")
  }
  
  # Check retMax and exit if above maximum
  if (retMax > 500) {
    stop("retMax is too high, max is 500")
  }
  
  # Check check_failed is Boolean
  if (!is.logical(check_failed) || length(check_failed) != 1 || is.na(check_failed)) {
    stop("check_failed argument should be set to TRUE or FALSE, default is FALSE")
  }
  
  # If check failed is used
  if(isTRUE(check_failed)){
    # Check timespan is given
    if(is.null(timespan)){
      stop("To check failed, use the timespan argument, e.g. timespan = '2month'")
    }
    # Check timespan is valid
    if (grepl("^(?i)\\d+(day|week|month)$", timespan, perl = TRUE) == FALSE) {
      stop("timespan format incorrect, should contain digit and day/week/month, e.g. `3day`")
    }
    # Get number of days/weeks/months by removing non-digit characters
    x <- as.numeric(gsub("\\D", "", timespan))
    # Define timespan for retrieval
    if (grepl("(?i)week", timespan, perl = TRUE)) {
      # Assign date as x number of weeks before today's date
      retrieval_date <- Sys.Date() - 7 * x
    } else if (grepl("(?i)month", timespan, perl = TRUE)) {
      # Assign date as x number of months before today's date
      retrieval_date <- Sys.Date() - 31 * x
    } else {
      # Assign date as x number of days before today's date
      retrieval_date <- Sys.Date() - x
    }
    message("Rechecking DOIs that failed before ", retrieval_date)
  }
  
  # Keep DOIs that were found, or that failed recently (so not rechecked yet)
  filter_check_failed <- function(ft_found){
    
    if (!"date" %in% colnames(ft_found)) {
      stop("full_texts record has no 'date' column; run update_full_texts_tbl() first to use check_failed")
    }
    
    ft_found %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(status == "found" | (!is.na(date) & date > retrieval_date))
  }
  
  # Using SOLES database
  if (!is.null(con)) {
    
    # If full_texts table exists, read it in
    if (DBI::dbExistsTable(con, "full_texts")){
      
      ft_found <- DBI::dbReadTable(con, "full_texts")
      
      if(isTRUE(check_failed)){
        # get studies which have been found or checked after retrieval date
        ft_found <- filter_check_failed(ft_found)
      }
      
    } else{
      
      # Create empty dataframe
      ft_found <- data.frame(doi = as.character())
    }
    
    # Read in DOI data
    dois <- dplyr::tbl(con, "unique_citations") %>%
      dplyr::select(doi, uid) %>%
      dplyr::left_join(dplyr::tbl(con, "study_classification"), by = "uid") %>%
      dplyr::filter(decision == "include",
                    !is.na(doi),
                    !doi == "") %>%
      dplyr::select(doi) %>%
      dplyr::distinct() %>%
      dplyr::collect()
    
    # Filter not found
    dois <- dois %>%
      dplyr::filter(!doi %in% ft_found$doi)
    
  } else {
    
    # Local log for non-database use
    ft_log_path <- file.path(ft_path, "full_texts_log.csv")
    
    if (file.exists(ft_log_path)) {
      
      ft_found <- utils::read.csv(ft_log_path, stringsAsFactors = FALSE)
      
    } else {
      
      ft_found <- data.frame(doi = as.character())
    }
    
    if (isTRUE(check_failed) && nrow(ft_found) > 0) {
      # get studies which have been found or checked after retrieval date
      ft_found <- filter_check_failed(ft_found)
    }
    
    # Use DOI dataframe provided by user
    dois <- doi_df %>%
      dplyr::select(doi) %>%
      dplyr::filter(!is.na(doi), !doi == "") %>%
      dplyr::distinct() %>%
      dplyr::filter(!doi %in% ft_found$doi)
    
  }
  
  
  # Get max number of dois to retrieve full texts for
  if(nrow(dois) < 1){
    message("No more full texts to retrieve!")
    return(invisible(NULL))
  } else if(nrow(dois) > retMax){
    message(nrow(dois), " full texts to retrieve; limiting to first ", retMax)
    dois <- head(dois, retMax)
  } else{
    message(nrow(dois), " full texts to retrieve")
  }
  
  # Create overall results
  ft_summary_all <- NULL
  
  # Get list of dois to retrieve
  dois_pdf <- dois$doi
  
  # If pdf_source is not null, retrieve PDFs
  if(!is.null(pdf_source)){
    
    # Create empty results
    pdf_summary <- NULL
    unpaywall_summary <- NULL
    
    
    # retrieve using unpaywall
    if("unpaywall" %in% pdf_source && length(dois_pdf) > 0){
      message("Trying unpaywall...")
      for(i in seq_along(dois_pdf)){
        message("Trying DOI ", i, " of ", length(dois_pdf), " in Unpaywall")
        pdf_summary_new <- ft_unpaywall(dois_pdf[i], unpaywall_email = unpaywall_email, ft_path = ft_path, ft_name_style = "doi")
        
        # Add to summary
        unpaywall_summary <- rbind(unpaywall_summary, pdf_summary_new)
        
        
        Sys.sleep(1)
      }
      
      # Remove not found from summary
      unpaywall_summary <- unpaywall_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(path))
      
      # Add to pdf summary
      pdf_summary <- rbind(pdf_summary, unpaywall_summary)
      
      message(nrow(unpaywall_summary), " files found from Unpaywall")
      
      # Remove from doi list where file found
      dois_pdf <- dois_pdf[!dois_pdf %in% unpaywall_summary$doi]
      message("Finished Unpaywall: ", length(dois_pdf), " full texts left to find")
    }
    
    crossref_summary <- NULL
    
    # retrieve using crossref
    if("crossref" %in% pdf_source && length(dois_pdf) > 0){
      message("Trying Crossref...")
      for(i in seq_along(dois_pdf)){
        message("Trying DOI ", i, " of ", length(dois_pdf), " in Crossref")
        
        pdf_summary_new <- ft_crossref(dois_pdf[i], ft_path = ft_path, ft_name_style = "doi")
        
        # Add to summary
        crossref_summary <- rbind(crossref_summary, pdf_summary_new)
        
        Sys.sleep(1)
      }
      
      # Remove not found from summary
      crossref_summary <- crossref_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(path))
      
      message(nrow(crossref_summary), " files found from Crossref")
      
      # Add to pdf summary
      pdf_summary <- rbind(pdf_summary, crossref_summary)
      
      # Remove from doi list where file found
      dois_pdf <- dois_pdf[!dois_pdf %in% crossref_summary$doi]
      message("Finished Crossref: ", length(dois_pdf), " full texts left to find")
    }
    
    wiley_summary <- NULL
    
    # retrieve using wiley
    if("wiley" %in% pdf_source && length(dois_pdf) > 0){
      message("Trying Wiley...")
      for(i in seq_along(dois_pdf)){
        message("Trying DOI ", i, " of ", length(dois_pdf), " in Wiley")
        
        pdf_summary_new <- ft_wiley(dois_pdf[i], wiley_token = wiley_token, ft_path = ft_path, ft_name_style = "doi")
        
        # Add to summary
        wiley_summary <- rbind(wiley_summary, pdf_summary_new)
        
        Sys.sleep(1)
      }
      # Remove not found from summary
      wiley_summary <- wiley_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(path))
      
      # Add to pdf summary
      pdf_summary <- rbind(pdf_summary, wiley_summary)
      
      
      message(nrow(wiley_summary), " files found from Wiley")
      
      # Remove from doi list where file found
      dois_pdf <- dois_pdf[!dois_pdf %in% wiley_summary$doi]
      message("Finished Wiley: ", length(dois_pdf), " full texts left to find")
    }
    
    # Add to overall results
    ft_summary_all <- rbind(ft_summary_all, pdf_summary)
  }
  
  # If xml_source is not null, retrieve XMLs
  if(!is.null(xml_source)){
    
    # Only try DOIs with no PDF found
    dois_xml <- dois_pdf
    
    # Create empty results
    xml_summary <- NULL
    epmc_summary <- NULL
    
    # retrieve using epmc
    if("epmc" %in% xml_source && length(dois_xml) > 0){
      message("Trying Europe PubMed Central...")
      for(i in seq_along(dois_xml)){
        message("Trying DOI ", i, " of ", length(dois_xml), " in Europe PubMed Central")
        xml_summary_new <- ft_epmc(dois_xml[i], ft_path = ft_path, ft_name_style = "doi")
        
        # Add to summary
        epmc_summary <- rbind(epmc_summary, xml_summary_new)
        
        Sys.sleep(1)
      }
      
      # Remove not found from summary
      epmc_summary <- epmc_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(path))
      
      # Add to xml summary
      xml_summary <- rbind(xml_summary, epmc_summary)
      
      message(nrow(epmc_summary), " files found from Europe PubMed Central")
      
      dois_xml <- dois_xml[!dois_xml %in% epmc_summary$doi]
      message("Finished Europe PubMed Central: ", length(dois_xml), " full texts left to find")
    }
    
    elsevier_summary <- NULL
    
    # retrieve using elsevier
    if("elsevier" %in% xml_source && length(dois_xml) > 0){
      message("Trying Elsevier...")
      for(i in seq_along(dois_xml)){
        message("Trying DOI ", i, " of ", length(dois_xml), " in Elsevier")
        xml_summary_new <- ft_elsevier(dois_xml[i], elsevier_token = elsevier_token, ft_path = ft_path, ft_name_style = "doi")
        
        # Add to summary
        elsevier_summary <- rbind(elsevier_summary, xml_summary_new)
        
        Sys.sleep(1)
      }
      # Remove not found from summary
      elsevier_summary <- elsevier_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(path))
      
      # Add to xml summary
      xml_summary <- rbind(xml_summary, elsevier_summary)
      
      message(nrow(elsevier_summary), " files found from Elsevier")
      
      # Remove from doi list where file found
      dois_xml <- dois_xml[!dois_xml %in% elsevier_summary$doi]
      message("Finished Elsevier: ", length(dois_xml), " full texts left to find")
    }
    
    # Add to overall results
    ft_summary_all <- rbind(ft_summary_all, xml_summary)
    
  }
  
  
  # if results still null, no files found
  if(is.null(ft_summary_all) || nrow(ft_summary_all) == 0){
    message("no full texts found")
  }
  
  # Summarise "failed" studies
  ft_summary_not_found <- dois %>%
    dplyr::filter(!doi %in% ft_summary_all$doi) %>%
    dplyr::mutate(status = "failed",
                  doi_encoded = vapply(doi, utils::URLencode, character(1), reserved = TRUE, USE.NAMES = FALSE),
                  method = NA_character_,
                  path = NA_character_,
                  ft_ext = NA_character_,
                  date = Sys.Date())
  
  # Combine, with a fixed column order
  ft_summary_all <- dplyr::bind_rows(ft_summary_all, ft_summary_not_found) %>%
    dplyr::select(dplyr::all_of(ft_cols))
  
  
  if (!is.null(con)) {
    
    # Read existing table, if present
    if (DBI::dbExistsTable(con, "full_texts")) {
      
      ft_existing <- DBI::dbReadTable(con, "full_texts") %>%
        dplyr::mutate(date = as.Date(date))
      
    } else {
      
      ft_existing <- NULL
    }
    
    # Combine existing + new results
    ft_all <- dplyr::bind_rows(
      ft_existing,
      ft_summary_all
    )
    
    # Keep all found rows
    found_rows <- ft_all %>%
      dplyr::filter(status == "found") %>%
      dplyr::distinct()
    
    # Keep only the latest failed per DOI, and only for DOIs never found
    failed_rows <- ft_all %>%
      dplyr::filter(status == "failed",
                    !doi %in% found_rows$doi) %>%
      dplyr::arrange(doi, dplyr::desc(date)) %>%
      dplyr::group_by(doi) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    
    # Combine back
    ft_all <- dplyr::bind_rows(found_rows, failed_rows)
    
    # Write cleaned table once
    DBI::dbWriteTable(con, "full_texts", ft_all, overwrite = TRUE)
    
  } else {
    
    # No db connection - write results to full_texts_log file
    ft_log_path <- file.path(ft_path, "full_texts_log.csv")
    
    if (file.exists(ft_log_path)) {
      
      # Append results to existing log file
      utils::write.table(
        ft_summary_all,
        ft_log_path,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE
      )
    } else {
      
      # Create new log file
      utils::write.csv(
        ft_summary_all,
        ft_log_path,
        row.names = FALSE
      )
    }
    
    message("Retrieval log written to: ", ft_log_path)
  }
  
  return(ft_summary_all)
  
}


#' Check PDF files are valid
#'
#' Checks PDF files exist, are above a minimum size, and start with %PDF.
#'
#' @param path Character vector of file paths.
#' @param min_size_kb Numeric. Minimum file size in KB. Default is 15.
#'
#' @return Logical vector, TRUE where the file is a valid PDF.
#' @export
check_pdf <- function(path, min_size_kb = 15){
  
  vapply(path, function(f) {
    
    # Check file exists
    if (is.na(f) || !file.exists(f)) return(FALSE)
    
    # Check file size
    if (file.size(f) < min_size_kb * 1024) return(FALSE)
    
    # Check file starts with %PDF
    identical(readBin(f, "raw", 4), charToRaw("%PDF"))
    
  }, logical(1), USE.NAMES = FALSE)
}


# Internal helper: remove a file only if it exists (avoids file.remove warnings)
remove_if_exists <- function(f){
  if (!is.na(f) && file.exists(f)) file.remove(f)
  invisible(NULL)
}


#' Retrieve PDF documents from Unpaywall using DOI
#'
#' @param doi character string containing DOI
#' @param uid optional unique ID for file naming
#' @param unpaywall_email your email, required for retrieve from Unpaywall
#' @param ft_path folder where PDFs should be downloaded
#' @param ft_name_style naming style for PDFs; options = c("doi", "uid")
#'
#' @return dataframe summary of retrieval
#' @export
#'
#' @import roadoi
#' @import tidyr
#' @import utils
#' @import dplyr
#'
ft_unpaywall <- function(doi, uid, unpaywall_email, ft_path, ft_name_style = "doi"){
  
  
  # Check DOI is character
  if(!is.character(doi) || !is.character(unpaywall_email)){
    stop("doi and unpaywall_email should be character strings")
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path, recursive = TRUE)
    message("file path created for full texts: ", ft_path)
  }
  
  # URL encode the DOI
  doi_encoded <- utils::URLencode(doi, reserved = TRUE)
  
  # Check ft_name_style is valid
  if(ft_name_style == "uid"){
    ft_name = uid
  } else if(ft_name_style == "doi"){
    ft_name = doi_encoded
  } else{
    stop("ft_name_style should be 'doi' or 'uid'; default is doi")
  }
  
  # Query DOI using DOI
  res <- tryCatch(
    suppressWarnings(
      suppressMessages(
        roadoi::oadoi_fetch(dois = doi, email = unpaywall_email))),
    error = function(e) NULL)
  
  # If response given
  if(!is.null(res)){
    
    if (!"best_oa_location" %in% names(res)) {
      message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
      
      ft_summary <- data.frame(
        status = "failed",
        doi = doi,
        doi_encoded = doi_encoded,
        method = "unpaywall",
        path = NA,
        ft_ext = NA,
        date = Sys.Date()
      )
      
      return(ft_summary)
    }
    # remove wiley, elsiever, sage to avoid errors
    df <- res %>%
      # Select columns
      select(best_oa_location, doi, oa_status, publisher) %>%
      # Unnest columns
      tidyr::unnest(cols = c(best_oa_location))
    
    if(nrow(df) > 0){
      df <- df %>%
        mutate(path = paste0(ft_path, "/", ft_name, ".pdf")) %>%
        # Remove big publishers
        # filter(!publisher %in% c("Wiley", "Elsevier BV", "SAGE Publications")) %>%
        filter(!publisher %in% c("Wiley", "Elsevier BV")) %>%
        # filter(!grepl("tandfonline",url)) %>%
        rename(doi_encoded = doi) %>%
        mutate(doi_encoded = utils::URLencode(doi_encoded, reserved = TRUE))
      # Fix if pdf url column is missing
      if (!"url_for_pdf" %in% names(df)) {
        if ("url" %in% names(df)) {
          df <- df %>% rename(url_for_pdf = url)
        } else {
          df$url_for_pdf <- NA_character_
        }
      }
      # Create file path
      df <- df %>%
        filter(!is.na(url_for_pdf)) %>%
        filter(grepl("\\.pdf$", url_for_pdf))
      # Check still result after filtering
      if(nrow(df) > 0) {
        # Extract PDF URL and file destination
        upw_urls <- df$url_for_pdf
        upw_dest <- df$path
        
        # Download PDFs using Unpaywall URL
        for (i in seq_along(upw_urls)) {
          tryCatch(
            {
              # Download file
              download.file(upw_urls[i], upw_dest[i])
            },
            error = function(e) {
              # Print error message
              message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
            },
            warning = function(w) {
              # Print warning message
              message(sprintf("Download warning for DOI '%s'", doi))
            }
          )
          
          # Stop if valid PDF downloaded
          if (check_pdf(upw_dest[i])) {
            
            message(sprintf("Found unpaywall PDF for DOI '%s'", doi))
            break
            
          } else if (file.exists(upw_dest[i])) {
            
            # Remove invalid or small file
            file.remove(upw_dest[i])
            message(sprintf("Invalid or under 15KB PDF removed for DOI '%s'", doi))
          }
        }
      } else{
        message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
      }
    } else{
      message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
    }
    # Keep only valid PDFs (after download)
    if ("path" %in% names(df)){
      df_valid <- df[check_pdf(df$path), ]
    } else{
      df_valid <- data.frame()
    }
    
    # If at least one file was successfully downloaded
    if (nrow(df_valid) > 0) {
      ft_summary <- data.frame(
        status = "found",
        doi = doi,
        doi_encoded = doi_encoded,
        method = "unpaywall",
        path = df_valid$path[1],
        ft_ext = tools::file_ext(df_valid$path[1]),
        date = Sys.Date()
      )
    } else {
      # No valid files downloaded
      ft_summary <- data.frame(
        status = "failed",
        doi = doi,
        doi_encoded = doi_encoded,
        method = "unpaywall",
        path = NA,
        ft_ext = NA,
        date = Sys.Date()
      )
    }
  } else{
    # Print error message
    message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
    # Generate summary
    ft_summary <- data.frame(
      status = "failed",
      doi = doi,
      doi_encoded = doi_encoded,
      method = "unpaywall",
      path = NA,
      ft_ext = NA,
      date = Sys.Date()
    )
  }
  # Return summary
  return(ft_summary)
}


#' Retrieve PDF documents from Crossref using DOI
#'
#' @param doi character string containing DOI
#' @param uid optional unique ID for file naming
#' @param ft_path folder where PDFs should be downloaded
#' @param ft_name_style naming style for PDFs; options = c("doi", "uid")
#'
#' @return dataframe summary of retrieval
#' @export
#'
#' @import rcrossref
#' @import utils
#' @import tidyr
#' @import dplyr
#'
ft_crossref <- function(doi, uid, ft_path, ft_name_style = "doi"){
  
  # Check DOI is character
  if(!is.character(doi)){
    stop("doi should be a character string")
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path, recursive = TRUE)
    message("file path created for full texts: ", ft_path)
  }
  
  # URL encode the DOI
  doi_encoded <- utils::URLencode(doi, reserved = TRUE)
  
  # Check ft_name_style is valid
  if(ft_name_style == "uid"){
    ft_name = uid
  } else if(ft_name_style == "doi"){
    ft_name = doi_encoded
  } else{
    stop("ft_name_style should be 'doi' or 'uid'; default is doi")
  }
  
  # Query API
  res <- tryCatch(
    suppressWarnings(
      suppressMessages(
        rcrossref::cr_works(dois = doi_encoded))),
    error = function(e) NULL)
  
  # If response given
  if(!is.null(res) && "link" %in% names(res$data)){
    # Get data
    df <- res$data %>%
      dplyr::select(name = doi, link) %>%
      dplyr::mutate(name = utils::URLencode(name, reserved = TRUE)) %>%
      tidyr::unnest(cols = c(link)) %>%
      dplyr::mutate(content.type = ifelse(content.type == "unspecified" & grepl("pdf", URL), "pdf", content.type)) %>%
      dplyr::filter(grepl("pdf", content.type)) %>%
      dplyr::mutate(
        path = paste0(ft_path, "/", ft_name, ".pdf")
      ) %>%
      dplyr::select(name, URL, content.type, path) %>%
      dplyr::distinct()
    
    # filter out big publishers (typically give warnings)
    df <- df %>%
      filter(!grepl("elsevier",URL)) %>%
      filter(!grepl("wiley",URL))
    #   filter(!grepl("pubs.acs.org",URL)) %>%
    #   filter(!grepl("sagepub",URL)) %>%
    #   filter(!grepl("tandfonline",URL))
    
    # check still result after filtering
    if(nrow(df) > 0){
      # Extract PDF URL and file destination
      cr_urls <- df$URL
      cr_dest <- df$path
      
      # Download PDFs using CrossRef URL
      for (i in seq_along(cr_urls)) {
        tryCatch(
          {
            # Download file
            download.file(cr_urls[i], cr_dest[i])
          },
          error = function(e) {
            # Print error message
            message(sprintf("Not found crossRef PDF for DOI '%s'", doi))
          },
          warning = function(w) {
            # Print warning message
            message(sprintf("Download warning for DOI '%s'", doi))
          }
        )
        
        # Stop if valid PDF downloaded
        if (check_pdf(cr_dest[i])) {
          
          message(sprintf("Found crossRef PDF for DOI '%s'", doi))
          break
          
        } else if (file.exists(cr_dest[i])) {
          
          # Remove invalid or small file
          file.remove(cr_dest[i])
          message(sprintf("Invalid or under 15KB PDF removed for DOI '%s'", doi))
        }
      }
    } else{
      message(sprintf("Not found crossRef PDF for DOI '%s'", doi))
    }
    # Keep only valid PDFs (after download)
    df_valid <- df[check_pdf(df$path), ]
    
    # If at least one file was successfully downloaded
    if (nrow(df_valid) > 0) {
      ft_summary <- data.frame(
        status = "found",
        doi = doi,
        doi_encoded = doi_encoded,
        method = "crossref",
        path = df_valid$path[1],
        ft_ext = tools::file_ext(df_valid$path[1]),
        date = Sys.Date()
      )
    } else {
      # No valid files downloaded
      ft_summary <- data.frame(
        
        status = "failed",
        doi = doi,
        doi_encoded = doi_encoded,
        method = "crossref",
        path = NA,
        ft_ext = NA,
        date = Sys.Date()
      )
    }
  } else{
    # Print error message
    message(sprintf("Not found crossRef PDF for DOI '%s'", doi))
    # Generate summary
    ft_summary <- data.frame(
      status = "failed",
      doi = doi,
      doi_encoded = doi_encoded,
      method = "crossref",
      path = NA,
      ft_ext = NA,
      date = Sys.Date()
    )
  }
  # Return summary
  return(ft_summary)
}


#' Retrieve PDF documents from Wiley using DOI
#'
#' @param doi character string containing DOI
#' @param uid optional unique ID for file naming
#' @param wiley_token API key required for text data mining on Wiley texts
#' @param ft_path folder where PDFs should be downloaded
#' @param ft_name_style naming style for PDFs; options = c("doi", "uid")
#'
#' @return dataframe summary of retrieval
#' @export
#'
#' @import httr
#' @import utils
#' @import dplyr
#'
ft_wiley <- function(doi, uid, wiley_token, ft_path, ft_name_style = "doi"){
  
  # Check DOI is character
  if(!is.character(doi) || !is.character(wiley_token)){
    stop("doi and wiley_token should be character strings")
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path, recursive = TRUE)
    message("file path created for full texts: ", ft_path)
  }
  
  # URL encode the DOI
  doi_encoded <- utils::URLencode(doi, reserved = TRUE)
  
  # Check ft_name_style is valid
  if(ft_name_style == "uid"){
    ft_name = uid
  } else if(ft_name_style == "doi"){
    ft_name = doi_encoded
  } else{
    stop("ft_name_style should be 'doi' or 'uid'; default is doi")
  }
  
  ft_file <- paste0(ft_path, "/", ft_name, ".pdf")
  
  # Query DOI using DOI
  res <- tryCatch({
    
    httr::GET(
      paste0("https://api.wiley.com/onlinelibrary/tdm/v1/articles/", doi_encoded),
      httr::add_headers(`Wiley-TDM-Client-Token` = wiley_token),
      httr::write_disk(ft_file, overwrite = TRUE)
    )
    
  }, error = function(e) {
    message(sprintf("ERROR for DOI '%s': %s", doi, e$message))
    return(NULL)
  })
  
  # If successful and valid PDF
  if(!is.null(res) && httr::status_code(res) == 200 && check_pdf(ft_file)){
    
    # Show success message
    message(sprintf("Found wiley PDF for DOI '%s'", doi))
    # Generate summary data frame
    ft_summary <- data.frame(
      status = "found",
      doi = doi,
      doi_encoded = doi_encoded,
      method = "wiley",
      path = ft_file,
      ft_ext = "pdf",
      date = Sys.Date()
    )
  } else{
    
    # Remove empty or invalid file
    remove_if_exists(ft_file)
    
    # Show error
    message(sprintf("No wiley PDF found for DOI '%s'", doi))
    # Generate summary
    ft_summary <- data.frame(
      status = "failed",
      doi = doi,
      doi_encoded = doi_encoded,
      method = "wiley",
      path = NA,
      ft_ext = NA,
      date = Sys.Date()
    )
  }
  # Return summary
  return(ft_summary)
}


#' Retrieve XML documents from Elsevier using DOI
#'
#' @param doi character string containing DOI
#' @param uid optional unique ID for file naming
#' @param elsevier_token API key required for text data mining on Elsevier texts
#' @param ft_path folder where XMLs should be downloaded
#' @param ft_name_style naming style for XMLs; options = c("doi", "uid")
#'
#' @return dataframe summary of retrieval
#' @export
#'
#' @import httr
#' @import utils
#' @import dplyr
#'
ft_elsevier <- function(doi, uid, elsevier_token, ft_path, ft_name_style = "doi"){
  
  # Check DOI is character
  if(!is.character(doi) || !is.character(elsevier_token)){
    stop("doi and elsevier_token should be character strings")
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path, recursive = TRUE)
    message("file path created for full texts: ", ft_path)
  }
  
  # URL encode the DOI
  doi_encoded <- utils::URLencode(doi, reserved = TRUE)
  
  # Check ft_name_style is valid
  if(ft_name_style == "uid"){
    ft_name = uid
  } else if(ft_name_style == "doi"){
    ft_name = doi_encoded
  } else{
    stop("ft_name_style should be 'doi' or 'uid'; default is doi")
  }
  
  ft_file <- paste0(ft_path, "/", ft_name, ".xml")
  
  # Query DOI using DOI
  res <- tryCatch({
    
    httr::GET(paste0("https://api.elsevier.com/content/article/doi/", doi_encoded),
              httr::add_headers(`X-ELS-APIKey` = elsevier_token, Accept = "text/xml"),
              httr::write_disk(ft_file, overwrite=TRUE))
    
  }, error = function(e) {
    message(sprintf("ERROR for DOI '%s': %s", doi, e$message))
    return(NULL)
  })
  
  # If successful
  if(!is.null(res) && httr::status_code(res) == 200){
    
    # Show success message
    message(sprintf("Found elsevier XML for DOI '%s'", doi))
    # Generate summary data frame
    ft_summary <- data.frame(
      status = "found",
      doi = doi,
      doi_encoded = doi_encoded,
      method = "elsevier",
      path = ft_file,
      ft_ext = "xml", date = Sys.Date()
    )
  } else{
    
    # Remove empty file
    remove_if_exists(ft_file)
    
    # Show error
    message(sprintf("No elsevier XML found for DOI '%s'", doi))
    # Generate summary
    ft_summary <- data.frame(
      status = "failed",
      doi = doi,
      doi_encoded = doi_encoded,
      method = "elsevier",
      path = NA,
      ft_ext = NA,
      date = Sys.Date()
    )
  }
  # Return summary
  return(ft_summary)
}


#' Retrieve XML documents from EuropePMC using DOI
#'
#' @param doi character string containing DOI
#' @param uid optional unique ID for file naming
#' @param ft_path folder where XMLs should be downloaded
#' @param ft_name_style naming style for XMLs; options = c("doi", "uid")
#'
#' @return dataframe summary of retrieval
#' @export
#'
#' @import rcrossref
#' @import europepmc
#' @import xml2
#' @import utils
#' @import dplyr
#'
ft_epmc <- function(doi, uid, ft_path, ft_name_style = "doi"){
  
  # Check DOI is character
  if(!is.character(doi)){
    stop("doi should be a character string")
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path, recursive = TRUE)
    message("file path created for full texts: ", ft_path)
  }
  
  # URL encode the DOI
  doi_encoded <- utils::URLencode(doi, reserved = TRUE)
  
  # Check ft_name_style is valid
  if(ft_name_style == "uid"){
    ft_name = uid
  } else if(ft_name_style == "doi"){
    ft_name = doi_encoded
  } else{
    stop("ft_name_style should be 'doi' or 'uid'; default is doi")
  }
  
  # Get pmcids
  pmcid_id <- tryCatch({
    
    # Query rcrossref
    result <- rcrossref::id_converter(doi)
    
    # get data
    pmcid_id <- result$records
    
    if ("status" %in% colnames(pmcid_id) || !"pmcid" %in% colnames(pmcid_id)) {
      # Error status or no PMCID, create placeholder
      data.frame(pmcid = NA, pmid = NA, doi = doi)
      
    } else {
      # Select relevant columns
      pmcid_id %>%
        select(pmcid, pmid, doi)
      
    }
  }, error = function(e) {
    # Handle errors from id_converter (e.g. network or API issues)
    data.frame(pmcid = NA, pmid = NA, doi = doi)
  })
  
  # Filter list with PMCID, keep first
  pmcid_id <- pmcid_id %>%
    dplyr::filter(!is.na(pmcid)) %>%
    head(1)
  
  # Continue if pmcids are found
  if(nrow(pmcid_id) > 0){
    
    ft_summary <- tryCatch({
      
      xml_result <- europepmc::epmc_ftxt(ext_id = pmcid_id$pmcid)
      
      # Save as file (suppress warning about %-encoded file names)
      suppressWarnings(xml2::write_xml(xml_result, paste0(ft_path, "/", ft_name, ".xml")))
      
      # Remove from environment
      rm(xml_result)
      # Print message
      message("Downloaded XML file for pmcid: ", pmcid_id$pmcid, " / DOI: ", doi)
      
      # Write summary
      data.frame(
        status = "found",
        doi = doi,
        doi_encoded = doi_encoded,
        method = "epmc",
        path = paste0(ft_path, "/", ft_name, ".xml"),
        ft_ext = "xml",
        date = Sys.Date()
      )
      
    }, error = function(e) {
      
      # Print a message if there's an error
      message("No XML for pmcid: ", pmcid_id$pmcid, " / DOI: ", doi)
      # Generate summary
      data.frame(
        status = "failed",
        doi = doi,
        doi_encoded = doi_encoded,
        method = "epmc",
        path = NA,
        ft_ext = NA,
        date = Sys.Date()
      )
    })
  } else{
    message("No PMCID for DOI: ", doi)
    # Generate summary
    ft_summary <- data.frame(
      status = "failed",
      doi = doi,
      doi_encoded = doi_encoded,
      method = "epmc",
      path = NA,
      ft_ext = NA,
      date = Sys.Date()
    )
  }
  
  # Return summary
  return(ft_summary)
}


#' Update and standardise full_texts database table
#'
#' Checks whether the \code{full_texts} table is in the expected format. If not,
#' optionally updates the table by:
#' \itemize{
#'   \item Saving a backup copy to a \code{db_tables_legacy} folder as an \code{.fst} file
#'   \item Rebuilding the table based on files currently present in a specified folder
#'   \item Reassigning \code{found}/\code{failed} status based on file presence
#' }
#'
#' @param con A DBI database connection.
#' @param path Character. Path to folder containing full text files.
#'
#' @return Invisibly returns NULL. Updates the \code{full_texts} table in the database.
#' @export
#'
#' @importFrom DBI dbExistsTable dbReadTable dbWriteTable
#' @importFrom utils menu URLdecode
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom fst write.fst
update_full_texts_tbl <- function(con, path = ""){
  
  
  if (DBI::dbExistsTable(con, "full_texts")){
    
    
    tidy_table <- utils::menu(c("Yes", "No"),
                              title = paste0("Would you like to remove the 'found' rows from the database table whose files do not exist in ", path, "?"))
    
    if (tidy_table == 1) {
      
      ft_files_missing <- DBI::dbReadTable(con, "full_texts") %>%
        dplyr::filter(status == "found") %>%
        dplyr::mutate(file_exists = !is.na(path) & file.exists(path)) %>%
        dplyr::filter(file_exists == FALSE)
      
      ft_update <- DBI::dbReadTable(con, "full_texts") %>%
        dplyr::filter(!(status == "found" & doi %in% ft_files_missing$doi))
      
      DBI::dbWriteTable(con, "full_texts", ft_update, overwrite = TRUE)
      
      message(paste0("Number of 'found' studies removed as the files don't exist: ", nrow(ft_files_missing), "\n",
                     "Database table has been updated."))
      
      
      
    } else {
      
      message("No changes to the database table have been made")
      
      
    }
    
    
    # Check database table exists
    
    current_ft_tbl <- DBI::dbReadTable(con, "full_texts")
    
    # Required columns for updated table
    required_cols <- c("doi", "path", "status", "method", "ft_ext", "date", "doi_encoded")
    
    # Check current db table has required columns
    if (all(required_cols %in% colnames(current_ft_tbl))) {
      
      message("Table in correct format")
      
    } else {
      
      message("Table is missing columns")
      
      update_table <- utils::menu(c("Yes", "No"),
                                  title = "Would you like to update the full_texts database table to the new format? This will also save the current version.")
      
      if (update_table == 1) {
        
        
        legacy_folder <- "db_tables_legacy"
        
        # Create folder if it doesn't exist
        if (!dir.exists(legacy_folder)) {
          dir.create(legacy_folder, recursive = TRUE)
        }
        
        # Read table
        current_ft_tbl <- DBI::dbReadTable(con, "full_texts")
        
        # Create filename with date
        file_name <- paste0("full_texts_", Sys.Date(), ".fst")
        file_path <- file.path(legacy_folder, file_name)
        
        # Write to fst
        fst::write.fst(current_ft_tbl, file_path)
        
        message(paste0("Current full_texts table written to ", file_path))
        
        # Files currently in path folder
        files_found_now <- list.files(
          path = path,
          full.names = TRUE
        )
        
        # Retrieve file info
        files_info <- file.info(files_found_now)
        
        # Create "found" df
        # URLdecode also decodes the older fix_illegal_chars() file names
        pdfs_found_now_path <- data.frame(
          path = files_found_now,
          file_name = basename(files_found_now),
          stringsAsFactors = FALSE
        ) %>%
          dplyr::mutate(
            doi_encoded = tools::file_path_sans_ext(file_name),
            doi = vapply(doi_encoded, utils::URLdecode, character(1), USE.NAMES = FALSE),
            doi_encoded = vapply(doi, utils::URLencode, character(1), reserved = TRUE, USE.NAMES = FALSE),
            status = "found",
            method = "Unknown",
            ft_ext = tools::file_ext(file_name),
            date = as.Date(files_info$mtime),
            file_size = files_info$size
          ) %>%
          dplyr::select(status, doi_encoded, doi, method, path, ft_ext, date, file_size)
        
        # Read current table
        pdfs_checked <- DBI::dbReadTable(con, "full_texts") %>%
          select(doi, status)
        
        # Join new "found" table with current db table
        pdfs_checked_now <- full_join(pdfs_found_now_path, pdfs_checked, by="doi")
        
        # Find the "failed" studies
        pdfs_checked_now <- pdfs_checked_now %>%
          mutate(status = ifelse(is.na(status.x), "failed", "found"),
                 doi_encoded = ifelse(is.na(doi_encoded),
                                      vapply(doi, utils::URLencode, character(1), reserved = TRUE, USE.NAMES = FALSE),
                                      doi_encoded)) %>%
          select(status, doi, doi_encoded, method, path, ft_ext, date) %>%
          distinct()
        
        # Write to db
        DBI::dbWriteTable(con, "full_texts", pdfs_checked_now, overwrite = TRUE)
        
        
        message("Table updated to new format.")
        
        
      } else {
        
        message("full_texts table not updated.")
        
      }
      
    }
    
  } else{
    
    stop("No full_texts database table exists.")
    
  }
  
  return(invisible(NULL))
  
}


#' Clean full_texts folder by removing unwanted files
#'
#' Interactively cleans the full texts folder and updates the full_texts database table.
#' The number of files is shown before each step:
#' \itemize{
#'   \item All `.json` files and their corresponding `.txt` files are deleted
#'   \item `.txt` files which are actually PDFs are renamed to `.pdf`
#'   \item `.pdf` files below 15 KB and their corresponding `.txt` files are deleted
#'   \item Rows in the full_texts table where the file no longer exists are removed
#' }
#'
#' @param path Character. Path to the folder containing full text files.
#' @param con Database connection. If NULL, the full_texts table is not updated.
#'
#' @return Invisibly returns NULL. Prints summary of files removed.
#' @export
clean_full_texts_folder <- function(path, con = NULL){
  
  # Remove json files ---------------------
  
  # json files
  json_files <- list.files(
    path,
    pattern = "\\.json$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Corresponding TXT files
  json_txts <- sub("\\.json$", ".txt", json_files, ignore.case = TRUE)
  
  # Only keep txt files that actually exist
  json_txts <- json_txts[file.exists(json_txts)]
  
  message(paste0("Number of json files: ", length(json_files), "\n",
                 "Number of linked txt files: ", length(json_txts)))
  
  remove_json <- utils::menu(c("Yes", "No"),
                             title = "Would you like to delete all JSON files and their linked .txt files from the path folder?")
  
  if (remove_json == 1) {
    
    # Combine and delete
    file.remove(c(json_files, json_txts))
    
    message(paste0(
      "json files deleted: ", length(json_files), "\n",
      "txt files deleted: ", length(json_txts)
    ))
    
  } else {
    
    message("json files and their linked txt files have not been deleted.")
  }
  
  # Rename txt files which are pdfs ---------------------
  
  txt_files <- list.files(
    path,
    pattern = "\\.txt$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Check which txt files start with %PDF
  is_pdf <- vapply(txt_files, function(f) {
    identical(readBin(f, "raw", 4), charToRaw("%PDF"))
  }, logical(1))
  
  fake_txts <- txt_files[is_pdf]
  
  # New pdf file names
  new_pdfs <- sub("\\.txt$", ".pdf", fake_txts, ignore.case = TRUE)
  
  # Don't rename if pdf with same name already exists
  keep <- !file.exists(new_pdfs)
  fake_txts <- fake_txts[keep]
  new_pdfs <- new_pdfs[keep]
  
  message(paste0("Number of txt files which are pdfs: ", length(fake_txts)))
  
  rename_txt <- utils::menu(c("Yes", "No"),
                            title = "Would you like to rename the txt files which are pdfs to .pdf?")
  
  if (rename_txt == 1) {
    
    file.rename(fake_txts, new_pdfs)
    
    # Update file paths in database table
    if (!is.null(con)) {
      
      ft_tbl <- DBI::dbReadTable(con, "full_texts")
      
      if ("ft_ext" %in% colnames(ft_tbl)) {
        ft_tbl <- ft_tbl %>%
          dplyr::mutate(ft_ext = ifelse(path %in% fake_txts, "pdf", ft_ext))
      }
      
      ft_tbl <- ft_tbl %>%
        dplyr::mutate(path = ifelse(path %in% fake_txts, new_pdfs[match(path, fake_txts)], path))
      
      DBI::dbWriteTable(con, "full_texts", ft_tbl, overwrite = TRUE)
    }
    
    message(paste0("txt files renamed to pdf: ", length(fake_txts)))
    
  } else {
    
    message("txt files have not been renamed.")
  }
  
  # Remove small pdf files ---------------------
  
  files <- list.files(
    path,
    pattern = "\\.pdf$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  info <- file.info(files)
  
  # PDFs to delete
  pdfs_to_delete <- files[!is.na(info$size) & info$size < 15 * 1024]
  
  # Corresponding TXT files
  pdf_txts <- sub("\\.pdf$", ".txt", pdfs_to_delete, ignore.case = TRUE)
  
  # Only keep txt files that actually exist
  pdf_txts <- pdf_txts[file.exists(pdf_txts)]
  
  message(paste0("Number of pdf files under 15KB: ", length(pdfs_to_delete), "\n",
                 "Number of linked txt files: ", length(pdf_txts)))
  
  remove_pdf <- utils::menu(c("Yes", "No"),
                            title = "Would you like to delete all PDF files under 15KB, and their linked .txt files?")
  
  if (remove_pdf == 1) {
    
    # Combine and delete
    file.remove(c(pdfs_to_delete, pdf_txts))
    
    message(paste0(
      "pdf files deleted: ", length(pdfs_to_delete), "\n",
      "txt files deleted: ", length(pdf_txts)
    ))
    
  } else {
    
    message("pdf files and their linked txt files have not been deleted.")
  }
  
  # Update full_texts database table ---------------------
  
  if (!is.null(con)) {
    
    ft_tbl <- DBI::dbReadTable(con, "full_texts") %>%
      dplyr::mutate(file_exists = ifelse(is.na(path), FALSE, file.exists(path)))
    
    # Found studies where the file doesn't exist
    ft_missing <- ft_tbl %>%
      dplyr::filter(status == "found") %>%
      dplyr::filter(file_exists == FALSE)
    
    message(paste0("Number of rows in full_texts table where the file doesn't exist: ", nrow(ft_missing)))
    
    update_table <- utils::menu(c("Yes", "No"),
                                title = "Would you like to remove these rows from the full_texts table?")
    
    if (update_table == 1) {
      
      ft_update <- ft_tbl %>%
        # Remove found rows where the file doesn't exist
        dplyr::filter(!(status == "found" & file_exists == FALSE)) %>%
        dplyr::select(-file_exists)
      
      DBI::dbWriteTable(con, "full_texts", ft_update, overwrite = TRUE)
      
      message(paste0(nrow(ft_missing), " rows removed. Database table has been updated."))
      
    } else {
      
      message("No changes to the database table have been made")
    }
  }
  
  return(invisible(NULL))
}