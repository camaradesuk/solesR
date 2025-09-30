#' Retrieve full text documents
#'
#' Retrieve full texts as PDF or XML for SOLES.
#'
#' @param con Database connection
#' @param pdf_source where to retrieve PDFs from, options = c("unpaywall", "crossref", "wiley") OR NULL
#' @param xml_source where to retrieve XMLs from, options = c("epmc", "elsevier") OR NULL
#' @param ft_path folder where full texts should be downloaded to
#' @param retMax maximum number of full texts to attempt to retrieve; max = 500; dault = 500
#' @param check_failed logical, whether to check DOIs that previously failed; default = FALSE
#' @param timespan timespan for check_failed in format digit(week/month) e.g. "2month"
#' @param wiley_token API token required to retrieve from Wiley
#' @param elsevier_token API token required to retrieve from Elsevier
#' @param unpaywall_email Your email, required to retrieve from Unpaywall
#'
#' @return summary of retrieval wrritten to database
#' @export
#' 
#' @import DBI
#' @import dplyr
#'
get_ft <- function(con, pdf_source = c("unpaywall", "crossref", "wiley"), xml_source = c("epmc", "elsevier"), ft_path, retMax = 500, check_failed = FALSE, timespan = NULL, wiley_token = NULL, elsevier_token = NULL, unpaywall_email = NULL){
  
  # Check con is exists
  if(!inherits(con, "PqConnection")){
    stop("'con' is not a valid database connection")
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
      stop("'pdf_source' is invalid, options are: 'epmc', 'elsevier' or NULL")
    }
  }
  
  # Check tokens and emails exist if required
  if("unpaywall" %in% pdf_source & is.null(unpaywall_email)){
    stop("Cannot retrieve using unpaywall if unpaywall_email argument is missing")
  }
  if("wiley" %in% pdf_source & is.null(wiley_token)){
    stop("Cannot retrieve using wiley if wiley_token argument is missing")
  }
  if("elsevier" %in% pdf_source & is.null(elsevier_token)){
    stop("Cannot retrieve using elsevier if elsevier_token argument is missing")
  }
  
  # If ft_path does not exist, create it
  if(!dir.exists(ft_path)){
    dir.create(ft_path)
    message("file path created for Unpaywall full texts: ", ft_path)
  }
  
  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax %% 1 != 0 | retMax < 0) {
    stop("retMax is not a whole number")
  }
  
  # Check retMax and exit if above maximum
  if (retMax > 500) {
    stop("retMax is too high, max is 500")
  }
  
  # Check check_failed is Boolean
  if (is.logical(check_failed) == FALSE) {
    stop(message("check_soles argument should be set to TRUE or FALSE, default is FALSE"))
  }
  
  # If check failed is used
  if(isTRUE(check_failed)){
    # Check timespan is given
    if(is.null(timespan)){
      stop("to check failed, use tmespan argument")
    }
    # Check timepsan is valid
    if (grepl("^(?i)\\d+(week|month)$", timespan) == FALSE) {
      stop("timespan format incorrect, should contain digit and week or month, e.g. `2month`")
    }
    # Define timespan for retrieval
    if (grepl("(?i)week", timespan) == TRUE) {
      # Get number of weeks by removing non-digit characters
      x <- as.numeric(gsub("\\D", "", timespan))
      # Assign date as x number of weeks before today's date
      retrieval_date <- Sys.Date() - 7 * x
    } else if (grepl("(?i)month", timespan) == TRUE) {
      # Get number of months by removing non-digit characters
      x <- as.numeric(gsub("\\D", "", timespan))
      # Assign date as x number of months before today's date
      retrieval_date <- Sys.Date() - 31 * x
    }
    message("Rechecking DOIs previously checked before ", retrieval_date)
  }
  
  # If full_texts table exists, read it in
  if (dbExistsTable(con, "full_texts")){
    ft_found <- DBI::dbReadTable(con, "full_texts")
    if(isTRUE(check_failed)){
      ft_found <- ft_found %>%
        dplyr::filter(date > retrieval_date)
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
                  !is.na(doi)) %>%
    dplyr::select(doi) %>%
    dplyr::distinct() %>%
    collect()
  
  # Filter ot found
  dois <- dois %>%
    dplyr::filter(!doi %in% ft_found$doi)
  
  
  # Get max number of dois to retrieve full texts for
  if(nrow(dois) < 1){
    stop("No more full texts to retrieve!")
  } else if(nrow(dois) > retMax){
    message(nrow(dois), " full texts to retrieve; limiting to first ", retMax)
    dois <- head(dois, retMax)
  } else{
    message(nrow(dois), " full texts to retrieve")
  }
  
  # Create overall results 
  ft_summary_all <- NULL
  
  # If pdf_source is not null, retrieve PDFs
  if(!is.null(pdf_source)){
    
    # Get list of dois to retrieve
    dois_pdf <- dois$doi
    
    # Create empty results
    pdf_summary <- NULL
    
    # retrieve using unpaywall
    if("unpaywall" %in% pdf_source){
      message("Trying unpaywall...")
      for(i in 1:length(dois_pdf)){
        message("Trying DOI ", i, " of ", length(dois_pdf), " in unpaywall")
        pdf_summary_new <- ft_unpaywall(dois_pdf[i], unpaywall_email = unpaywall_email, ft_path = ft_path, ft_name_style = "doi")
        pdf_summary <- rbind(pdf_summary, pdf_summary_new)
        Sys.sleep(1)
      }
      # Remove not found from summary
      pdf_summary <- pdf_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(ft_path_full))
      message(nrow(pdf_summary), " files found from unpaywall")
      # Remove from doi list where file found
      dois_pdf <- dois_pdf[!dois_pdf %in% pdf_summary$doi]
      message("Finished unpaywall: ", length(dois_pdf), " full texts left to find")
    }
    
    # retrieve using crossref
    if("crossref" %in% pdf_source & length(dois_pdf) > 0){
      message("Trying crossref...")
      for(i in 1:length(dois_pdf)){
        message("Trying DOI ", i, " of ", length(dois_pdf), " in crossref")
        pdf_summary_new <- ft_crossref(dois_pdf[i], ft_path = ft_path, ft_name_style = "doi")
        pdf_summary <- rbind(pdf_summary, pdf_summary_new)
        Sys.sleep(1)
      }
      # Remove not found from summary
      pdf_summary <- pdf_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(ft_path_full))
      message(nrow(pdf_summary), " files found from crossref")
      # Remove from doi list where file found
      dois_pdf <- dois_pdf[!dois_pdf %in% pdf_summary$doi]
      message("Finished crossref: ", length(dois_pdf), " full texts left to find")
    }
    
    # retrieve using wiley
    if("wiley" %in% pdf_source & length(dois_pdf)> 0){
      message("Trying wiley...")
      for(i in 1:length(dois_pdf)){
        message("Trying DOI ", i, " of ", length(dois_pdf), " in wiley")
        pdf_summary_new <- ft_wiley(dois_pdf[i], wiley_token = wiley_token, ft_path = ft_path, ft_name_style = "doi")
        pdf_summary <- rbind(pdf_summary, pdf_summary_new)
        Sys.sleep(1)
      }
      # Remove not found from summary
      pdf_summary <- pdf_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(ft_path_full))
      message(nrow(pdf_summary), " files found from wiley")
      # Remove from doi list where file found
      dois_pdf <- dois_pdf[!dois_pdf %in% pdf_summary$doi]
      message("Finished wiley: ", length(dois_pdf), " full texts left to find")
    }
    
    # Add to overall results
    ft_summary_all <- rbind(ft_summary_all, pdf_summary)
  }
  
  # If pdf_source is not null, retrieve PDFs
  if(!is.null(xml_source)){
    
    # Get list of dois to retrieve
    dois_xml <- dois$doi
    
    # Create empty results
    xml_summary <- NULL
    
    # retrieve using epmc
    if("epmc" %in% xml_source){
      message("Trying epmc...")
      for(i in 1:length(dois_xml)){
        message("Trying DOI ", i, " of ", length(dois_xml), " in epmc")
        xml_summary_new <- ft_epmc(dois_xml[i], ft_path = ft_path, ft_name_style = "doi")
        xml_summary <- rbind(xml_summary, xml_summary_new)
        Sys.sleep(1)
      }
      # Remove not found from summary
      xml_summary <- xml_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(ft_path_full))
      message(nrow(xml_summary), " files found from epmc")
      # Remove from doi list where file found
      dois_xml <- dois_xml[!dois_xml %in% xml_summary$doi]
      message("Finished epmc: ", length(dois_xml), " full texts left to find")
    }
    
    # retrieve using elsevier
    if("elsevier" %in% xml_source & length(dois_xml) > 0){
      message("Trying elsevier...")
      for(i in 1:length(dois_xml)){
        message("Trying DOI ", i, " of ", length(dois_xml), " in elsevier")
        xml_summary_new <- ft_elsevier(dois_xml[i], elsevier_token = elsevier_token, ft_path = ft_path, ft_name_style = "doi")
        xml_summary <- rbind(xml_summary, xml_summary_new)
        Sys.sleep(1)
      }
      # Remove not found from summary
      xml_summary <- xml_summary %>%
        # Remove where no file path, therefore no file
        dplyr::filter(!is.na(ft_path_full))
      message(nrow(xml_summary), " files found from elsevier")
      # Remove from doi list where file found
      dois_xml <- dois_xml[!dois_xml %in% xml_summary$doi]
      message("Finished elsevier: ", length(dois_xml), " full texts left to find")
    }
    
    # Add to overall results
    ft_summary_all <- rbind(ft_summary_all, xml_summary)
    
  }
  
  # if results still null, do file found
  if(is.null(ft_summary_all)){
    message("no full texts found")
  }
  
  # Summarise not found
  ft_summary_not_found <- dois %>%
    dplyr::filter(!doi %in% ft_summary_all$doi) %>%
    dplyr::mutate(doi_encoded = URLencode(doi, reserved = TRUE),
                  method = NA,
                  ft_path_full = NA,
                  ft_ext = NA,
                  date = Sys.Date())
  
  # Combine previously found, found now, and not found
  ft_summary_all <- rbind(ft_found, ft_summary_all, ft_summary_not_found)
  
  # Write to database
  DBI::dbWriteTable(con, "full_texts", ft_summary_all, overwrite = TRUE)
  
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
  if(is.character(doi) == FALSE | is.character(unpaywall_email) == FALSE){
    stop(message("doi, and unpaywall_email should all be character strings"))
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path)
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
  
  # Query DPI using DOI
  try(res <- suppressWarnings(
    suppressMessages(
      roadoi::oadoi_fetch(dois = doi, email = unpaywall_email))),silent=TRUE)
  # If response given
  if(exists("res")){
    # remove wiley, elsiever, sage to avoid errors
    df <- res %>%
      # Select columns
      select(best_oa_location, doi, oa_status, publisher) %>%
      # Unnest columns
      tidyr::unnest(cols = c(best_oa_location))
    if(nrow(df) > 0){
      df <- df %>%
        mutate(ft_path_full = paste0(ft_path, "/", ft_name, ".pdf")) %>%
        # Remove big publishers
        filter(!publisher %in% c("Wiley", "Elsevier BV", "SAGE Publications")) %>% 
        filter(!grepl("tandfonline",url)) %>%
        rename(doi_encoded = doi) %>%
        mutate(doi_encoded = utils::URLencode(doi_encoded, reserved = TRUE))
      # Fix if pdf url column is missing
      if (!"url_for_pdf" %in% colnames(df)) {
        df <- df %>%
          rename(url_for_pdf = url)
      }
      # Create file path
      df <- df %>%
        filter(!is.na(url_for_pdf)) %>%
        filter(grepl("\\.pdf$", url_for_pdf))
      # Check still result after filtering
      if(nrow(df) > 0) {
        # Extract PDF URL and file destination
        upw_urls <- df$url_for_pdf
        upw_dest <- df$ft_path_full
        
        # Download PDFs using CrossRef URL
        for (i in 1:length(upw_urls)) {
          tryCatch(
            {
              # Download file
              download.file(upw_urls[i], upw_dest[i])
              # Print success message
              message(sprintf("Found unpaywall PDF for DOI '%s'", doi))
            },
            error = function(e) {
              # Print error message
              message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
              
            },
            warning = function(w) {
              # Check if file exists
              if (file.exists(upw_dest[i])) {
                # Get file size
                file_size <- file.size(upw_dest[i])
                # If file size is zxero, remove
                if (file_size == 0){
                  file.remove(upw_dest[i])
                  # Print warning message
                  message(sprintf("File size error in PDF for DOI '%s'", doi))
                }
              }
            }
          )
        }
      } else{
        message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
      }
    } else{
      message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
    }
    # Keep only files that actually exist (after download)
    if ("ft_path_full" %in% names(df)){
      df_valid <- df[file.exists(df$ft_path_full), ]
    } else{
      df_valid <- data.frame()
    }
    
    # If at least one file was successfully downloaded
    if (nrow(df_valid) > 0) {
      ft_summary <- data.frame(
        doi = doi,
        doi_encoded = doi_encoded,
        method = "unpaywall",
        ft_path_full = df_valid$ft_path_full[1],
        ft_ext = tools::file_ext(df_valid$ft_path_full[1]),
        date = Sys.Date()
      )
    } else {
      # No valid files downloaded
      ft_summary <- data.frame(
        doi = doi,
        doi_encoded = doi_encoded,
        method = "unpaywall",
        ft_path_full = NA,
        ft_ext = NA,
        date = Sys.Date()
      )
    }
  } else{
    # Print error message
    message(sprintf("Not found unpaywall PDF for DOI '%s'", doi))
    # Generate summary
    ft_summary <- data.frame(
      doi = doi,
      doi_encoded = doi_encoded,
      method = "unpaywall",
      ft_path_full = NA,
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
  if(is.character(doi) == FALSE){
    stop(message("doi should be character strings"))
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path)
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
  try(res <- suppressWarnings(
    suppressMessages(
      rcrossref::cr_works(dois = doi_encoded))), silent=TRUE)
  # If response given
  if(exists("res") && "link" %in% names(res$data)){
    # Get data
    df <- res$data %>%
      dplyr::select(name = doi, link) %>%
      dplyr::mutate(name = utils::URLencode(name, reserved = TRUE)) %>%
      tidyr::unnest(cols = c(link)) %>%
      dplyr::mutate(content.type = ifelse(content.type == "unspecified" & grepl("pdf", URL), "pdf", content.type)) %>%
      dplyr::filter(grepl("pdf", content.type)) %>%
      dplyr::mutate(
        ft_path_full = paste0(ft_path, "/", ft_name, ".pdf")
      ) %>%
      dplyr::select(name, URL, content.type, ft_path_full) %>%
      dplyr::distinct()
    
    # filter out big publishers (typically give warnings)
    df <- df %>%
      filter(!grepl("elsevier",URL)) %>%
      filter(!grepl("wiley",URL)) %>%
      filter(!grepl("pubs.acs.org",URL)) %>% 
      filter(!grepl("sagepub",URL)) %>% 
      filter(!grepl("tandfonline",URL))
    
    # check still result after filtering
    if(nrow(df) > 0){
      # Extract PDF URL and file destination
      cr_urls <- df$URL
      cr_dest <- df$ft_path_full
      
      # Download PDFs using CrossRef URL
      for (i in 1:length(cr_urls)) {
        tryCatch(
          {
            # Download file
            download.file(cr_urls[i], cr_dest[i])
            # Print success message
            message(sprintf("Found crossRef PDF for DOI '%s'", doi))
          },
          error = function(e) {
            # Print error message
            message(sprintf("Not found crossRef PDF for DOI '%s'", doi))
            
          },
          warning = function(w) {
            # Check if file exists
            if (file.exists(cr_dest[i])) {
              # Get file size
              file_size <- file.size(cr_dest[i])
              # If file size is zxero, remove
              if (file_size == 0){
                file.remove(cr_dest[i])
                # Print warning message
                message(sprintf("File size error in PDF for DOI '%s'", doi))
              }
            }
          }
        )
      }
    } else{
      message(sprintf("Not found crossRef PDF for DOI '%s'", doi))
    }
    # Keep only files that actually exist (after download)
    df_valid <- df[file.exists(df$ft_path_full), ]
    
    # If at least one file was successfully downloaded
    if (nrow(df_valid) > 0) {
      ft_summary <- data.frame(
        doi = doi,
        doi_encoded = doi_encoded,
        method = "crossRef",
        ft_path_full = df_valid$ft_path_full[1],
        ft_ext = tools::file_ext(df_valid$ft_path_full[1]),
        date = Sys.Date()
      )
    } else {
      # No valid files downloaded
      ft_summary <- data.frame(
        doi = doi,
        doi_encoded = doi_encoded,
        method = "crossRef",
        ft_path_full = NA,
        ft_ext = NA,
        date = Sys.Date()
      )
    }
  } else{
    # Print error message
    message(sprintf("Not found crossRef PDF for DOI '%s'", doi))
    # Generate summary
    ft_summary <- data.frame(
      doi = doi,
      doi_encoded = doi_encoded,
      method = "crossRef",
      ft_path_full = NA,
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
  if(is.character(doi) == FALSE | is.character(wiley_token) == FALSE){
    stop(message("doi and wiley_token should all be character strings"))
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path)
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
  
  # Query DPI using DOI
  res <- httr::GET(paste0(url = "https://api.wiley.com/onlinelibrary/tdm/v1/articles/", doi),
                   httr::add_headers(`Wiley-TDM-Client-Token` = wiley_token))
  
  # If successful
  if(httr::status_code(res) == 200){
    # Retrieve PDF and save using naming convention
    res <- httr::GET(paste0(url = "https://api.wiley.com/onlinelibrary/tdm/v1/articles/", doi),
                     httr::add_headers(`Wiley-TDM-Client-Token` = wiley_token),
                     httr::write_disk(paste0(ft_path, "/", ft_name, ".pdf"), overwrite=TRUE))
    # Show success message
    message(sprintf("Found wiley PDF for DOI '%s'", doi))
    # Generate summary data frme
    ft_summary <- data.frame(
      doi = doi,
      doi_encoded = doi_encoded,
      method = "wiley",
      ft_path_full = paste0(ft_path, "/", ft_name, ".pdf"),
      ft_ext = "pdf",
      date = Sys.Date()
    )
  } else{
    # Show error
    message(sprintf("No wiley PDF found for DOI '%s'", doi))
    # Generate summary
    ft_summary <- data.frame(
      doi = doi,
      doi_encoded = doi_encoded,
      method = "wiley",
      ft_path_full = NA,
      ft_ext = NA,
      date = Sys.Date()
    )
  }
  # Return summary
  return(ft_summary)
}


#' Retrieve XML documents from Wiley using DOI
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
  if(is.character(doi) == FALSE | is.character(elsevier_token) == FALSE){
    stop(message("doi and elsevier_token should all be character strings"))
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path)
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
  
  # Query DPI using DOI
  res <- httr::GET(paste0(url = "https://api.elsevier.com/content/article/doi/", doi_encoded),
                   httr::add_headers(`X-ELS-APIKey` = elsevier_token))
  
  # If successful
  if(httr::status_code(res) == 200){
    # Retrieve XML and save using naming convention
    res <- httr::GET(paste0(url = "https://api.elsevier.com/content/article/doi/", doi_encoded),
                     httr::add_headers(`X-ELS-APIKey` = elsevier_token, Accept = ""),
                     httr::write_disk(paste0(ft_path, "/", ft_name, ".xml"), overwrite=TRUE))
    # Show success message
    message(sprintf("Found elsevier XML for DOI '%s'", doi))
    # Generate summary data frme
    ft_summary <- data.frame(
      doi = doi,
      doi_encoded = doi_encoded,
      method = "elsevier",
      ft_path_full = paste0(ft_path, "/", ft_name, ".xml"),
      ft_ext = "xml", date = Sys.Date()
    )
  } else{
    # Show error
    message(sprintf("No elsevier XML found for DOI '%s'", doi))
    # Generate summary
    ft_summary <- data.frame(
      doi = doi,
      doi_encoded = doi_encoded,
      method = "elsevier",
      ft_path_full = NA,
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
  if(is.character(doi) == FALSE ){
    stop(message("doi and elsevier_token should all be character strings"))
  }
  
  # Check ft_path exists
  if(!dir.exists(ft_path)){
    dir.create(ft_path)
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
    # Query crossref
    result <- rcrossref::id_converter(doi)
    # get data
    result_df <- result$records
    
    if ("status" %in% colnames(result_df)) {
      # Error status, create placeholder
      data.frame(pmcid = NA, pmid = NA, doi = doi)
    } else {
      # Select relevant columns
      result_df %>% select(pmcid, pmid, doi)
    }
  }, error = function(e) {
    # Handle errors from id_converter (e.g. network or API issues)
    data.frame(pmcid = NA, pmid = NA, doi = doi)
  })
  
  # Filter list with PMCID
  pmcid_id <- result_df %>%
    dplyr::filter(!is.na(pmcid))
  
  # Continue if pmcids are found
  if(nrow(pmcid_id) > 0){
    tryCatch({
      xml_result <- europepmc::epmc_ftxt(ext_id = pmcid_id$pmcid)
      # Save as file if it exists
      xml2::write_xml(xml_result, paste0(ft_path, "/", ft_name, ".xml"))
      # Write summary
      ft_summary <- data.frame(
        doi = doi,
        doi_encoded = doi_encoded,
        method = "epmc",
        ft_path_full = paste0(ft_path, "/", ft_name, ".xml"),
        ft_ext = "xml", date = Sys.Date()
      )
      # Remove from environment
      rm(xml_result)
      # Print message
      message("Downloaded XML file for pmcid:", pmcid_id$pmcid, " / DOI: ", doi)
    }, error = function(e) {
      # Print a message if there's an error
      message("No XML for for pmcid:", pmcid_id$pmcid, " / DOI: ", doi)
      # Generate summary
      ft_summary <- data.frame(
        doi = doi,
        doi_encoded = doi_encoded,
        method = "epmc",
        ft_path_full = NA,
        ft_ext = NA,
        date = Sys.Date()
      )
    })
  } else{
    message(message("No PMCID for DOI: ", doi))
    # Generate summary
    ft_summary <- data.frame(
      doi = doi,
      doi_encoded = doi_encoded,
      method = "epmc",
      ft_path_full = NA,
      ft_ext = NA,
      date = Sys.Date()
    )
  }
}


