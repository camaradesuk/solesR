#' Download full texts
#'
#' This function downloads the full text of included citations
#'
#' @param con connection to db - note that this must contain tables of the format 'type_tag' and dictionaries of the type type_dictionary
#' @param path pdf folder
#' @param n number of full text publications to download
#' @param class_date screening date to filter by
#' @param class_name screening name to filter by
#' @param check_failed logical; check failed pdfs again?
#' @param email email address required for unpaywall
#' @import dplyr
#' @export
#'

get_ft <- function(con, path, n=NULL, class_date=NULL, class_name=NULL, check_failed=FALSE, email=""){
  
  # Get citation data --------
  # get dois
  dois <- tbl(con, "unique_citations") %>% select(doi, uid) %>% filter(!is.na(doi))
  
  if(check_failed == TRUE){
    
    ft_found <- tbl(con, "full_texts") %>% filter(status == "found")
    
  } else{
    
    # get list of full texts already searched for
    ft_found <- tbl(con, "full_texts")
    
  }
  
  # get included studies with no full text
  included_no_ft <- tbl(con, "study_classification") %>% filter(decision=="include") %>% select(uid, date, name) %>% distinct() %>%
    left_join(dois, by="uid") %>% anti_join(ft_found, by="doi") %>% collect()
  
  # remove missing dois
  included_no_ft <- included_no_ft %>% filter(!is.na(doi)) %>% filter(!doi == "")
  
  # apply filters for screening date
  if(!is.null(class_date)){
    
    included_no_ft <- included_no_ft %>%
      filter(date == class_date)
  }
  
  # apply filters for screening name
  if(!is.null(class_name)){
    
    included_no_ft <- included_no_ft %>%
      filter(name == class_name)
  }
  
  # get number of missing full texts
  n_missing <- nrow(included_no_ft)
  
  # If no texts missing, return messages to the user
  if (n_missing == 0){
    
    message("No more pdf files left to find!")
    message("All pdf files have either been \"found\",\"failed\" or have a mising DOI")
    message("Try check_failed = TRUE to download more pdf files.")
    return()
  }
  
  # if N specified, subset
  if(!is.null(n)){
    
    # if specified subset n is larger than the number to find then n becomes n_missing
    if (n_missing < n){
      
      n <- n_missing
      message("Only ", n, " full texts to find from included studies")
      
    }
    
    to_find <- included_no_ft[sample(nrow(included_no_ft), n), ]
    message("Trying to find ", n, " full texts from included studies.")
    
  } else if(nrow(included_no_ft) > 100){
    message("You have attempted to retrieve PDFs for ", n_missing, " records. Only processing the first 100 records.")
    to_find <- included_no_ft[1:100,]
    
  } else if(nrow(included_no_ft)==0){
    
    return()
    
  } else{
    
    to_find <- included_no_ft
    
  }
  
  # Unpawywall --------
  # try unpaywall using dois
  message("trying Unpaywall...")
  
  try(upw_res <- suppressWarnings(suppressMessages(roadoi::oadoi_fetch(dois = to_find$doi, email=email))),silent=TRUE)
  
  if(exists("upw_res")){
    
    # remove wiley, elsiever, sage to avoid errors
    upw_res <- upw_res %>%
      select(best_oa_location, doi, oa_status, publisher) %>%
      tidyr::unnest(cols = c(best_oa_location)) %>%
      filter(!publisher %in% c("Wiley", "Elsevier BV"))
    
    # link back to get uid
    upw_res <- left_join(upw_res, included_no_ft, by="doi")
    
    # write path for pdf download
    upw_res <- upw_res %>%
      mutate(doi_fixed = doi)
    
    # fix doi slash to dollar sign
    upw_res$doi_fixed  <- fix_illegal_chars(upw_res$doi_fixed)
    
    upw_res <- upw_res %>%
      mutate(pdf = paste0(path, "/", doi_fixed, ".pdf")) %>%
      filter(!is.na(url_for_pdf))
    
    
  } else {
    upw_res <- NULL}
  
  message(paste(nrow(upw_res), "full texts found via Unpaywall! Attempting download..."))
  
  urls <- upw_res$url_for_pdf
  dest <- upw_res$pdf
  
  #empty_file <- 0
  # Download texts from Unpaywall
  for (i in 1:length(urls)) {
    tryCatch(
      {
        
        download.file(urls[i], dest[i])
        cat("Downloaded:", urls[i], "\n")
        
        
      },
      error = function(e) {
        cat("Error occurred while downloading:", urls[i], "\n")
        
      },
      warning = function(w) {
        
        cat("Warning occurred while downloading:", urls[i], "\n")
        if (file.exists(dest[i])) {
          
          file_size <- file.size(dest[i])
          
          if (file_size == 0){
            
            #empty_file <- empty_file + 1
            
            file.remove(dest[i])
            
          }
          
        }
        
      }
    )
    
  }
  
  # CrossRef -------
  message("trying CrossRef....")
  pdfs_found_now <- get_dois_with_ft(path)
  
  # correction to make lower - DOI not always in same case!
  still_missing <- to_find %>% filter(!(tolower(doi) %in% tolower(pdfs_found_now)))
  
  try(cr_res <- suppressWarnings(suppressMessages(rcrossref::cr_works(dois = still_missing$doi))), silent=TRUE)
  
  if(exists("cr_res")){
    df <- cr_res$data
    df <- left_join(df, to_find, by="doi")
    df <- df %>% select(doi, uid, link) %>% tidyr::unnest(cols = c(link))
    df <- df %>%
      mutate(name = doi)
    
    df$name  <- fix_illegal_chars(df$name)
    
    df <- df %>%
      mutate(content.type = ifelse(content.type == "unspecified" & grepl('pdf', URL), "pdf", content.type)) %>%
      mutate(ft_path=ifelse(grepl('pdf',content.type), paste0(path, "/", name, ".pdf"), as.character(name))) %>%
      mutate(ft_path=ifelse(grepl('text',content.type), paste0(path, "/", name, ".txt"), as.character(ft_path))) %>%
      mutate(ft_path=ifelse(grepl('xml',content.type), paste0(path, "/", name, ".xml"), as.character(ft_path))) %>%
      mutate(ft_path = ifelse(grepl('xml$', URL), paste0(path, "/", name, ".xml"), as.character(ft_path))) %>%
      filter(!content.type == "unspecified")
    
    # Filter rows with content type "text/html"
    html_rows <- df %>%
      filter(content.type == "text/html")
    
    # Filter rows with content type "pdf"
    pdf_rows <- df %>%
      filter(grepl("pdf", content.type))
    
    # Identify ids with both "text/html" and "pdf"
    ids_with_both <- intersect(html_rows$doi, pdf_rows$doi)
    
    # Remove rows with content type "text/html" where corresponding doi also has "pdf"
    df <- df %>%
      filter(!(doi %in% ids_with_both & content.type == "text/html"))
    
    
    # filter out elsiever / wiley / acs (warnings)
    df <- df %>%
      filter(!grepl("elsevier",URL)) %>%
      filter(!grepl("wiley",URL)) %>%
      filter(!grepl("pubs.acs.org",URL))
    
  } else {df <- NULL}
  
  message(paste(length(unique(df$doi))), " full texts found via CrossRef! Attempting download....")
  
  cr_urls <- df$URL
  cr_dest <- df$ft_path
  
  # Download texts from CrossRef
  for (i in 1:length(cr_urls)) {
    tryCatch(
      {
        Sys.sleep(2)
        download.file(cr_urls[i], cr_dest[i])
        
        cat("Downloaded:", cr_urls[i], "\n")
      },
      error = function(e) {
        cat("Error occurred while downloading:", cr_urls[i], "\n")
        
      },
      warning = function(w) {
        cat("Warning occurred while downloading:", cr_urls[i], "\n")
        if (file.exists(cr_dest[i])) {
          
          file_size <- file.size(cr_dest[i])
          
          if (file_size == 0){
            
            #empty_file <<- empty_file + 1
            
            file.remove(cr_dest[i])
            
          }
          
        }
      }
    )
  }
  
  # Elsevier --------
  message("trying Elsevier....")
  pdfs_found_now <- get_dois_with_ft(path)
  
  still_to_find <- to_find %>% filter(!(tolower(doi) %in% tolower(pdfs_found_now)))
  
  for(i in 1:length(still_to_find$uid)){
    
    try(suppressWarnings(suppressMessages(
      elsevier_ft(still_to_find[i,"doi"], still_to_find[i,"doi"],  token = Sys.getenv("Elsevier_API"), path=path))), silent=TRUE)
  }
  
  pdfs_found_now <- get_dois_with_ft(path)
  
  # check if any new pdfs found in sample
  found <- still_to_find %>%
    filter(doi %in% pdfs_found_now)
  
  message(paste0("Found ", length(found$uid), " more full texts via Elsevier!"))
  
  # Wiley --------
  message("trying Wiley...")
  pdfs_found_now <- get_dois_with_ft(path)
  
  still_missing <- to_find %>% filter(!(tolower(doi) %in% tolower(pdfs_found_now)))
  
  still_missing$doi <- gsub('\\s+', '', still_missing$doi)
  
  for(i in 1:length(still_missing$uid)){
    
    try(suppressWarnings(suppressMessages(
      wiley_ft(still_missing[i,"doi"], still_missing[i,"doi"],  token = Sys.getenv("WILEY_API"), path=path))), silent=TRUE)
  }
  
  pdfs_found_now <- get_dois_with_ft(path)
  
  # check if any new pdfs found in sample
  found <- still_missing %>%
    filter(doi %in% pdfs_found_now)
  
  message(paste0("Found ", length(found$uid), " more full texts via Wiley!"))
  
  # Write to database -----
  # get paths
  pdfs_found_now_path <- get_dois_with_ft(path, remove_ext=FALSE)
  pdfs_found_now_path <- data.frame(doi = tools::file_path_sans_ext(pdfs_found_now_path),
                                    file.path = paste0(path, "/", fix_illegal_chars(pdfs_found_now_path)),
                                    status = "found")
  
  # read pdf link table
  pdfs_checked <- dbReadTable(con, "full_texts")
  
  # join with paths
  pdfs_checked_now <- full_join(pdfs_found_now_path, pdfs_checked, by="doi")
  
  pdfs_checked_now <- pdfs_checked_now %>%
    mutate(path = file.path) %>%
    mutate(status = ifelse(is.na(status.x), "failed", "found")) %>%
    select(status, doi, path) %>%
    distinct()
  
  # get failed to add to list
  add <- to_find %>%
    filter(!doi %in% pdfs_found_now_path$doi) %>%
    mutate(status="failed", path=NA) %>%
    select(status, doi, path)
  
  pdfs_checked_now <- rbind(pdfs_checked_now,add) %>%
    mutate(file_size = file.size(path)) %>%
    mutate(status = ifelse((file_size == 0 & status == "found"), "found - empty file", status)) %>%
    select(-file_size)
  
  # keep only one file for each doi
  pdfs_checked_now <- pdfs_checked_now %>%
    filter(!is.na(doi)) %>%
    group_by(doi) %>%
    arrange(desc(path)) %>%
    slice_head() %>%
    ungroup() %>%
    unique()
  
  # how many new full texts?
  n_found <- length(unique(pdfs_checked_now$doi[which(pdfs_checked_now$status=="found")])) - length(unique(pdfs_checked$doi[which(pdfs_checked$status=="found")]))
  message("Found ", n_found, "/", nrow(to_find), " full texts. Writing to SOLES database...")
  
  dbWriteTable(con, "full_texts", pdfs_checked_now, overwrite=TRUE)
  #cat(paste0("Empty Files: ", empty_file))
}


get_dois_with_ft <- function(path, remove_ext=TRUE){
  
  pdfs_found_now <- list.files(path = path)
  
  if(remove_ext==TRUE){
    
    pdfs_found_now <- tools::file_path_sans_ext(pdfs_found_now)
    
  } else {
    
    pdfs_found_now <- pdfs_found_now
  }
  
  pdfs_found_now <- gsub("\\%2F","\\/",pdfs_found_now)
  pdfs_found_now <- gsub("\\%3C","<",pdfs_found_now)
  pdfs_found_now <- gsub("\\%3E",">",pdfs_found_now)
  pdfs_found_now <- gsub("\\%3A",":",pdfs_found_now)
  pdfs_found_now <- gsub("\\%3B",";",pdfs_found_now)
  pdfs_found_now <- gsub("\\%22",'"',pdfs_found_now)
  pdfs_found_now <- gsub("\\%7C","\\|",pdfs_found_now)
  pdfs_found_now <- gsub("\\%3F","\\?",pdfs_found_now)
  pdfs_found_now <- gsub("\\%2A","\\*",pdfs_found_now)
  
}

fix_illegal_chars <- function(x){
  
  x <- gsub("\\/", "%2F", x)
  x <- gsub("<", "%3C", x)
  x  <- gsub(">", "%3E", x)
  x  <- gsub(":", "%3A", x)
  x <- gsub(";", "%3B", x)
  x  <- gsub('"', "%22", x)
  x  <- gsub("\\|", "%7C", x)
  x  <- gsub("\\?", "%3F", x)
  x  <- gsub("\\*", "%2A", x)
  
  
}

#' Wiley full text retrieval
#'
#'  This function retrieves full texts from Wiley journals
#' @export
#' @param doi digital object identifier
#' @param uid unique identifier for paper
#' @param token API token
#' @param path where full texts should be stored

wiley_ft <- function(doi, uid, token, path){
  
  doi <- gsub("\\/", "%2F", doi)
  uid  <- gsub("\\/", "%2F", uid)
  
  res <- httr::GET(paste0(url = "https://api.wiley.com/onlinelibrary/tdm/v1/articles/", doi),
                   httr::add_headers(`Wiley-TDM-Client-Token` = token))
  
  if(httr::status_code(res) == 200){
    
    res <- httr::GET(paste0(url = "https://api.wiley.com/onlinelibrary/tdm/v1/articles/", doi),
                     httr::add_headers(`Wiley-TDM-Client-Token` = token),
                     httr::write_disk(paste0(path, "/", doi, ".pdf"), overwrite=TRUE))
  } else{
    
    message("There is no Wiley API access to this doi")
  }
}


#' Elsevier full text retrieval
#'
#' This function retrieves full texts from Elsevier journals
#' @export
#' @param doi digital object identifier
#' @param uid unique identifier for paper
#' @param token API token
#' @param path where full texts should be stored
elsevier_ft <- function(doi, uid, token, path){
  
  doi <- gsub("\\/", "%2F", doi)
  uid  <- gsub("\\/", "%2F", uid)
  
  res <- httr::GET(paste0(url = "https://api.elsevier.com/content/article/doi/", doi),
                   httr::add_headers(`X-ELS-APIKey` = token))
  
  if(httr::status_code(res) == 200){
    
    res <- httr::GET(paste0(url = "https://api.elsevier.com/content/article/doi/", doi),
                     httr::add_headers(`X-ELS-APIKey` = token),
                     httr::write_disk(paste0(path, "/", uid, ".json"), overwrite=TRUE))
    
    file <- jsonlite::fromJSON(paste0(path, "/", uid, ".json"))
    file_txt <- file[["full-text-retrieval-response"]][["originalText"]]
    
    if(!is.null(file_txt)){
      
      write.table(file_txt, paste0(path, "/", uid, ".txt"), row.names = FALSE)
    }
  } else{
    
    message("Elsiever API cannot access this doi")
  }
  
}
