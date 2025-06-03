#' Pull recently retrieved citations from database 
#'
#' This function pulls in unique recently retrieved citations from the soles database to deduplicate 
#' against newly retrieved citations. This ensures that any new citations are not already present in
#' the database (in a different format/ from a different source) before they are added to soles. 
#'
#' @param con connection to db
#' @param prev_months number of months to look back in database - default is 2
#' @return recent citations 
#' 
get_recent_citations <- function(con, prev_months = prev_months){
  
  older_month <- Sys.Date()-(30*prev_months)
  current_date <- Sys.Date()
  
  # Generate a sequence of dates at the start of each month within the range
  dates_sequence <- seq.Date(from = as.Date(cut(older_month, "month")),
                             to = as.Date(cut(current_date, "month")),
                             by = "months")
  
  regex_pattern <- paste(format(dates_sequence, "%m%y"), collapse = '|')
  query <- paste0('SELECT * FROM unique_citations WHERE date ~ \'(', regex_pattern, ')\'')
  recent_citations_in_db <- dbGetQuery(con, query)
  
  # edit source for dedup - in database already
  recent_citations_in_db$source <- "in_db_already"
  
  # make all NA real NAs
  recent_citations_in_db[recent_citations_in_db == "NA" ] <- NA
  recent_citations_in_db[recent_citations_in_db == "" ] <- NA
  
  return(recent_citations_in_db)
}

#' Get new unique citations 
#'
#' @param con connection to db
#' @param new_citations new citations identified
#' @param prev_months how many previous months of citations do you want to dedup against?
#' @return recent citations 
#' @export
#' 
get_new_unique <- function(con, new_citations, prev_months=2){
  
  if(prev_months == 0){
    citations <- new_citations
    
  } else{
    old_citations <- get_recent_citations(con, prev_months=prev_months)
    citations <- rbind(new_citations, old_citations)
  }
  
  citations$isbn <- citations$issn 
  citations$label <- citations$date
  citations$record_id <- citations$uid
  
  if(length(citations$record_id) > 50000){
    
    message("Splitting up dataframe and running multiple deduplications due to size...")
    
    deduplicated_dataframes <- list()
    
    citations <- citations %>%
      arrange(year, title, author)
    
    split_citations <- split(citations, ceiling(seq(nrow(citations))/50000))
    
    for(i in 1:length(split_citations)){
      
      # Perform deduplication for the current year's citations
      dedup_results <- ASySD::dedup_citations(split_citations[[i]], merge_citations = TRUE, keep_source = "in_db_already")
      
      # Append deduplicated results to the list
      deduplicated_dataframes[[i]] <- dedup_results$unique
    } 
    
    dedup_results <- bind_rows(deduplicated_dataframes)
    
    # get unique citations
    new_unique <- dedup_results
    
  } else {
    
    dedup_results <- ASySD::dedup_citations(citations, merge_citations = TRUE, keep_source = "in_db_already")
    
    # get unique citations
    new_unique <- dedup_results$unique
    
  }
  
  # fix DOIs
  new_unique <- format_doi(new_unique)
  
  date <- format(Sys.Date(), "%d%m%y")
  
  
  if (!("record_ids" %in% colnames(new_unique))) {
    
    warning("No duplicates detected... returning original dataframe")
    # ensure date is present
    new_unique$issn <- new_unique$isbn
    
    new_unique <- new_unique %>%
      select(uid, source, author, year, journal, doi, title,
             pages, volume, abstract, isbn, keywords,
             secondarytitle, url, date, issn, pmid, ptype, 
             source, author_country, number, author_affiliation)
    
    new_unique <- new_unique %>%
      filter(!grepl("in_db_already", .$source))
    
    return(new_unique)
  }
  
  message("updating citation source match table with new identifiers...")
  
  # # keep accession info
  matching_ids <- new_unique %>%
    select(uid, record_ids, doi, source, pmid) %>%
    mutate(wos_accession = ifelse(grepl("wos:", record_ids),
                                  paste(gsub(".*wos:", "", record_ids)),
                                  "")) %>%
    mutate(wos_accession = gsub(",.*", "", wos_accession)) %>%
    mutate(scopus_accession = ifelse(grepl("scopus-", record_ids),
                                     paste(gsub(".*scopus-", "", record_ids)),
                                     "")) %>%
    mutate(scopus_accession = gsub(",.*", "", scopus_accession)) %>%
    select(-source, -record_ids)
  
  matching_ids <- matching_ids %>% mutate_all(na_if,"")
  
  # if citation_source_match doesn't exist create it
  tbls <- dbListTables(con)
  if("citation_source_match" %in% tbls){
    existing_match_ids <- dbReadTable(con, "citation_source_match")
    match_ids <- matching_ids
    match_comb <- dplyr::full_join(existing_match_ids, match_ids)
  } else {
    existing_match_ids <- dbReadTable(con, "unique_citations")
    existing_match_ids <- existing_match_ids %>%
      select(uid) %>%
      mutate(pmid = NA, 
             wos_accession =NA,
             scopus_accession =NA)
    match_ids <- matching_ids
    match_comb <- dplyr::full_join(existing_match_ids, match_ids)
  }
  
  match_comb <- match_comb %>%
    group_by(uid) %>%
    arrange(wos_accession) %>%
    mutate(wos_accession = first(wos_accession)) %>%
    arrange(scopus_accession) %>%
    mutate(scopus_accession = first(scopus_accession)) %>%
    arrange(pmid) %>%
    mutate(pmid = first(pmid)) %>%
    arrange(doi) %>%
    mutate(doi = first(doi)) %>%
    unique() %>%
    ungroup()
  
  dbWriteTable(con, "citation_source_match", match_comb, overwrite=TRUE)
  
  message("formatting unique citations dataframe...")
  date <- format(Sys.Date(), "%d%m%y")
  
  # ensure date is present
  new_unique$issn <- new_unique$isbn
  
  new_unique <- new_unique %>%
    select(uid, source, author, year, journal, doi, title,
           pages, volume, abstract, isbn, keywords,
           secondarytitle, url, date, issn, pmid, ptype, 
           source, author_country, number, author_affiliation)
  
  new_unique <- new_unique %>%
    filter(!grepl("in_db_already", .$source)) 
  message(paste0("identified ", nrow(new_unique), " new unique citations"))
  
  return(new_unique)
  
}


#' Get new unique citations and perform deduplication
#'
#' This function processes a dataframe of citations, deduplicates them, and provides unique and manually deduplicated citation results.
#'
#' @param citations A dataframe containing citation information.
#' @param arrange_by Column in the dataframe to arrange data by, before duplication starts. Choose from "year", "author" or "title".
#' @param keep_source source to preferentially retain over other sources
#' @import dplyr
#' @return A list with components:
#'   \describe{
#'     \item{unique}{Dataframe containing unique citations.}
#'     \item{manual_dedup}{Dataframe containing citations to be manually checked for duplicates.}
#'     \item{match_ids}{Dataframe containing updated matching identifiers.}
#'   }
#' @export

dedup_first_search <- function(citations, arrange_by = NULL, keep_source="pubmed"){
  # remove 100% duplicate citations
  citations <- unique(citations)
  
  # remove duplicate uids
  citations <- citations %>%
    group_by(uid) %>%
    slice_head() %>%
    ungroup()
  
  # remove citations with the same pmid
  citations_no_pmid <- citations %>% 
    filter(pmid == "" | is.na(pmid))
  
  # get studies with a pmid and slice to keep only one per pmid 
  # note that studies with no pmid are not included here to avoid keeping only one study with NA pmid / missing pmid
  citations_with_pmid <- citations %>%
    filter(!uid %in% citations_no_pmid$uid) %>% 
    group_by(pmid) %>% 
    slice_head() %>% 
    ungroup()
  
  citations <- rbind(citations_no_pmid, citations_with_pmid)
  
  citations$isbn <- citations$issn 
  citations$label <- citations$date
  citations$record_id <- citations$uid
  
  if(length(citations$record_id) > 50000){
    
    message("Splitting up dataframe and running multiple deduplications due to size...")
    
    unique <- list()
    manual <- list()
    
    
    if (is.null(arrange_by)) {
      
      citations <- citations %>%
        arrange(year, title, author)
      
    } else {
      
      citations <- citations %>%
        arrange(!!rlang::sym(arrange_by))
    }
    
    
    split_citations <- split(citations, ceiling(seq(nrow(citations))/50000))
    
    for(i in 1:length(split_citations)){
      
      # Perform deduplication for the current year's citations
      dedup_results <- ASySD::dedup_citations(split_citations[[i]], merge_citations = TRUE, keep_source = keep_source)
      
      # Append deduplicated results to the list
      unique[[i]] <- dedup_results$unique
      manual[[i]] <- dedup_results$manual
    } 
    
    res_unique <- bind_rows(unique)
    res_manual <- bind_rows(manual)
    
  } else {
    
    dedup_results <- ASySD::dedup_citations(citations, merge_citations = TRUE, keep_source = keep_source)
    
    res_unique <- dedup_results$unique
    res_manual <- dedup_results$manual
    
  }
  
  if (!("record_ids" %in% colnames(res_unique))){
    
    warning("No duplicates detected... returning original dataframe")
    # ensure date is present
    res_unique$issn <- res_unique$isbn
    
    res_unique <- res_unique %>%
      select(uid, source, author, year, journal, doi, title,
             pages, volume, abstract, isbn, keywords,
             secondarytitle, url, date, issn, pmid, ptype, 
             source, author_country, number, author_affiliation)
    
    res_unique <- format_doi(res_unique)
    
    return(list("unique" = res_unique))
    
  }
  
  # correct DOIs
  res_unique <- format_doi(res_unique)
  res_manual <- format_doi(res_manual) 
  date <- format(Sys.Date(), "%d%m%y")
  
  message("updating citation source match table with new identifiers...")
  
  # # keep accession info
  matching_ids <- res_unique %>%
    select(uid, record_ids, doi, source, pmid) %>%
    mutate(wos_accession = ifelse(grepl("wos", record_ids),
                                  paste(gsub(".*wos.", "", record_ids)),
                                  "")) %>%
    mutate(wos_accession = gsub(",.*", "", wos_accession)) %>%
    mutate(scopus_accession = ifelse(grepl("scopus-", record_ids),
                                     paste(gsub(".*scopus-", "", record_ids)),
                                     "")) %>%
    mutate(scopus_accession = gsub(",.*", "", scopus_accession)) %>%
    select(-source, -record_ids)
  
  matching_ids <- matching_ids %>% mutate_all(na_if,"")
  
  match_comb <- matching_ids %>%
    group_by(uid) %>%
    arrange(wos_accession) %>%
    mutate(wos_accession = first(wos_accession)) %>%
    arrange(scopus_accession) %>%
    mutate(scopus_accession = first(scopus_accession)) %>%
    arrange(pmid) %>%
    mutate(pmid = first(pmid)) %>%
    arrange(doi) %>%
    mutate(doi = first(doi)) %>%
    unique() %>%
    ungroup()
  
  message("formatting unique citations dataframe...")
  date <- format(Sys.Date(), "%d%m%y")
  
  # ensure date is present
  res_unique$issn <- res_unique$isbn
  
  res_unique <- res_unique %>%
    select(uid, source, author, year, journal, doi, title,
           pages, volume, abstract, isbn, keywords,
           secondarytitle, url, date, issn, pmid, ptype, 
           source, author_country, number, author_affiliation)
  
  message(paste0("identified ", nrow(res_unique), " unique citations"))
  
  # catch to ensure all dois are lower case
  res_unique$doi <- tolower(res_unique$doi)
  
  return(list("unique" = res_unique,
              "manual_dedup" = res_manual,
              "match_ids" = match_comb))
  
}