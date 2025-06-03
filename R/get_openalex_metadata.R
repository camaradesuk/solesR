#' Extract concepts, funder, citation count, institution and open access data from OpenAlex.
#'
#' This function retrieves meta-data.
#'
#' @param con connection to db
#' @param fill_table Name of database table to prioritise tagging
#' @param n Number of studies to be tagged
#'
#' @import DBI
#' @import dplyr
#' @import dbplyr
#' @import tidyr
#' @import stringr
#' @import openalexR
#' @return Tables "discipline_tag", "funder_grant_tag", "institution_tag", "citation_count_tag", "retraction_tag", "article_type", oa_tag" will be updated with n number of new rows containing metadata. If no data is retrieved for these n citations, "Unknown" will be returned in the relevant columns. 
#' @export
#' @examples
#' \dontrun{
#' # Example usage:
#' get_openalex_metadata(con)
#' get_openalex_metadata(con, fill_table = "oa_tag", n = 200)
#' }

get_openalex_metadata <- function(con, fill_table = NULL, n = 100){
  
  # if table doesn't exist, create it ----
  if (!dbExistsTable(con, "funder_grant_tag")) {
    
    
    funder <- data.frame(doi = as.character(), 
                         funder_name=as.character(),
                         award_id = as.character(),
                         method = as.character())
    
    dbWriteTable(con, "funder_grant_tag", funder)
    message("Created funder_grant_tag table.")
    
  }
  
  if (!dbExistsTable(con, "discipline_tag")) {
    
    discipline <- data.frame(doi = as.character(), 
                             main_discipline = as.character(), 
                             level = as.character(),
                             score = as.character(),
                             method = as.character())
    
    dbWriteTable(con, "discipline_tag", discipline)
    message("Created discipline_tag table.")
    
  }
  
  if (!dbExistsTable(con, "institution_tag")) {
    
    institution <- data.frame(doi = as.character(), 
                              institution_id = as.character(), 
                              name = as.character(),
                              ror = as.character(),
                              institution_country_code = as.character(),
                              type = as.character(),
                              method = as.character())
    
    dbWriteTable(con, "institution_tag", institution) 
    message("Created institution_tag table.")
    
  }
  
  if (!dbExistsTable(con, "citation_count_tag")) {
    
    citation_count <- data.frame(doi = as.character(), 
                                 count = as.integer(), 
                                 method = as.character(),
                                 date = as.Date(character()))
    
    dbWriteTable(con, "citation_count_tag", citation_count)
    message("Created citation_count_tag table.")
    
  }
  
  if (!dbExistsTable(con, "retraction_tag")) {
    
    retraction <- data.frame(doi = as.character(), 
                             is_retracted = as.logical(), 
                             method = as.character(),
                             date = as.Date(character()))
    
    dbWriteTable(con, "retraction_tag", retraction)
    message("Created retraction_tag table.")
    
  }
  
  if (!dbExistsTable(con, "article_type")) {
    
    article <- data.frame(doi = as.character(),
                          language = as.character(),
                          type = as.character(),
                          is_paratext = as.logical(),
                          method = as.character())
    
    dbWriteTable(con, "article_type", article)
    message("Created article_type table.")
    
  }
  
  if (!dbExistsTable(con, "oa_tag")) {
    
    open_access <- data.frame(doi = as.character(), 
                              is_oa = as.logical(), 
                              oa_status = as.character(),
                              method = as.character())
    
    dbWriteTable(con, "oa_tag", open_access)
    message("Created oa_tag table.")
    
  }
  
  # Gather the tables in their current state ----
  institution_full <- tbl(con, "institution_tag") %>% 
    collect()
  
  discipline_full <- tbl(con, "discipline_tag") %>% 
    collect()
  
  funder_full <- tbl(con, "funder_grant_tag") %>% 
    collect()
  
  citation_count_full <- tbl(con, "citation_count_tag") %>% 
    collect()
  
  retraction_full <- tbl(con, "retraction_tag") %>% 
    collect()
  
  open_access_full <- tbl(con, "oa_tag") %>% 
    collect()
  
  article_full <- tbl(con, "article_type") %>%
    collect()
  
  # Get data
  included <- dbReadTable(con, "study_classification") %>% filter(decision == "include")
  dois <- tbl(con, "unique_citations") %>% select(uid, doi) %>% collect()
  
  if (is.null(fill_table)){
    
    # Filter for rows containing no data ----
    citations_missing_data <- dois  %>%
      filter(uid %in% included$uid) %>%
      # Filter to get DOIs that are missing in AT LEAST ONE table
      filter(!doi %in% institution_full$doi | !doi %in% discipline_full$doi |
               !doi %in% article_full$doi | !doi %in% citation_count_full$doi |
               !doi %in% funder_full$doi | !doi %in% open_access_full$doi |
               !doi %in% retraction_full$doi) %>% 
      mutate(across(where(is.character), ~na_if(., ""))) %>%
      filter(!(is.na(doi))) %>%
      select(doi) %>%
      distinct()
    
  } else {
    
    # Retrieve database table, from which tagging is to be prioritised
    table <- dbReadTable(con, fill_table)
    
    citations_missing_data <- dois  %>%
      filter(uid %in% included$uid) %>%
      filter(!doi %in% table$doi) %>% 
      mutate(across(where(is.character), ~na_if(., ""))) %>%
      filter(!(is.na(doi))) %>%
      select(doi) %>%
      distinct()
    
  }
  
  print(paste0(length(citations_missing_data$doi), " records left to tag!"))
  
  if(length(citations_missing_data$doi) < 1) {
    message("Done!")
    return(citations_missing_data)
  } else if(length(citations_missing_data$doi) > n) {
    message(paste0("Tagging the first ", n,  " records..."))
    citations_missing_data <- citations_missing_data[1:n,]
  } else {
    message("Tagging all remaining records...")
  }
  
  
  # Use the doi's with no discipline (which should also have no funder data) to search OpenAlex
  res <- NULL
  
  # Create a dataframe with data from openAlex ----
  for(i in 1:length(citations_missing_data$doi)){
    suppressWarnings({
      
      try(new <- openalexR::oa_fetch(
        identifier = NULL,
        entity = "works",
        doi = citations_missing_data$doi[i]),silent=TRUE)
    })
    if(is.data.frame(new)){
      res <- bind_rows(res, new)
    }
  }
  
  if(is.null(res)){
    
    message("Couldn't tag any more records.")
    return(citations)
  }
  
  # Unnest author data, and extract institution info ----
  res_institution <- res %>% 
    unnest(author) %>%
    filter(author_position == "first") %>%
    select(doi, institution_id, name = institution_display_name, ror = institution_ror, institution_country_code, type = institution_type) %>%
    mutate(institution_country_code = toupper(institution_country_code), 
           doi = str_remove(doi, "https://doi.org/"),
           method = "OpenAlex") %>%
    replace(is.na(.), "Unknown") %>%
    filter(!doi%in% institution_full$doi,
           doi %in% citations_missing_data$doi)
  
  res_institution_failed <- citations_missing_data %>%
    filter(!doi %in% res_institution$doi,
           !doi%in% institution_full$doi) %>%
    mutate(institution_id = "Unknown", name = "Unknown", ror = "Unknown", 
           institution_country_code = "Unknown", type = "Unknown", method = "OpenAlex")
  
  res_institution <- rbind(res_institution, res_institution_failed)
  
  # Take results and transform data for discipline_tag
  res_concepts <- res %>% 
    unnest(concepts, names_sep = "_") %>% 
    select(doi, concepts_display_name, concepts_level, concepts_score) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    filter(!concepts_score == 0,
           concepts_level == 2 | concepts_level == 1 | concepts_level == 0) %>%
    dplyr::rename(main_discipline = concepts_display_name,
                  level = concepts_level,
                  score = concepts_score) %>% 
    mutate(main_discipline = ifelse(score < 0.4, "Unknown", main_discipline),
           method = "OpenAlex") %>%
    filter(!main_discipline == "Unknown",
           !doi %in% discipline_full$doi,
           doi %in% citations_missing_data$doi) %>%
    replace(is.na(.), "Unknown")
  
  res_concepts_failed <- citations_missing_data %>%
    filter(!doi %in% res_concepts$doi,
           !doi %in% discipline_full$doi) %>%
    mutate(main_discipline = "Unknown", score = "Unknown", level = "Unknown",
           method = "OpenAlex")
  
  res_concepts <- rbind(res_concepts, res_concepts_failed)
  
  # Take results and transform data for funder_grant_tag
  res_funder <- res %>% 
    select(doi, grants) %>%
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    unnest_longer(grants) %>% 
    filter(!is.na(grants))
  
  if (nrow(res_funder > 0)){
    res_funder %>% 
      filter(!grants_id == "funder")
    
    res_funder$award_id <- NA
    
    if (nrow(res_funder) > 1) {
      for (i in 1:(nrow(res_funder) - 1)) {
        
        if (res_funder$grants_id[i + 1] == "award_id") {
          
          res_funder$award_id[i] <- res_funder$grants[i + 1]
        }
      }
    }
    
    res_funder <- res_funder %>%
      filter(!grants_id == "award_id") %>% 
      select(-grants_id, funder_name = grants, doi) %>%
      mutate(method = "OpenAlex") %>%
      replace(is.na(.), "Unknown") %>%
      filter(!doi %in% funder_full$doi,
             doi %in% citations_missing_data$doi)
    
    res_funder_failed <- citations_missing_data %>%
      filter(!doi %in% res_funder$doi,
             !doi %in% funder_full$doi) %>%
      mutate(funder_name = "Unknown", award_id = "Unknown", method = "OpenAlex")
    
    res_funder <- rbind(res_funder, res_funder_failed)
    
  } else{
    res_funder <- citations_missing_data %>%
      filter(!doi %in% funder_full$doi) %>%
      mutate(funder_name = "Unknown", award_id = "Unknown", method = "OpenAlex")
  }
  
  # Fix duplicate rows with incorrect info
  res_funder <- res_funder %>% filter(!grepl("https://openalex.org/", funder_name))
  
  # Transform data for citation_count_tag
  res_citation_count <- res %>% 
    select(doi, count = cited_by_count) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    mutate(method = "OpenAlex",
           date = Sys.Date()) %>% 
    filter(!doi %in% citation_count_full$doi,
           doi %in% citations_missing_data$doi) %>%
    replace(is.na(.), "Unknown")
  
  res_citation_count_failed <- citations_missing_data %>%
    filter(!doi %in% res_citation_count$doi,
           !doi %in% citation_count_full$doi) %>%
    mutate(count = NA, method = "OpenAlex", date = Sys.Date())
  
  res_citation_count <- rbind(res_citation_count, res_citation_count_failed)
  
  
  # Transform data for retraction_tag
  res_retraction <- res %>% 
    select(doi, is_retracted) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    mutate(method = "OpenAlex",
           date = Sys.Date()) %>% 
    filter(!doi %in% retraction_full$doi,
           doi %in% citations_missing_data$doi) %>%
    replace(is.na(.), "Unknown")
  
  res_retraction_failed <- citations_missing_data %>%
    filter(!doi %in% res_retraction$doi,
           !doi %in% retraction_full$doi) %>%
    mutate(is_retracted = NA, method = "OpenAlex", date = Sys.Date())
  
  res_retraction <- rbind(res_retraction, res_retraction_failed)
  res_retraction$method = "OpenAlex"
  
  
  res_oa <- res %>% 
    select(doi, is_oa, oa_status) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    mutate(method = "OpenAlex") %>% 
    filter(!doi %in% open_access_full$doi,
           doi %in% citations_missing_data$doi) %>%
    replace(is.na(.), "Unknown")
  
  res_oa_failed <- citations_missing_data %>%
    filter(!doi %in% res_oa$doi,
           !doi %in% open_access_full$doi) %>%
    mutate(is_oa = NA, oa_status = "Unknown", method = "OpenAlex")
  
  res_oa <- rbind(res_oa, res_oa_failed)
  
  if (nrow(res_oa > 0)){
    res_oa$method = "OpenAlex"
  }
  
  res_article <- res %>% 
    select(doi, language, type, is_paratext) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    mutate(method = "OpenAlex") %>% 
    filter(!doi %in% article_full$doi,
           doi %in% citations_missing_data$doi) %>%
    replace(is.na(.), "Unknown")
  
  res_article_failed <- citations_missing_data %>%
    filter(!doi %in% res_article$doi,
           !doi %in% article_full$doi) %>%
    mutate(language = "Unknown", type = "Unknown", is_paratext = NA, method = "OpenAlex")
  
  res_article <- rbind(res_article, res_article_failed)
  res_article$method = "OpenAlex"
  
  
  # Append tables with new data ----
  dbWriteTable(con, "institution_tag", res_institution, append = TRUE)
  dbWriteTable(con, "discipline_tag", res_concepts, append = TRUE)
  dbWriteTable(con, "funder_grant_tag", res_funder, append = TRUE)
  dbWriteTable(con, "oa_tag", res_oa, append = TRUE)
  dbWriteTable(con, "article_type", res_article, append = TRUE)
  dbWriteTable(con, "citation_count_tag", res_citation_count, append = TRUE)
  dbWriteTable(con, "retraction_tag", res_retraction, append = TRUE)
  
  message(paste0(length(citations_missing_data$doi)," records tagged via OpenAlex!"))
  
}