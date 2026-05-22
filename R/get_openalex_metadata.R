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
#' @return Tables "discipline_tag", "funder_grant_tag", "institution_tag", "citation_count_tag", "retraction_tag", "oa_tag" will be updated with n number of new rows containing metadata. If no data is retrieved for these n citations, "Unknown" will be returned in the relevant columns. 
#' @export
#' @examples
#' \dontrun{
#' # Example usage:
#' get_openalex_metadata(con)
#' get_openalex_metadata(con, fill_table = "oa_tag", n = 200)
#' }

get_openalex_metadata <- function(con, fill_table = NULL, n = 100){
  
  # Check con contains connection info
  if(!inherits(con, "PqConnection")){
    stop("'con' is not a valid database connection")
  }
  
  # Check fill_table input is valid
  if(!is.null(fill_table)){
    if (!(all(fill_table %in% c(
      "funder_grant_tag", 
      "discipline_tag", 
      "institution_tag", 
      "citation_count_tag", 
      "retraction_tag", 
      "oa_tag")))) {
      stop("'fill_table' is not valid database table name.")
    }
  }
  
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
  
  # Get data
  included <- dbReadTable(con, "study_classification") %>% filter(decision == "include")
  dois <- tbl(con, "unique_citations") %>% select(uid, doi) %>% collect()

  if (is.null(fill_table)){
    
    # Filter for rows containing no data ----
    citations_missing_data <- dois  %>%
      filter(uid %in% included$uid) %>%
      # Filter to get DOIs that are missing in AT LEAST ONE table
      filter(!doi %in% institution_full$doi | !doi %in% discipline_full$doi |
               !doi %in% citation_count_full$doi | !doi %in% funder_full$doi | 
               !doi %in% open_access_full$doi | !doi %in% retraction_full$doi) %>% 
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
  
  message(length(citations_missing_data$doi), " records left to tag!")
  
  if(length(citations_missing_data$doi) < 1) {
    message("Done!")
    return(citations_missing_data)
  } else if(length(citations_missing_data$doi) > n) {
    message("Tagging the first ", n,  " records...")
    citations_missing_data <- citations_missing_data[1:n,]
  } else {
    message("Tagging all remaining records...")
  }
  
  
  # Use the doi's with no discipline (which should also have no funder data) to search OpenAlex
  res <- NULL
  
  #  Query OpenAlex using DOI
  for(i in 1:length(citations_missing_data$doi)){
    
    # Try to retrieve information using title search
    new <- suppressWarnings(
      try(openalexR::oa_fetch(
          entity = "works",
          doi = citations_missing_data$doi[i]),
        silent = TRUE
      )
    )
    
    # Bind results together
    if(is.data.frame(new)){
      # Print success message to console
      message(sprintf("Success: Found OpenAlex data for DOI: '%s'", citations_missing_data$doi[i]))
      # If records retrieved, bind result to object
      res <- plyr::rbind.fill(res, new)
    } else {
      # Print failure message to console
      message(sprintf("Error: No data fetched for DOI: '%s'", citations_missing_data$doi[i]))
    }
    
    
  }
 
  if(is.null(res)){
    
    message("Couldn't tag any more records.")
    return(citations_missing_data)
  }
  
  # Unnest author data, and extract institution info ----
  res_institution <- tryCatch({
    
    res_institution <- res %>% 
      tidyr::unnest(authorships, names_sep = "_") %>%
      filter(authorships_author_position == "first") %>%
      rename(affiliations = authorships_affiliations) %>%
      tidyr::unnest(affiliations, names_sep = "_") %>%
      select(
        doi, 
        institution_id = affiliations_id, 
        name = affiliations_display_name, 
        ror = affiliations_ror, 
        institution_country_code = affiliations_country_code, 
        type = affiliations_type
      ) %>%
      mutate(
        institution_country_code = toupper(institution_country_code), 
        doi = str_remove(doi, "https://doi.org/"),
        method = "OpenAlex"
      ) %>%
      replace(is.na(.), "Unknown") %>%
      filter(
        !doi %in% institution_full$doi,
        doi %in% citations_missing_data$doi
      )
    
    res_institution_failed <- citations_missing_data %>%
      filter(
        !doi %in% res_institution$doi,
        !doi %in% institution_full$doi
      ) %>%
      mutate(
        institution_id = "Unknown",
        name = "Unknown",
        ror = "Unknown", 
        institution_country_code = "Unknown",
        type = "Unknown",
        method = "OpenAlex"
      )
    
    rbind(res_institution, res_institution_failed)
    
  }, error = function(e){
    
    message(
      paste("ERROR in res_institution:", conditionMessage(e))
    )
    
    NULL
  })
  
  
  # Take results and transform data for discipline_tag
  res_concepts <- tryCatch({
    
    res_concepts <- res %>% 
      tidyr::unnest(concepts, names_sep = "_") %>% 
      select(doi, concepts_display_name, concepts_level, concepts_score) %>% 
      mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
      filter(
        !concepts_score == 0,
        concepts_level == 2 | concepts_level == 1 | concepts_level == 0
      ) %>%
      dplyr::rename(
        main_discipline = concepts_display_name,
        level = concepts_level,
        score = concepts_score
      ) %>% 
      mutate(
        main_discipline = ifelse(score < 0.4, "Unknown", main_discipline),
        method = "OpenAlex"
      ) %>%
      filter(
        !main_discipline == "Unknown",
        !doi %in% discipline_full$doi,
        doi %in% citations_missing_data$doi
      ) %>%
      replace(is.na(.), "Unknown")
    
    res_concepts_failed <- citations_missing_data %>%
      filter(
        !doi %in% res_concepts$doi,
        !doi %in% discipline_full$doi
      ) %>%
      mutate(
        main_discipline = "Unknown",
        score = "Unknown",
        level = "Unknown",
        method = "OpenAlex"
      )
    
    rbind(res_concepts, res_concepts_failed)
    
  }, error = function(e){
    
    message(
      paste("ERROR in res_concepts:", e$message)
    )
    
    NULL
  })


  res_funder <- tryCatch({
    
    if (all(is.na(res$awards))) {
      
      res_awards <- tibble(
        doi = character(),
        funder_name = character(),
        award_id = character()
      )
      
    } else {
      
      res_awards <- res %>%
        dplyr::select(id, doi, awards) %>%
        dplyr::mutate(doi = stringr::str_remove(doi, "https://doi.org/")) %>%
        dplyr::filter(!is.na(awards)) %>% 
        dplyr::mutate(
          awards_long = purrr::map(awards, ~ {
            vec <- .x
            names(vec)[names(vec) == "id"] <- "award_id"
            names(vec)[names(vec) == "funder_display_name"] <- "funder_name"
            tibble(
              field = names(vec),
              value = as.character(vec)
            )
          })
        ) %>%
        dplyr::select(-awards) %>%
        tidyr::unnest(awards_long) %>%
        dplyr::group_by(id, doi, field) %>%
        dplyr::mutate(funder_index = row_number()) %>%
        tidyr::pivot_wider(names_from = field, values_from = value) %>%
        dplyr::ungroup() %>%
        dplyr::select(doi, funder_name, award_id = funder_award_id) %>%
        dplyr::group_by(doi, funder_name) %>%
        dplyr::summarise(
          award_id = paste(unique(award_id), collapse = "; "),
          .groups = "drop"
        )
    }
    
    if (all(is.na(res$funders))) {
      
      res_funder <- tibble(
        doi = character(),
        funder_name = character()
      )
      
    } else {
      
      res_funder <- res %>%
        dplyr::select(doi, funders) %>%
        dplyr::mutate(doi = stringr::str_remove(doi, "https://doi.org/")) %>%
        tidyr::unnest_longer(funders) %>%
        tidyr::unnest_wider(funders) %>%
        dplyr::filter(!is.na(display_name)) %>%
        dplyr::select(doi, funder_name = display_name) %>% 
        dplyr::distinct() %>% 
        dplyr::anti_join(
          res_awards,
          by = c("doi", "funder_name")
        )
    }
    
    res_funder_awards <- bind_rows(res_awards, res_funder) %>% 
      dplyr::mutate(
        method = "OpenAlex",
        award_id = ifelse(is.na(award_id), "Unknown", award_id)
      )
    
    res_funder_failed <- citations_missing_data %>%
      dplyr::filter(
        !doi %in% res_funder$doi,
        !doi %in% funder_full$doi
      ) %>%
      dplyr::mutate(
        funder_name = "Unknown",
        award_id = "Unknown",
        method = "OpenAlex"
      )
    
    rbind(res_funder_awards, res_funder_failed) %>% 
      dplyr::filter(!grepl("https://openalex.org/", funder_name)) %>% 
      dplyr::distinct()
    
  }, error = function(e){
    
    message(
      paste("ERROR in res_funder:", e$message)
    )
    
    NULL
  })
  
  
  # Transform data for citation_count_tag
  res_citation_count <- tryCatch({
    
    res_citation_count <- res %>% 
      select(doi, count = cited_by_count) %>% 
      mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
      mutate(
        method = "OpenAlex",
        date = Sys.Date()
      ) %>% 
      filter(
        !doi %in% citation_count_full$doi,
        doi %in% citations_missing_data$doi
      ) %>%
      replace(is.na(.), "Unknown")
    
    res_citation_count_failed <- citations_missing_data %>%
      filter(
        !doi %in% res_citation_count$doi,
        !doi %in% citation_count_full$doi
      ) %>%
      mutate(
        count = NA,
        method = "OpenAlex",
        date = Sys.Date()
      )
    
    rbind(res_citation_count, res_citation_count_failed)
    
  }, error = function(e){
    
    message(
      paste("ERROR in res_citation_count:", e$message)
    )
    
    NULL
  })
  
  # Transform data for retraction_tag
  res_retraction <- tryCatch({
    
    res_retraction <- res %>% 
      select(doi, is_retracted) %>% 
      mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
      mutate(
        method = "OpenAlex",
        date = Sys.Date()
      ) %>% 
      filter(
        !doi %in% retraction_full$doi,
        doi %in% citations_missing_data$doi
      ) %>%
      replace(is.na(.), "Unknown")
    
    res_retraction_failed <- citations_missing_data %>%
      filter(
        !doi %in% res_retraction$doi,
        !doi %in% retraction_full$doi
      ) %>%
      mutate(
        is_retracted = NA,
        method = "OpenAlex",
        date = Sys.Date()
      )
    
    rbind(res_retraction, res_retraction_failed)
    
  }, error = function(e){
    
    message(
      paste("ERROR in res_retraction:", e$message)
    )
    
    NULL
  })
  
  # Transform data for open access tag
  res_oa <- tryCatch({
    
    res_oa <- res %>% 
      select(doi, is_oa, oa_status) %>% 
      
      # Fix logic in is_oa
      mutate(is_oa = if_else(oa_status == "closed", FALSE, TRUE)) %>%
      
      mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
      
      mutate(method = "OpenAlex") %>% 
      
      filter(
        !doi %in% open_access_full$doi,
        doi %in% citations_missing_data$doi
      ) %>%
      
      replace(is.na(.), "Unknown")
    
    res_oa_failed <- citations_missing_data %>%
      filter(
        !doi %in% res_oa$doi,
        !doi %in% open_access_full$doi
      ) %>%
      mutate(
        is_oa = NA,
        oa_status = "Unknown",
        method = "OpenAlex"
      )
    
    rbind(res_oa, res_oa_failed)
    
  }, error = function(e){
    
    message(
      paste("ERROR in res_oa:", e$message)
    )
    
    NULL
  })
  
  
  # Append tables with new data ----
  if (!is.null(res_institution) && nrow(res_institution) > 0) {
    dbWriteTable(con, "institution_tag", res_institution, append = TRUE)
    message(paste0(nrow(res_institution)," records added to institution_tag"))
  }
  
  if (!is.null(res_concepts) && nrow(res_concepts) > 0) {
    dbWriteTable(con, "discipline_tag", res_concepts, append = TRUE)
    message(paste0(nrow(res_concepts)," records added to discipline_tag"))
    
  }
  
  if (!is.null(res_funder) && nrow(res_funder) > 0) {
    dbWriteTable(con, "funder_grant_tag", res_funder, append = TRUE)
    message(paste0(nrow(res_funder)," records added to funder_grant_tag"))
    
  }
  
  if (!is.null(res_oa) && nrow(res_oa) > 0) {
    dbWriteTable(con, "oa_tag", res_oa, append = TRUE)
    message(paste0(nrow(res_oa)," records added to oa_tag"))
    
  }
  
  if (!is.null(res_citation_count) && nrow(res_citation_count) > 0) {
    dbWriteTable(con, "citation_count_tag", res_citation_count, append = TRUE)
    message(paste0(nrow(res_citation_count)," records added to citation_count_tag"))
    
  }
  
  if (!is.null(res_retraction) && nrow(res_retraction) > 0) {
    dbWriteTable(con, "retraction_tag", res_retraction, append = TRUE)
    message(paste0(nrow(res_retraction)," records added to retraction_tag"))
    
  }
  
  message(paste0(length(citations_missing_data$doi)," records tagged via OpenAlex!"))
  
}
