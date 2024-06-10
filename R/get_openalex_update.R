#' Update citation count and retraction data from OpenAlex.
#'
#' This function retrieves and updates meta-data on DOIs where metadata was last retrieved over 6 months ago.
#'
#' @param con connection to db
#'
#' @import DBI
#' @importFrom plyr rbind.fill
#' @import tidyr
#' @import openalexR
#' @import dplyr
#' @import dbplyr
#' @import stringr
#' @import openalexR
#' @return Tables "citation_count_tag", "retraction_tag" will be overwritten with updated with data. . 
#' @export
#' @examples
#' \dontrun{
#' # Example usage:
#' get_openalex_update(con)
#' }

get_openalex_update <- function(con){
  
  
  # Gather the tables in their current state ----
  citation_count_full <- tbl(con, "citation_count_tag") %>% 
    collect()
  
  retraction_full <- tbl(con, "retraction_tag") %>% 
    collect()
  
  # Filter data retrieved more than 6 months ago
  citation_count_old <- citation_count_full %>% select(doi, date) %>%
    filter(date < Sys.Date() - months(6))
  
  retraction_old <- retraction_full %>% select(doi, date) %>%
    filter(date < Sys.Date() - months(6))
  
  citations_old_data <- rbind(citation_count_old, retraction_old) %>%
    distinct()
  
  print(paste0(nrow(citations_old_data), " citations to update!"))
  
  # Sample data
  if(length(citations_old_data$doi) < 1) {
    message("Done!")
    return(citations_old_data)
  } else if(length(citations_old_data$doi) > 100) {
    message("Updating the first 100 records...")
    citations_old_data <- citations_old_data[1:100,]
  } else {
    message("Updating all remaining records...")
  }
  
  
  # Use the doi's with no discipline (which should also have no funder data) to search OpenAlex
  res <- NULL
  
  # Create a dataframe with data from openAlex ----
  for(i in 1:length(citations_old_data$doi)){
    suppressWarnings({
      
      try(new <- openalexR::oa_fetch(
        identifier = NULL,
        entity = "works",
        doi = citations_old_data$doi[i]),silent=TRUE)
    })
    if(is.data.frame(new)){
      res <-  plyr::rbind.fill(res, new)
    }
  }
  
  if(is.null(res)){
    
    message("Couldn't update any more records.")
    return(citations)
  }
  
  
  # Transform data for citation_count_tag
  res_citation_count <- res %>% 
    select(doi, count = cited_by_count) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    mutate(method = "OpenAlex",
           date = Sys.Date()) %>% 
    filter(doi %in% citations_old_data$doi) %>%
    replace(is.na(.), "Unknown")
  
  res_citation_count_failed <- citations_old_data %>%
    filter(!doi %in% res_citation_count$doi) %>%
    mutate(count = "Unknown", method = "OpenAlex", date = Sys.Date())
  
  res_citation_count <- rbind(res_citation_count, res_citation_count_failed)
  
  
  # Transform data for retraction_tag
  res_retraction <- res %>% 
    select(doi, is_retracted) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    mutate(method = "OpenAlex",
           date = Sys.Date()) %>% 
    filter(doi %in% citations_old_data$doi) %>%
    replace(is.na(.), "Unknown")
  
  res_retraction_failed <- citations_old_data %>%
    filter(!doi %in% res_retraction$doi) %>%
    mutate(is_retracted = "Unknown", method = "OpenAlex", date = Sys.Date())
  
  res_retraction <- rbind(res_retraction, res_retraction_failed)
  res_retraction$method = "OpenAlex"
  
  
  # Combine new data with what is alreay in tables
  citation_count_updated <- citation_count_full %>%
    filter(!doi %in% res_citation_count$doi)
  
  citation_count_updated <- rbind(citation_count_updated, res_citation_count)
  
  retraction_updated <-  retraction_full %>%
    filter(!doi %in% res_retraction$doi)
  
  retraction_updated <- rbind(retraction_updated, res_retraction)
  
  # Overwrite tables with new data ----
  dbWriteTable(con, "citation_count_tag", citation_count_updated, overwrite = TRUE)
  dbWriteTable(con, "retraction_tag", retraction_updated, overwrite = TRUE)
  
  message(paste0(length(citations_missing_data$doi)," records updated via OpenAlex!"))
  
}

