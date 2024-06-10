#' Extract open access status from Unpaywall
#'
#' This function retrieves meta-data.
#'
#' @param con connection to db
#' @param email email address to use for Unpaywall
#' @param n number of citations to tag for open access
#' @import DBI
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @export
#'


oa_tag <- function(con, email, n = 100){
  
  # Get records already tagged
  tagged <- DBI::dbReadTable(con, "oa_tag")

  # Identify studies that require tag
  study_to_tag <- tbl(con, "unique_citations") %>%
    left_join(tbl(con, "study_classification"), by = "uid") %>%
    filter(decision == "include") %>%
    select(doi) %>% # select doi first to make collect quicker
    collect() %>%
    distinct() %>%
    filter(!doi %in% tagged$doi) %>%
    filter(!is.na(doi))
  
  # Create vector
  my_dois <- study_to_tag$doi
  
  # Get sample
  if (length(my_dois) < 1){
    message("No more papers to tag by open access status")
    return()
  }else if (length(my_dois) > 100){
    message(paste0("Over 100 papers to tag by open access status. Selecting a sample of ", n))
    my_dois <- sample(my_dois, n)
  }else{
    my_dois <- my_dois
  }
  
  # use oadoi_fetch() to get Unpaywall data on those DOIs
  # my_dois_oa <- roadoi::oadoi_fetch(dois = my_dois, email) %>%
  #   select(doi, is_oa, oa_status) %>%
  #   mutate(method = "Unpaywall")
  # 
  
  my_dois_oa <- purrr::map(my_dois, .f = purrr::safely(function(x)
    roadoi::oadoi_fetch(dois = x, email = email)
  ))
  
  my_dois_oa <- purrr::map_df(my_dois_oa, "result")
  
  my_dois_oa <- my_dois_oa %>%
  select(doi, is_oa, oa_status) %>%
    mutate(method = "Unpaywall")
  
  unknown_oa <- data.frame(doi = my_dois) %>%
    filter(!doi %in% my_dois_oa$doi) %>%
    select(doi) %>%
    mutate(is_oa = NA,
           oa_status = "unknown",
           method = NA)
  
  my_dois_oa <- rbind(my_dois_oa, unknown_oa)
  
  # Write to database
  dbWriteTable(con, "oa_tag", my_dois_oa, append = T)

  to_write <- length(unique(my_dois_oa$doi))
  message("written ", to_write, " citations with OA status to db")
}
