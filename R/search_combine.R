#' Combine searches
#'
#' This function combines search results across searches
#' @param ... One or more data frame objects to be combined. Each data frame should represent search results
#'        and can have different sets of columns. The function automatically aligns these data frames by column names,
#'        appending missing columns as `NA`.
#'        
#' @importFrom plyr rbind.fill
#' @import dplyr
#' @return combined search results with all the correct columns
#' @export
combine_searches <- function(...){
  
  # merge all searches in one dataframe
  combined <-  plyr::rbind.fill(...) 
  combined$number <- combined$issue
  # define relevant columns for soles
  x <- c("uid", "source", "author", "year", "journal", "doi", "title",
         "pages", "volume", "abstract", "isbn", "keywords",
         "secondarytitle", "url", "date", "issn", "pmid", "ptype", 
         "source", "author_country", "number", "author_affiliation")
  # append missing relevant columns to data frame
  combined[x[!(x %in% colnames(combined))]] = NA
  # select relevant columns
  combined <- combined %>%
    dplyr::select(uid, source, author, year, journal, doi, title,
                  pages, volume, abstract, isbn, keywords,
                  secondarytitle, url, date, issn, pmid, ptype, 
                  source, author_country, number, author_affiliation) 
  
}

#' Update retrieved citations table
#'
#' This function updates the retrieved citations table with newly retrieved studies
#'
#' @param con connection to db
#' @param citations df of all new citations retrieved
#' @return update of the retrieved_citations table 
#' @export
#' 
check_if_retrieved <- function(con, citations){
  
  # get table of retrieved uids
  retrieved <- DBI::dbReadTable(con, "retrieved_citations")
  
  # remove already retrieved citations
  new_citations <- citations %>%
    filter(!uid %in% retrieved$uid)
  
  new_citations_write <- new_citations %>%
    select(uid, date) %>%
    rename(label = date)
  
  print(paste0(length(new_citations_write$uid), " new citations identified and written to retrieved_citations table"))
  dbWriteTable(con, "retrieved_citations", new_citations_write, append=TRUE)
  
  return(new_citations)
}
