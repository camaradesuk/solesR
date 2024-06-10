#' This function creates the required tables for the soles database
#' 
#' @param con Connection to db
#' @param overwrite Whether to overwrite an existing table. Default is `FALSE`.
#' 
#' @examples
#' \dontrun{
#' create_tables(con)
#' create_tables(con, overwrite = TRUE)
#' }
#' 
#' @export
#' 

create_tables <- function(con, overwrite = FALSE) {
  
  # unique citations table
  unique_citations_df <- data.frame(
    uid = character(),
    doi = character(),
    author = character(),
    year = character(),
    journal = character(),
    title = character(),
    abstract = character(),
    volume = character(),
    number = character(),
    pages = character(),
    isbn = character(),
    keywords = character(),
    url = character(),
    date = character(),
    issn = character(),
    source = character(),
    author_country = character(),
    author_affiliation = character(),
    ptype = character(),
    pmid = character(),
    secondarytitle = character()
  )
  
  # retrieved citations table
  retrieved_citations_df <- data.frame(
    uid = character(),
    label = character()
  )
  
  # study classification table
  study_classification_df <- data.frame(
    uid = character(),
    name = character(),
    decision = character(),
    score = numeric(),
    date = character(),
    cid = numeric(),
    type = character()
  )
  
  # ml performance table
  ml_performance_df <- data.frame(
    threshold = numeric(),
    utility = numeric(),
    specificity = numeric(),
    sensitivity = numeric(),
    precision = numeric(),
    f1 = numeric(),
    balanced_accuracy = numeric(),
    cid = numeric()
  )
  
  # full texts table
  full_texts_df <- data.frame(
    status = character(),
    doi = character(),
    path = character()
  )
  
  # pico tag table
  pico_tag_df <- data.frame(uid = character(),
                            regex_id = double(),
                            method = character(),
                            frequency = character(),
                            strings = character())
  
  list_tables <- c("unique_citations", "retrieved_citations","study_classification",
                   "ml_performance","full_texts", "pico_tag")
  
  db_tables <- DBI::dbListTables(con)
  
  for(t in list_tables){
    
    t_df <- paste0(t, "_df")
    
    if(t %in% db_tables)
      if (overwrite) {
        DBI::dbWriteTable(con, t, eval(parse(text = t_df)), overwrite = TRUE)
        cat("\n", t, "table overwritten")
      } else if (!overwrite){
        cat(t, "table already exists. Use `overwrite = TRUE` to overwrite it.")
      }
    
    if(!t %in% db_tables) {
      DBI::dbWriteTable(con, t, eval(parse(text = t_df)))
      cat("\n", t, "table created")
    }
  }
}

#' Load a search export file, determine its source and extract date from filename.
#' 
#' @param file A file containing search results. Valid sources are all literature databases supported in soles:
#' pubmed, wos, scopus, medline, ovid, embase and psychinfo.
#'
#' @keywords internal
#' 

read_search <- function(file) {
  
  # determine database source
  source <- stringr::str_extract(
    file, 
    "pubmed|wos|scopus|psychinfo|medline|ovid|embase")
  
  filename <- as.character(file)
  
  if(is.na(source)) 
    cat(filename, "input type not currently supported")
  
  # read search file
  dat <- soles::manual_upload(file, source = source)
  
}


#' This function reads in search files in the soles project directory and populates the `unique_citations` and `retrieved_citations tables`.
#' `setup_soles()` expects that the required tables in the project database already exist. See `create_tables()`.
#'
#' @import dplyr
#' @import osfr
#' @param con Connection to db
#' @param folder The name of the folder in the project directory where all search export files are saved. 
#' @param master_node OSF master node
#' @details The function expects that within the main search folder, each sub-folder is named after the corresponding database source. 
#' Valid sub-folder names are: pubmed, medline, wos, scopus, ovid, psychinfo, embase.
#' The function also expects that each search file within the sub-folder follows the consistent naming convention `source_date`, where `source` 
#' is the literature database and `date` corresponds to the date the search was conducted in `ddmmyyyy` format.
#' For example, the following would be valid search export file names: `pubmed_01012023.txt`, `scopus_15102022.bib`. 
#' If multiple export files from the same search on the same date exist, these can be distinguished by numbering the searches 
#' e.g. `pubmed1_01012023.txt`, `pubmed2_01012023.txt`, etc. Full path example: `manual_search/pubmed/pubmed1_01012023.txt`
#'
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' setup_soles(con, "path/to/search/folder")
#' setup_soles(con, "manual_search")
#' }
#'
#' @export

setup_soles <- function(con, folder, master_node){
  
  # check if db is empty
  db_tables <- DBI::dbListTables(con)
  if (length(db_tables) < 1) {
    stop("No tables found in database. Use create_tables() before setup_soles().")
  }
  
  # combine all search files
  message("Reading all search files")
  files <- list.files(folder, recursive = T, full.names = T)
  search_results <- do.call(
    rbind,
    lapply(files, read_search)
  )
  
  # show table with search file info
  file_summary <- search_results %>% 
    group_by(filename) %>% 
    summarize(n=length(unique(uid)))
  
  message('The following records were found in each file.')
  print(file_summary)
  
  # Ask for confirmation to proceed
  proceed <- readline("Do you want to proceed? y/n ")
  
  if (grepl("y", proceed)) {
    
    # retrieved citations
    soles::check_if_retrieved(con, search_results) 
    
    # deduplicate with asysd
    all_unique <- suppressWarnings(soles::dedup_first_search(search_results))
    all_unique <- all_unique$unique
    
    n_unique <- length(all_unique$uid)
    
    # write first search results to database
    cat("Writing", n_unique, "citations to unique_citations table")
    DBI::dbWriteTable(con, "unique_citations", all_unique, append = TRUE)
    
  }
  
  # get pico tables from OSF
  dir.create("temp")
  
  try({
    osfr::osf_retrieve_node(master_node) %>% 
      osfr::osf_ls_files() %>% 
      osfr::osf_download("temp")
  })
  
  try(pico_ontology <- read.csv("temp/pico_ontology.csv"))
  try(pico_dictionary <- read.csv("temp/pico_dictionary.csv"))
  
  # write tables to db
  try(DBI::dbWriteTable(con, "pico_ontology", pico_ontology))
  try(DBI::dbWriteTable(con, "pico_dictionary", pico_dictionary))
  
  unlink("temp", recursive = TRUE)
  
}

