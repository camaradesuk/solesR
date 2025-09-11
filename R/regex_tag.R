#' Tag records using regex
#' 
#' Tag SOLES records using title and abstract or full (PDF or XML) text
#'
#' @param con valid database connection
#' @param tag_type type of tagging, e.g. species or model
#' @param tag_method method of tagging, either tiab or fulltext
#' @param tag_main_category set to all to tag all categories or sepcific category
#' @param retMax maximum numer of recrods to tag; default 1000; max 5000
#' @param ignore_case logical; whether to globally ignore case in regex
#' @param extract_strings logical; whether to extract strings where regex matches occur
#'
#' @return database appended with newly tagged records
#' @export
#' 
#' @import dplyr
#' @import DBI
#' @import stringr
#' @import readtext
#' @import quanteda
#' @import xml2
#' @import tidypmc
#'
regex_tag <- function(con, tag_type, tag_method, tag_main_category = "all", retMax = 1000, ignore_case = FALSE, extract_strings = TRUE){
  
  # Still to test:
  # Check database structure
  # Check output format (e.g. not tagged handling)
  # TXT from PDF is not saved -- or should it be a separate function?
  # Check for typos or errors
  # Ignore_case and extract_strings are not yet used in function
  
  # Check con is valid
  if(!inherits(con, "PqConnection")){
    stop("'con' is not a valid database connection")
  }
  
  # Check tag_method is valid (is tiab or fulltext)
  if(tag_method != "tiab" & tag_mehod != "fulltext"){
    stop("tag_method is invalid; valid options are 'tiab' or 'fulltext'")
  }
  
  # Check ignore case and extract_strings are Booloean
  if(!is.logical(ignore_case) | !is.logical(extract_strings)){
    stop("ignore_case and extract_strings should be logical")
  }
  
  # Check if retMax is a positive integer and exit if not
  if (is.numeric(retMax) == FALSE | retMax %% 1 != 0 | retMax < 0) {
    stop("retMax is not a whole number")
  }
  
  # Check retMax and exit if above maximum
  if (retMax > 5000) {
    stop("retMax is too high, max is 5000; default is 1000")
  }
  
  # Read in pico_ontology and dictionary
  dictionary <- dplyr::tbl(con, "pico_dictionary") %>%
    dplyr::left_join(dplyr::tbl(con, "pico_ontology"), by = c("id" = "regex_id")) %>%
    dplyr::filter(type == tag_type) %>%
    dplyr::collect()
  
  tagged <- dplyr::tbl(con, "pico_ontology") %>% 
    dplyr::filter(type == tag_type) %>% 
    dplyr::left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
    dplyr:select(uid, method, regex_id) %>% 
    dplyr::collect()
  
  # Filter by main_category
  if(main_category != "all"){
    dictionary <- dictionary %>%
      dplyr::filter(main_category == tag_main_category)
    tagged <- tagged %>%
      dplyr::filter(main_category == tag_main_category)
  }
  
  # Get IDs for unknown
  unknown_id <- tbl(con, "pico_ontology") %>% filter(type == tag_type) %>% filter((main_category == "Unknown") & grepl("^999999", as.character(regex_id))) %>% collect()
  unknown_id <- unknown_id$regex_id
  
  # Remove unknown from dictionary
  dictionary <- dictionary %>%
    filter(!id %in% unknown_id)
  
  # Get included records
  included_records <- dplyr::tbl(con, "unique_citations") %>%
    dplyr::select(uid, doi, title, abstract) %>%
    dplyr::left_join(tbl(con, "study_classification"), by = "uid") %>%
    dplyr::filter(decision == "include") %>%
    dplyr::select(uid, doi, title, abstract) %>%
    collect()
  
  if(tag_method == "tiab"){
    
    # Get previously tagged records
    done_tiab <- tagged %>%
      dplyr::filter(method == "tiab_regex")
    
    # Get records to tag
    to_tag_tiab <- included_records %>%
      dplyr::filter(filter(!uid %in% done_tiab$uid)) %>%
      dplyr::select(uid, title, abstract) %>%
      # Remove records without a title or abstract
      dplyr::filter(is.na(title)) %>%
      dplyr::filter(is.na(abstract))
    
    # Get sample to tag
    if(nrow(to_tag_tiab) < 1){
      stop("No more records to tag by title / abtract")
    } else if(nrow(to_tag_tiab > retMax)){
      message(nrow(to_tag_tiab), " records to tag by title / abstract; tagging the first ", retMax)
      # Filter
      to_tag_tiab <- head(to_tag_tiab, retMax)
    } else{
      message(nrow(to_tag_tiab), " records to tag by title / abstract")
    }
    
    # Create title and abstract text column
    to_tag_tiab$text <- paste(to_tag_tiab$title, to_tag_tiab$abstract, sep = ". ")
    # Remove instances of teo periods nexts to each other
    to_tag_tiab$text <- gsub("\\.\\.|\\. \\.", ".", to_tag_tiab$text)
    # Select relevant columns
    to_tag_tiab <- to_tag_tiab %>% select(uid, text)
    
    # Create text corpus
    tiab_corpus <- quanteda::corpus(to_tag_tiab$text, docnames = to_tag_tiab$uid)
    
    # Tokenise by sentence
    tiab_tokens <- quanteda::tokens(tiab_corpus, what = "sentence")
    
    # Create empty results object
    tiab_results <- NULL
    
    # Run over each regex in dictionary
    for (i in 1:nrow(dictionary)){
      # Check for match
      try(tiab_match <- quanteda::kwic(tiab_tokens, dictionary$regex[i], window = 1, valuetype = "regex"))
      # Collect data if match found
      if(!is.null(tiab_match)){
        tiab_match <- as.data.frame(tiab_match)
        tiab_match$pattern <- as.character(tiab_match$pattern)
        tiab_match <- tiab_match %>%
          mutate(name = dictionary$name[i],
                 type = dictionary$type[i],
                 match = stringr::str_extract(tiab_match$keyword, tiab_match$pattern))
        tiab_results <- rbind(tiab_results, tiab_match)
      }
    }
    
    # Write results to database
    DBI::dbWriteTable(con, "pico_tag", append = TRUE)
    
  } else if(tag_method == "fulltext"){
    
    # Get previously tagged records
    done_fulltext <- tagged %>%
      dplyr::filter(method == "fulltext_regex")
    
    # Get records to tag
    to_tag_fulltext <- included_records %>%
      dplyr::filter(filter(!uid %in% done_fulltext$uid)) %>%
      dplyr::select(uid, doi) %>%
      dplyr::left_join(dplyr::tbl(con, "full_texts"), by = "doi") %>%
      dplyyr::select(uid, doi, ft_path_full, ft_ext)
    
    # Get sample to tag
    if(nrow(to_tag_fulltext) < 1){
      stop("No more records to tag by fulltext")
    } else if(nrow(to_tag_fulltext > retMax)){
      message(nrow(to_tag_fulltext), " records to tag by fulltext; tagging the first ", retMax)
      # Filter
      to_tag_fulltext <- head(to_tag_fulltext, retMax)
    } else{
      message(nrow(to_tag_fulltext), " records to tag by fulltext")
    }
    
    # Subset xml full texts
    to_tag_fulltext_xml <- to_tag_fulltext %>%
      dplyr::filter(ft_ext == "xml")
    
    # Subset pdf full texts
    to_tag_fulltext_pdf <- to_tag_fulltext %>%
      dplyr::filter(ft_ext == "pdf")
    
    # Read in xml
    if(nrow(to_tag_fulltext_xml) > 0){
      # Create empty results object
      xml_texts <- NULL
      for (i in 1:nrow(to_tag_fulltext_xml)){
        xml <- xml2::read_xml(to_tag_fulltext_xml$ft_path_full[i])
        xml <- tidypmc::pmc_text(xml) %>%
          mutate(uid = to_tag_fulltext_xml$uid[i])
        xml_texts <- rbind(xml_texts, xml)
      }
    }
    
    # Read in pdf
    if(nrow(to_tag_fulltext_pdf) > 0){
      # Create empty results object
      pdf_texts <- NULL
      for (i in 1:nrow(to_tag_fulltext_pdf)){
        pdf <- readtext::readtext(to_tag_fulltext_pdf$ft_path_full[i]) %>% mutate(doc_id = to_tag_fulltext_pdf$uid[i])
        pdf_texts <- rbind(pdf_texts, pdf)
      }
    }
    
    # Create empty results objects
    xml_results <- NULL
    pdf_results <- NULL
    
    # Run regex on XML
    if(!is.null(xml_texts)){
      for (i in 1:nrow(dictionary)){
        try(xml_match <- tidypmc::separate_text(xml_texts, dictionary$regex[i]))
        if(!is.null(xml_match)){
          xml_match <- xml_match %>%
            mutate(name = dictionary$name[i],
                   type = dictionary$type[i])
          xml_results <- rbind(xml_results, xml_match)
        }
      }
    }
    
    # Run regex on pdf
    if(!is.null(pdf_texts)){
      # Create corpus
      pdf_corpus <- quanteda::corpus(pdf_texts)
      # Tokenize by sentnece
      pdf_tokens <- quanteda::tokens(pdf_corpus, what = "sentence")
      for (i in 1:nrow(regex_dictionary)){
        try(pdf_match <- quanteda::kwic(pdf_tokens, dictionary$regex[i], window = 1, valuetype = "regex"))
        if(!is.null(pdf_match)){
          pdf_match <- as.data.frame(pdf_match)
          pdf_match$pattern <- as.character(pdf_match$pattern)
          pdf_match <- pdf_match %>%
            mutate(name = dictionary$name[i],
                   type = dictionary$type[i],
                   match = stringr::str_extract(pdf_match$keyword, pdf_match$pattern))
          pdf_results <- rbind(pdf_results, pdf_match)
        }
      }
    }
    
    # Create overall results
    fulltext_results <- NULL
    if(!is.null(xml_results)){
      fulltext_results <- rbind(fulltext_results, xml_results)
    }
    if(!is.null(pdf_results)){
      fulltext_results <- rbind(fulltext_results, pdf_results)
    }
    
    # Write results to database
    if(is.null(fulltext_results)){
      DBI::dbWriteTable(con, "pico_tag", append = TRUE)
    }
    
  }
  
}