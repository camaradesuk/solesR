#' Tag citations using regex
#'
#' This function tags included citations with different regex dictionaries, using the full text and title/abstract/keyword fields
#' separately. Finally, tags are added to the relevant database tables
#'
#' @param con connection to db - note that this must contain tables of the format 'type_tag' and dictionaries of the type type_dictionary
#' @param tag_type tag type related to db table headers e.g. outcome, model, or intervention
#' @param tag_method full_text OR tiabkw
#' @param tag_main_category tag main category from pico ontology table, leave as default all if you want all for that type
#' @param ignore_case do you want to ignore case for regexes?
#' @param extract_strings do you want to extract regex matches from text?
#' @import tidyr
#' @import dplyr
#' @export

pico_tag <- function(con, tag_type, tag_method, tag_main_category="all", ignore_case = FALSE, extract_strings=TRUE){
  
  # Load included citations ----------------
  message("loading included studies...")
  
  # read table with pdf links
  got_ft <- tbl(con, "full_texts") %>% filter(status=="found") %>% select(doi, path)
  
  # get already tagged studies
  if("all" %in% tag_main_category){
    
    tagged <- tbl(con, "pico_ontology") %>% filter(type == tag_type) %>% left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
      select(uid, method, regex_id) %>% collect()
    dictionary <- tbl(con, "pico_dictionary") %>% left_join(tbl(con, "pico_ontology"), by=c("id" = "regex_id")) %>% filter(type == tag_type) %>% collect()
    dictionary <- as.data.frame(dictionary)
    
  } else {
    tagged <- tbl(con, "pico_ontology") %>% filter(type == tag_type) %>% filter(main_category %in% tag_main_category) %>% left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
      select(uid, method, regex_id) %>% collect()
    
    dictionary <- tbl(con, "pico_dictionary") %>% left_join(tbl(con, "pico_ontology"), by=c("id" = "regex_id")) %>% filter(type == tag_type) %>% filter(main_category %in% tag_main_category) %>% collect()
    dictionary <- as.data.frame(dictionary)
    dictionary <- dictionary %>%
      select(id, regex) %>%
      distinct()
  }
  
  # get id for unknown
  unknown_id <- tbl(con, "pico_ontology") %>% filter(type == tag_type) %>% filter((main_category == "Unknown") & grepl("^999999", as.character(regex_id))) %>% collect()
  unknown_id <- unknown_id$regex_id
  
  dictionary <- dictionary %>%
    filter(!id %in% unknown_id)
  
  #get included studies
  included <- tbl(con, "study_classification") %>% filter(decision == "include")
  
  # get included relevant studies + metadata
  suppressMessages(
    included_studies <- tbl(con, "unique_citations") %>%
      select(uid, title, abstract, keywords, doi) %>%
      semi_join(included, by="uid") %>%  # keep only those in included
      left_join(got_ft, by="doi") %>% # keep only those in included
      collect()
  )
  
  # TIABKW Tagging ----------------
  # get studies still to tag for tiab
  if(tag_method == "tiabkw"){
    
    done_tiab <- tagged %>%
      filter(method == "tiabkw_regex")
    
    to_tag_tiab <- included_studies %>%
      filter(!uid %in% done_tiab$uid) %>%
      select(uid, title, abstract, keywords)
    
    # if no more papers to tag, message
    if(length(to_tag_tiab$uid) < 1){
      
      message("No more papers to tag by title/abstract/keywords")
      return()
      
    }else if(length(to_tag_tiab$uid) > 5000){
      
      message("Over 5000 papers to tag by title/abstract/keywords. Selecting a sample of 5000")
      
      to_tag_tiab_sample <- to_tag_tiab[sample(nrow(to_tag_tiab), 5000), ]
      to_tag_tiab_sample[is.na(to_tag_tiab_sample)] <- ""
      to_tag_tiab_sample <- as.data.frame(to_tag_tiab_sample)
      
    } else{
      
      to_tag_tiab_sample <- as.data.frame(to_tag_tiab)
      
    }
    
    # message n studies to tag
    message(paste0(length(unique(to_tag_tiab_sample$uid)), " papers ready to tag..."))
    
    to_tag_tiab_sample[is.na(to_tag_tiab_sample)] <- ""
    
    if(extract_strings == TRUE){
      
      # run auto annotation
      tiab_result <- suppressMessages(suppressWarnings(
        AutoAnnotation::CountTermsInStudies(
          searchingData = to_tag_tiab_sample,
          dictionary = dictionary,
          ignoreCase = ignore_case,
          dictionaryNameHeader = "id",
          dictionaryRegexHeader = "regex",
          cutIntro = TRUE,
          cutRefs = TRUE,
          extractStrings = TRUE,
          textSearchingHeaders = c("keywords", "title", "abstract"),
          ignoreExistingTextFile = FALSE)))
      
      # bind back to keep id columns
      tiab_result <- cbind(tiab_result, to_tag_tiab_sample)
      
      # get string result
      string_result <- tiab_result  %>%
        select(contains("Matched Strings"), uid) %>%
        tidyr::pivot_longer(c(-uid), names_to="id", values_to="strings") %>%
        mutate(strings = as.character(strings)) %>%
        mutate(id = as.character(id)) %>%
        unique()
      
      # remove extra in id field
      string_result$id <- gsub(":.*", "", string_result$id)
      string_result$strings <- gsub("character\\(0\\)", "", string_result$strings)
      string_result$id <- as.character(string_result$id)
      
      # get regex count result
      tiab_result <- tiab_result[!is.na(names(tiab_result))]
      regex_count <- tiab_result %>%
        select(-contains("Matched Strings"), -title, -abstract, -keywords, -Status) %>%
        tidyr::pivot_longer(c(-uid), names_to="id", values_to="frequency") %>%
        mutate(frequency = as.character(frequency)) %>%
        mutate(frequency = as.numeric(frequency)) %>%
        group_by(uid) %>%
        mutate(unknown = ifelse(all(frequency==0), TRUE, FALSE)) %>%
        mutate(id = ifelse(unknown == TRUE, paste(unknown_id), paste(id))) %>%
        ungroup() %>%
        filter(!(frequency == 0 & unknown == FALSE)) %>%
        select(-unknown) %>%
        unique()
      
      final_regex_result <- suppressMessages(left_join(regex_count, string_result))
      final_regex_result$method <- "tiabkw_regex"
      
    } else {
      
      tiab_result <- suppressMessages(suppressWarnings(
        AutoAnnotation::CountTermsInStudies(
          searchingData = to_tag_tiab_sample,
          dictionary = dictionary,
          ignoreCase = ignore_case,
          dictionaryNameHeader = "id",
          dictionaryRegexHeader = "regex",
          cutIntro = TRUE,
          cutRefs = TRUE,
          extractStrings = FALSE,
          textSearchingHeaders = c("keywords", "title", "abstract"),
          ignoreExistingTextFile = FALSE)))
      
      # bind back to keep id columns
      tiab_result <- cbind(tiab_result, to_tag_tiab_sample)
      
      # get regex count result
      tiab_result <- tiab_result[!is.na(names(tiab_result))]
      final_regex_result <- tiab_result %>%
        select(-title, -abstract, -keywords, -Status) %>%
        tidyr::pivot_longer(c(-uid), names_to="id", values_to="frequency") %>%
        mutate(frequency = as.character(frequency)) %>%
        mutate(frequency = as.numeric(frequency)) %>%
        group_by(uid) %>%
        mutate(unknown = ifelse(all(frequency==0), TRUE, FALSE)) %>%
        mutate(id = ifelse(unknown == TRUE, paste(unknown_id), paste(id))) %>%
        ungroup() %>%
        filter(!(frequency == 0 & unknown == FALSE)) %>%
        select(-unknown) %>%
        unique()
      
      final_regex_result$method <- "tiabkw_regex"
      final_regex_result$strings <- ""
      
    }
    
    final_regex_result <- final_regex_result %>%
      rename(regex_id = id)
    
    dbWriteTable(con, "pico_tag", final_regex_result, append=TRUE)
    
    message(paste0(length(unique(final_regex_result$uid)),
                   " studies with title / abstracts / keyword fields tagged by ",
                   tag_type))
    
  } else {
    
    # Full Text Tagging -----------
    
    if(tag_method == "fulltext"){
      
      done_ft <- tagged %>%
        filter(method == "fulltext_regex")
      
      to_tag_ft <- included_studies %>%
        filter(!uid %in% done_ft$uid) %>%
        filter(!path == "")
      
      # if no more papers to tag, message
      if(length(to_tag_ft$uid) < 1){
        
        message("No more papers to tag by full text")
        return()
        
      } else if(length(to_tag_ft$uid) > 1000){
        
        message("Over 1000 papers to tag by full text. Selecting a sample of 1000")
        
        to_tag_ft_sample <- to_tag_ft[sample(nrow(to_tag_ft), 1000), ]
        to_tag_ft_sample[is.na(to_tag_ft_sample)] <- ""
        to_tag_ft_sample <- as.data.frame(to_tag_ft_sample)
        
      } else{
        
        to_tag_ft_sample <- as.data.frame(to_tag_ft)
      }
      
      # message n studies to tag
      message(paste0(length(unique(to_tag_ft_sample$uid)), " papers ready to tag..."))
      
      if(extract_strings == TRUE){
        
        ft_result <- suppressMessages(suppressWarnings(
          AutoAnnotation::CountTermsInStudies(
            searchingData = to_tag_ft_sample,
            dictionary = dictionary,
            ignoreCase = ignore_case,
            dictionaryNameHeader = "id",
            dictionaryRegexHeader = "regex",
            cutIntro = TRUE,
            cutRefs = TRUE,
            extractStrings = TRUE,
            linkSearchHeaders = "path",
            ignoreExistingTextFile = FALSE)))
        
        ft_result <- cbind(ft_result, to_tag_ft_sample)
        
        # get string result
        string_result <- ft_result  %>%
          select(contains("Matched Strings"), uid) %>%
          tidyr::pivot_longer(c(-uid), names_to="id", values_to="strings") %>%
          mutate(strings = as.character(strings)) %>%
          mutate(id = as.character(id))
        
        # remove extra in id field
        string_result$id <- gsub(":.*", "", string_result$id)
        string_result$strings <- gsub("character\\(0\\)", "", string_result$strings)
        string_result$id <- as.character(string_result$id)
        
        # get regex count result
        regex_count <- ft_result %>%
          filter(pathStatus == "OK: File is read Successfully") %>%
          select(-contains("Matched Strings"), -title, -abstract, -path, -keywords, -pathStatus, -doi) %>%
          tidyr::pivot_longer(c(-uid), names_to="id", values_to="frequency") %>%
          mutate(frequency = as.character(frequency)) %>%
          mutate(frequency = as.numeric(frequency)) %>%
          group_by(uid) %>%
          mutate(unknown = ifelse(all(frequency==0), TRUE, FALSE)) %>%
          mutate(id = ifelse(unknown == TRUE, paste(unknown_id), paste(id))) %>%
          ungroup() %>%
          filter(!(frequency == 0 & unknown == FALSE)) %>%
          select(-unknown) %>%
          unique()
        
        final_regex_result <- suppressMessages(left_join(regex_count, string_result))
        final_regex_result$method <- "fulltext_regex"
        
      } else {
        
        ft_result <- suppressMessages(suppressWarnings(
          AutoAnnotation::CountTermsInStudies(
            searchingData = to_tag_ft_sample,
            dictionary = dictionary,
            ignoreCase = ignore_case,
            dictionaryNameHeader = "id",
            dictionaryRegexHeader = "regex",
            cutIntro = TRUE,
            cutRefs = TRUE,
            extractStrings = FALSE,
            linkSearchHeaders = "path",
            ignoreExistingTextFile = FALSE)))
        
        # bind back to keep id columns
        ft_result <- cbind(ft_result, to_tag_ft_sample)
        
        # get regex count result
        regex_count <- ft_result %>%
          filter(pathStatus == "OK: File is read Successfully") %>%
          select(-title, -abstract, -path, -keywords, -pathStatus, -doi) %>%
          tidyr::pivot_longer(c(-uid), names_to="id", values_to="frequency") %>%
          mutate(frequency = as.character(frequency)) %>%
          mutate(frequency = as.numeric(frequency)) %>%
          group_by(uid) %>%
          mutate(unknown = ifelse(all(frequency==0), TRUE, FALSE)) %>%
          mutate(id = ifelse(unknown == TRUE, paste(unknown_id), paste(id))) %>%
          ungroup() %>%
          filter(!(frequency == 0 & unknown == FALSE)) %>%
          select(-unknown) %>%
          unique()
        
        final_regex_result <- regex_count
        final_regex_result$method <- "fulltext_regex"
        final_regex_result$strings <- ""
        
      }
      
      final_regex_result <- final_regex_result %>%
        rename(regex_id = id)
      
      dbWriteTable(con, "pico_tag", final_regex_result, append=TRUE)
      message(paste0(length(unique(final_regex_result$uid)),
                     " full texts tagged by ", tag_type))
    }
  }
}

#' Complete PICO Tagging Function
#'
#' This function checks how many studies are still to be tagged, then runs the pico_tag function multiple times until there are no more studies to tag.
#'
#' @param con A database connection object.
#' @param tag_method The tagging method to use, either "fulltext" or "tiabkw".
#' @param tag_type The type of PICO tag to apply (e.g. "intervention", "species" etc).
#' @param tag_main_category The main category of the PICO tag. Default is "all".
#'
#' @return The function does not explicitly return a value, but it updates the connected database with new pico tags.
#'
#' @details
#' This function retrieves studies that are already tagged using the specified tagging method, and then tags the remaining studies that meet
#' the specified criteria. It supports two tagging methods: "fulltext" and "tiabkw". The tagging process
#' involves using regular expressions to extract relevant information from the full text or title/abstract/keywords, depending on
#' the chosen method.
#'
#' @examples
#' \dontrun{
#' Tag studies using full text regex
#' complete_pico_tag(con, tag_method = "fulltext", tag_type = "intervention", tag_main_category = "all")
#'}
#' @import dplyr
#' @import dbplyr
#' @export
complete_pico_tag <- function(con, tag_method = "", tag_type = "", tag_main_category = "all"){
  
  got_ft <- tbl(con, "full_texts") %>% filter(status=="found") %>% select(doi, path)
  
  # Get included studies
  included <- tbl(con, "study_classification") %>% filter(decision == "include")
  
  # Get included relevant studies + metadata
  included_studies <- tbl(con, "unique_citations") %>%
    select(uid, title, abstract, keywords, doi) %>%
    semi_join(included, by="uid") %>%
    left_join(got_ft, by="doi") %>%
    collect()
  
  if (tag_method == "fulltext"){
    
    message(paste0("Retrieving studies already tagged using full text regex..."))
    
    if (tag_main_category == "all"){
      
      tagged <- tbl(con, "pico_ontology") %>%
        filter(type == tag_type) %>%
        left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
        filter(method == "fulltext_regex") %>%
        select(uid, method, regex_id) %>%
        collect()
      
    }else{
      
      # Retrieve studies already tagged by full text
      tagged <- tbl(con, "pico_ontology") %>%
        filter(type == tag_type) %>%
        filter(main_category %in% tag_main_category) %>%
        left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
        filter(method == "fulltext_regex") %>%
        select(uid, method, regex_id) %>%
        collect()
    }
    
    to_tag <- included_studies %>%
      filter(!uid %in% tagged$uid) %>%
      filter(!path == "")
    
    message(paste0("Total number of studies still to tag using full text: ", length(to_tag$uid)))
    pre_to_tag <- to_tag
    new_tags <- 0
    total_new_tags <- 0
    
    # Run loop until all remaining studies are tagged
    while (new_tags < length(to_tag$uid)){
      
      try(pico_tag(con, tag_type = tag_type,
                   tag_main_category = tag_main_category,
                   tag_method = tag_method,
                   extract_strings = FALSE))
      
      if (tag_main_category == "all"){
        
        post_tagged <- tbl(con, "pico_ontology") %>%
          filter(type == tag_type) %>%
          left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
          filter(method == "fulltext_regex") %>%
          select(uid, method, regex_id) %>%
          collect()
        
      }else{
        
        # Retrieve studies already tagged by full text
        post_tagged <- tbl(con, "pico_ontology") %>%
          filter(type == tag_type) %>%
          filter(main_category %in% tag_main_category) %>%
          left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
          filter(method == "fulltext_regex") %>%
          select(uid, method, regex_id) %>%
          collect()
      }
      
      post_to_tag <- included_studies %>%
        filter(!uid %in% post_tagged$uid) %>%
        filter(!path == "")
      
      # Calculate how many studies are still to be tagged
      new_tags <- length(pre_to_tag$uid) - length(post_to_tag$uid)
      total_new_tags <- total_new_tags + new_tags
      message(paste0("Number of studies to tag by ", tag_method, " : ", length(post_to_tag$uid)))
      
      # If there are no new studies still to be tagged exit the loop
      if (new_tags == 0){
        
        break
        
      }
      
      pre_to_tag <- post_to_tag
      
    }
    
    message(paste0(total_new_tags, " out of a possible ", length(to_tag$uid), " tagged for ", tag_type))
    
  } else if (tag_method == "tiabkw"){
    
    message(paste0("Retrieving studies already tagged using tiabkw..."))
    
    if (tag_main_category == "all"){
      
      tagged <- tbl(con, "pico_ontology") %>%
        filter(type == tag_type) %>%
        left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
        filter(method == "tiabkw_regex") %>%
        select(uid, method, regex_id) %>%
        collect()
      
    }else{
      
      # Retrieve studies already tagged by full text
      tagged <- tbl(con, "pico_ontology") %>%
        filter(type == tag_type) %>%
        filter(main_category %in% tag_main_category) %>%
        left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
        filter(method == "tiabkw_regex") %>%
        select(uid, method, regex_id) %>%
        collect()
    }
    
    to_tag <- included_studies %>%
      filter(!uid %in% tagged$uid) %>%
      select(uid, title, abstract, keywords)
    
    message(paste0("Total number of studies still to tag using tiabkw: ", length(to_tag$uid)))
    pre_to_tag <- to_tag
    new_tags <- 0
    total_new_tags <- 0
    
    # Run loop until all remaining studies are tagged
    while (total_new_tags < length(to_tag$uid)){
      
      message("Running pico_tag for tiabkw...")
      try(pico_tag(con, tag_type = tag_type,
                   tag_main_category = tag_main_category,
                   tag_method = tag_method,
                   extract_strings = FALSE))
      
      if (tag_main_category == "all"){
        
        post_tagged <- tbl(con, "pico_ontology") %>%
          filter(type == tag_type) %>%
          left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
          filter(method == "tiabkw_regex") %>%
          select(uid, method, regex_id) %>%
          collect()
        
      }else{
        
        # Retrieve studies already tagged by full text
        post_tagged <- tbl(con, "pico_ontology") %>%
          filter(type == tag_type) %>%
          filter(main_category %in% tag_main_category) %>%
          left_join(tbl(con, "pico_tag"), by=c("regex_id")) %>%
          filter(method == "tiabkw_regex") %>%
          select(uid, method, regex_id) %>%
          collect()
      }
      
      post_to_tag <- included_studies %>%
        filter(!uid %in% post_tagged$uid) %>%
        select(uid, title, abstract, keywords)
      
      # Calculate how many new studies have been tagged
      new_tags <- length(pre_to_tag$uid) - length(post_to_tag$uid)
      total_new_tags <- total_new_tags + new_tags
      message(paste0("Number of studies to still to tag by ", tag_method, " : ", length(post_to_tag$uid)))
      
      # If there are no new studies still to be tagged then exit the loop
      if (new_tags == 0){
        
        break
        
      }
      pre_to_tag <- post_to_tag
    }
    message(paste0(total_new_tags, " out of a possible ", length(to_tag$uid), " tagged for ", tag_type))
  }
}