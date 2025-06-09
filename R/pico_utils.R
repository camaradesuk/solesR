#' Go through regex input file, identify duplicate names and regexes and optionally remove them
#'
#' @param regex_file The input file containing the regex dictionary. Required column names:
#' name, type, regex, main_category, sub_category1 and sub_category2
#'
#' @keywords internal
#'


clean_regex_file <- function(regex_file) {

  dat <- regex_file

  req_cols = c("name","regex","type","main_category","sub_category1","sub_category2")

  if(!all((req_cols) %in% colnames(dat))) {
    stop("Input regex file does not have all required columns: name, type, regex, main_category, sub_category1 and sub_category2")
  }

  # remove duplicate rows
  dat <- dat %>%
    select(name, type, regex, main_category, sub_category1, sub_category2) %>%
    distinct()


  # find duplicate names (different regex) and optionally combine in one regex

  dup_names <- dat %>%
    group_by(name, type, main_category, sub_category1, sub_category2) %>%
    tally() %>%
    filter(n > 1) %>%
    pull(name)

  correct_dup_names <- dat %>%
    filter(name %in% dup_names) %>%
    group_by(name, type, main_category, sub_category1, sub_category2) %>%
    summarize(regex = paste(regex, collapse = "|")) %>%
    ungroup()

  n_dup_names <- length(unique(dup_names))

  dup_names_tbl <- dat %>%
    group_by(name, type, main_category, sub_category1, sub_category2) %>%
    reframe(n = length(unique(regex)),
            regex = regex) %>%
    filter(n > 1) %>%
    select(-n)

  if(n_dup_names > 0) {

    print(dup_names_tbl)
    proceed_combineRegexes <- menu(c("Yes", "No"),
                                   title = message("Different RegEx that correspond to the same name/concept were found.
                                                   Should they be combined in one expression separated by an OR operator?"))

    if (proceed_combineRegexes == "1") {
      dat <- dat %>%
        filter(!name %in% dup_names) %>%
        bind_rows(correct_dup_names)

    } else if(proceed_combineRegexes == "2") {

    }
  } else {

  }


  # find duplicate regex (different name) and optionally remove

  dup_regex_tbl <- dat %>%
    group_by(type, regex, main_category, sub_category1, sub_category2) %>%
    reframe(n = length(unique(name)), name = name) %>%
    filter(n > 1) %>%
    select(-n) %>%
    relocate(name) %>%
    ungroup()

  n_dup_regex <- length(unique(dup_regex_tbl$name))

  if(n_dup_regex > 0) {

    print(dup_regex_tbl)

    proceed_removeDupRegex <- menu(c("Yes", "No"),
                                   title = message("The above entries have duplicate RegEx that correspond to different names/concepts. Should duplicates be removed?
                                               Choosing 'Yes' will keep only the first occurrence of each duplicate RegEx"))

    if (proceed_removeDupRegex == "1") {

      dat <- dat %>%
        group_by(type, regex, main_category, sub_category1, sub_category2) %>%
        slice(1) %>%
        ungroup()
    }
  }

  return(dat)
}


#' This function takes in user-provided RegEx entries and cross-checks them against the main SOLES pico dictionary on OSF.
#' If the provided RegEx are syntactically valid and represent new concepts, they are optionally added to the OSF repository, as well as the SOLES project-specific pico tables. Note that all RegEx should be perl-compatible to be considered valid.
#'
#' @import dplyr
#' @import osfr
#' @importFrom qdapRegex is.regex
#' @importFrom data.table fwrite
#' @importFrom tibble as_tibble_col
#'
#' @param con Connection to soles project db
#' @param regexfile The path to the csv file containing the RegEx. The file should contain the following columns: `name`, `regex`, `type`, `main_category`, `sub_category1`, and `sub_category2`. If the ontology columns are empty, NAs will be introduced.
#' @param master_node OSF node for project to retrieve.
#' @param add_node OSF node for project to retrieve.
#'
#' @details To be able to add new entries to the main dictionary, users will be asked to provide metadata such as the domain of new regex entries and their initials. This information will be included in the log file on OSF.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' add_regex_soles(con, "path/to/new/regex.csv")
#' }
#'
#' @export


add_regex_soles <- function(con, regexfile, master_node, add_node) {

  # first check if soles db is up to date with OSF
  message("Checking status of db pico tables...")
  update <- check_pico(con)

  # create clean temp dir

  if(!dir.exists("temp")) {
    dir.create("temp")
  } else if(dir.exists("temp") &
            length(list.files("temp")) > 0) {
    unlink("temp", recursive = TRUE)
    dir.create("temp")
  }

  # Bring in OSF data -------------------------------------------------------


  message("Retrieving pico tables from OSF...")
  # get pico tables from OSF
  osfr::osf_retrieve_node(master_node) %>%
    osfr::osf_ls_files() %>%
    osfr::osf_download("temp")

  pico_o <- read.csv("temp/pico_ontology.csv")
  pico_d <- read.csv("temp/pico_dictionary.csv")

  # remove osf files from temp folder
  unlink(c("temp/pico_dictionary.csv","temp/pico_ontology.csv"))

  # Check input file --------------------------------------------------------

  regex <- read.csv(regexfile, header = T)

  # check required col names and duplicate name/regex
  regex_to_add <- clean_regex_file(regex)

  # if col id exists, delete it
  if ("id" %in% colnames(regex_to_add)) {
    regex_to_add <- regex_to_add %>%
      select(-id)
  }

  # first format misspelled types
  regex_to_add <- regex_to_add %>%
    mutate(
      type = case_when(
        grepl("[Mm]odels?", type) ~ "model",
        grepl("[Ii]nterventions?", type) ~ "intervention",
        grepl("[Oo]utcomes?", type) ~ "outcome",
        grepl("[Ii]ntervention[_]routes?", type) ~ "intervention_route",
        grepl("[Ss]ex", type) ~ "sex",
        grepl("[Cc]ountry", type) ~ "country",
        grepl("[Cc]ountries", type) ~ "country",
        grepl("[Ss]pecies", type) ~ "species",
        .default = type
      )
    )

  # check if new type should be added

  ontology_types <- paste(unique(pico_o$type) %>% {paste0("\\b", .,"\\b")}, collapse = "|")

  input_types <- unique(regex_to_add$type)

  for (i in input_types) {
    if(!grepl(ontology_types, i)) {
      message(paste0("New RegEx type '", i, "' recognized and will be added to the dictionary."))
      newtype_proceed <- readline("Do you want to proceed? y/n ")

      if(grepl(newtype_proceed, "n")) {
        stop("")
      }
    }
  }


# Validate regex syntax ---------------------------------------------------

  message("Validating new RegEx...")
  expr2test <- regex_to_add %>% pull(regex)

  # non-PERL-compatible regexes will be flagged as invalid
  is_regex_valid <- data.frame(
    sapply(expr2test, qdapRegex::is.regex)
  )

  is_regex_valid_df <- cbind(regex = expr2test, is_regex_valid) %>%
    rename(is_valid = 2) %>%
    tibble::remove_rownames() %>%
    right_join(regex_to_add, by="regex", relationship = "many-to-many") %>%
    group_by(name, type, main_category, sub_category1, sub_category2, regex) %>%
    slice(1) %>%
    ungroup()

  # retain only valid regexes

  if(!all(is_regex_valid_df$is_valid)) {
    invalid <- is_regex_valid_df %>% filter(is_valid %in% FALSE)
    n_invalid <- invalid %>% tally() %>% pull(n)
    write.csv(is_regex_valid_df, file = paste0(tools::file_path_sans_ext(basename(regexfile)), "_invalid.csv"), row.names = F)
    stop(cat(n_invalid,
             "RegEx found with invalid syntax. Note that soles is currently only compatible with perl-compatible RegEx.\nYou can inspect the invalid entries in the following file: ",
             paste0(tools::file_path_sans_ext(basename(regexfile)), "_invalid.csv"), "\n"))

  }


  # Validate regex against text ---------------------------------------------

  valid_regex <- is_regex_valid_df %>%
    filter(is_valid %in% TRUE) %>%
    rowwise() %>%
    mutate(name_detected = grepl(pattern = regex, x = name, ignore.case = T, perl = T))

  if(!all(valid_regex$name_detected)) {
    incorrect <- valid_regex %>% filter(name_detected %in% FALSE)
    n_incorrect <- length(unique(incorrect$name))
    message(cat(n_incorrect,
                "potentially incorrect name-RegEx pairs were found (name not detected by RegEx).\n"))

    remove_incorrect <- menu(c("Return dataframe with results so the RegEx can be inspected before proceeding (recommended)",
                               "Remove all entries where the provided name is not detected by the RegEx and proceed",
                               "Proceed without removing any RegEx"
                               ),
                             title = "What would you like to do?")

    if (remove_incorrect == "1") {
      write.csv(valid_regex, file = paste0(tools::file_path_sans_ext(basename(regexfile)), "_name-not-detected.csv"), row.names = F)
      stop(cat("You can inspect the incorrect entries in the following file: ",
               paste0(tools::file_path_sans_ext(basename(regexfile)), "_name-not-detected.csv"), "\n"))
    } else if (remove_incorrect == "2") {
      correct_regex <- valid_regex %>% filter(name_detected %in% TRUE)
    } else if (remove_incorrect == "3") {
      correct_regex <- valid_regex
    }
  } else {
    correct_regex <- valid_regex
  }


  # Check for duplicates ----------------------------------------------------

  # Additional workaround for invalid syntax

  try_grepl <- function(pattern, name) {
    tryCatch(grepl(pattern, name, ignore.case = T, perl = T), error = function(err) NA)
  }

  message("Checking for duplicates in new RegEx...")

  ontology_names <- unique(pico_o$name)

  matched <- pico_o %>%
    rename(area = id) %>%
    left_join(pico_d, by = c("regex_id" = "id"), relationship = "many-to-many") %>%
    mutate(match = grepl(pattern = paste(correct_regex$regex, collapse = '|'), x = name)) %>%
    select(name, match, type, regex_id, regex, area) %>%
    rename(type_ont = type,
           name_ont = name,
           id_ont = regex_id,
           regex_ont = regex,
           area_ont = area) %>%
    filter(match %in% TRUE)

  regex_match <- matched %>%
    fuzzyjoin::regex_right_join(correct_regex, by = c(name_ont = "regex")) %>%
    rowwise() %>%
    mutate(match_type = case_when(type_ont != type  ~ FALSE,
                                  type_ont == type  ~ TRUE)) %>%
    mutate(match_concept = suppressWarnings(try_grepl(regex_ont, name))) %>%
    ungroup() %>%
    mutate(
      is_likely_new = case_when(
        is.na(match) ~ TRUE,
        match %in% TRUE & match_type %in% FALSE ~ TRUE,
        match %in% TRUE & match_type %in% TRUE ~ FALSE),
      is_new = case_when(is.na(match) ~ TRUE,
                         is_likely_new %in% TRUE & match_concept %in% TRUE & match_type %in% TRUE ~ FALSE,
                         is_likely_new %in% TRUE & match_concept %in% TRUE & match_type %in% FALSE ~ TRUE,
                         is_likely_new %in% TRUE & match_concept %in% FALSE ~ TRUE,
                         is_likely_new %in% FALSE ~ FALSE)
      ) %>%
    filter(!is.na(name))

  # retain only new regexes
  message("Removing RegEx that already exist in the soles database...")

  existing_names <- regex_match %>%
    filter(is_new %in% TRUE) %>%
    filter(name %in% ontology_names) %>%
    select(name, type) %>%
    distinct() %>%
    rename(type_new = type) %>%
    left_join(pico_o, by = "name") %>%
    mutate(match_type = ifelse(type_new == type, TRUE, FALSE)) %>%
    filter(match_type %in% TRUE) %>%
    pull(name)

  new_regex <- regex_match %>%
    filter(!name %in% existing_names) %>%
    filter(is_new %in% TRUE) %>%
    select(name, regex, type, main_category, sub_category1, sub_category2) %>%
    distinct() %>%
    arrange(name)

  n_new <- length((new_regex$name))

  # if no new regexes are provided show message
  n_provided <- length(unique(regex$name))

  if(n_new < 1 &
     isTRUE(update)) {
    message("No new RegEx identified.")
  } else if(n_new < 1 &
            isFALSE(update)) {
    message("No new RegEx identified. To make use of the most up-to-date pico dictionary, we recommend getting the latest version from OSF. See ?soles::check_pico()")
  } else if(n_new > 0) {
    # if new regexes are provided go through more checks and update OSF

    # Retrieve ontology information -------------------------------------------

    ontology_cols <- c("type","main_category","sub_category1","sub_category2")


    for (i in ontology_cols) {

      # write column if information exists in user csv
      if(!is.null(new_regex[[i]])) {
        new_regex[[i]] <- ifelse(!is.na(new_regex[[i]]),
                                       new_regex[[i]],
                                       NA)
      } else {
        # label as NA if no information found
        new_regex[[i]] <- NA
      }

    }

    # Add new regex to master files -------------------------------------------

    # determine regex id for new entries
    last_existing_id <- pico_d %>%
      filter(id < 9999000) %>%
      arrange(id) %>%
      slice_tail() %>%
      pull(id)

    pico_new <- new_regex %>%
      ungroup() %>%
      mutate(id = seq(from = last_existing_id + 1, length.out = nrow(.))) %>%
      mutate(area = NA)

    pico_d_new <- pico_new %>%
      select(id, regex)

    pico_o_new <- pico_new %>%
      rename(regex_id = id,
             id = area) %>%
      select(name, regex_id, type, main_category, sub_category1, sub_category2, id)

    # add regex for new Unknown type

    # new_regex
    new_types <- str_remove_all(input_types, ontology_types) %>%
      {stringi::stri_remove_empty(.)}

    # determine regex id for new unknown type
    last_unknown_id <- pico_d %>%
      filter(id > 9999000) %>%
      arrange(id) %>%
      slice_tail() %>%
      pull(id)

    unknown_o_new <- new_types %>%
      tibble::as_tibble_col(column_name = "type") %>%
      mutate(regex_id = seq(from = last_unknown_id + 1, length.out = nrow(.)),
             name = str_to_title(paste("Unknown", str_replace(type, "_", " "))),
             main_category = "Unknown",
             sub_category1 = "Unknown",
             sub_category2 = "Unknown",
             id = NA) %>%
      select(name, regex_id, type, main_category, sub_category1, sub_category2, id)

    unknown_d_new <- unknown_o_new %>%
      mutate(regex = "") %>%
      select(regex_id, regex) %>%
      rename(id = regex_id)

    # Upload new files to OSF -------------------------------------------------

    # show status update
    n_new <- length(unique(pico_d_new$id))
    tbl_new <- pico_o_new %>%
      group_by(type) %>%
      summarize(n_regex = length(unique(regex_id)))

    message(n_new, ' new regex entries were found and will be added to the SOLES OSF dictionary.')
    print(tbl_new)

    # ask for confirmation to proceed
    osf_confirm <- readline("Do you want to proceed? y/n ")

    if (grepl("y", osf_confirm)) {

      entry_id <- readline("Please enter a domain id for the RegEx you provided ") %>%
        na_if("")

      user_id <- readline("Please provide your initials as user identification ") %>%
        na_if("")

      if (is.na(entry_id)|is.na(user_id)) {
        stop("Please provide the required metadata to continue.")
      } else if (!is.na(entry_id) & !is.na(user_id)) {

        # merge back with master files
        pico_dictionary <- pico_d %>%
          bind_rows(pico_d_new) %>%
          bind_rows(unknown_d_new) %>%
          arrange(id)

        pico_ontology <- pico_o %>%
          bind_rows(pico_o_new) %>%
          bind_rows(unknown_o_new) %>%
          arrange(regex_id)

        # write updated master files locally

        write.csv(pico_ontology, file = "temp/pico_ontology.csv", row.names = FALSE)
        write.csv(pico_dictionary, file = "temp/pico_dictionary.csv", row.names = FALSE)

        # write new additions in separate csv per type

        ont_split <- split(pico_o_new, pico_o_new$type)
        lapply(ont_split, function(x) write.csv(x,
                                                paste0("temp/", paste("regex_ontology", x$type[1], entry_id, user_id, format(Sys.Date(), "%d%m%y"), sep = "_"), ".csv"),
                                                row.names = FALSE))

        dict_2split <- pico_o_new %>%
          select(-id) %>%
          left_join(pico_d_new, by = c("regex_id" = "id")) %>%
          rename(id = regex_id) %>%
          select(id, regex, type)

        dict_split <- split(dict_2split, dict_2split$type)

        lapply(dict_split, function(x) write.csv(x %>% select(-type),
                                                 paste0('temp/', paste("regex_dictionary", x$type[1], entry_id, user_id, format(Sys.Date(), "%d%m%y"), sep = "_"), '.csv'),
                                                 row.names = FALSE))


        # create log entry
        logfile <- pico_o_new %>%
          group_by(type) %>%
          summarize(n_regex = length(unique(regex_id))) %>%
          mutate(domain = entry_id,
                 user = user_id,
                 timestamp = Sys.time(),
                 change_type = "addition") %>%
          relocate(timestamp, domain, user)

        data.table::fwrite(logfile, file = "temp/log.csv", append = T)

        # update the OSF master tables and log file
        osfr::osf_retrieve_node(master_node) %>%
          osfr::osf_upload(path = c("temp/pico_dictionary.csv",
                                    "temp/pico_ontology.csv",
                                    "temp/log.csv"),
                           conflicts = "overwrite",
                           progress = TRUE,
                           recurse = TRUE)

        # upload user regexes to separate folder in OSF
        osfr::osf_retrieve_node(add_node) %>%
          osfr::osf_upload(path = list.files("temp", pattern = "regex", full.names = T),
                           progress = TRUE,
                           recurse = TRUE)

        # append new regex entries to soles db pico tables

        proceed_appendAWS <- menu(c("Yes", "No"),
                                  title = "The pico tables in your SOLES project database will also be updated. Do you want to continue?")
        if (proceed_appendAWS == "1") {
          message("Updating pico tables in soles project AWS database...")
          DBI::dbAppendTable(con, "pico_dictionary", pico_d_new)
          DBI::dbAppendTable(con, "pico_ontology", pico_o_new)
        }

        # delete temp dir
        unlink("temp", recursive = TRUE)
        message("Done!")
      }
    }
  }
}




#' This function checks the status of pico tables in a SOLES project and provides users the option of
#' updating to the newest version available on OSF.
#'
#' @import dplyr
#' @import osfr
#' @param con Connection to soles project db
#'
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' check_pico(con)
#' }
#'
#' @export


check_pico <- function(con, master_node) {

  # clean temp dir
  if(!dir.exists("temp")) {
    dir.create("temp")
  } else if(dir.exists("temp") &
            length(list.files("temp")) > 0) {
    unlink("temp", recursive = TRUE)
    dir.create("temp")
  }

  # get latest pico tables from OSF

  osfr::osf_retrieve_node(master_node) %>%
    osfr::osf_ls_files() %>%
    osfr::osf_download("temp")

  pico_o_osf <- read.csv("temp/pico_ontology.csv")
  pico_d_osf <- read.csv("temp/pico_dictionary.csv")


  # check if AWS pico tables exist and are not empty
  if(DBI::dbExistsTable(con, "pico_dictionary")) {
    pico_d_db <- DBI::dbReadTable(con, "pico_dictionary")
    if(nrow(pico_d_db) < 1) {
      message("The pico_dictionary table is empty. The corresponding OSF table will be written to the database by default.")
      DBI::dbWriteTable(con, "pico_dictionary", pico_d_osf, overwrite = TRUE)
      pico_d_db <- DBI::dbReadTable(con, "pico_dictionary")
    }
  } else {
    message("No pico_dictionary table was found. The OSF pico_dictionary table will be written to the database by default.")
    try(DBI::dbWriteTable(con, "pico_dictionary", pico_d_osf))
    pico_d_db <- DBI::dbReadTable(con, "pico_dictionary")
  }

  if(DBI::dbExistsTable(con, "pico_ontology")) {
    pico_o_db <- DBI::dbReadTable(con, "pico_ontology")
    if(nrow(pico_o_db) < 1) {
      message("The pico_ontology table is empty. The corresponding OSF table will be written to the database by default.")
      DBI::dbWriteTable(con, "pico_ontology", pico_o_osf, overwrite = TRUE)
      pico_o_db <- DBI::dbReadTable(con, "pico_ontology")
    }
  } else {
    message("No pico_ontology table was found. The OSF pico_ontology table will be written to the database by default.")
    try(DBI::dbWriteTable(con, "pico_ontology", pico_o_osf))
    pico_o_db <- DBI::dbReadTable(con, "pico_ontology")

  }


  # get current pico tables from db

  # check table match
  table_diff <- setdiff(pico_d_osf$id, pico_d_db$id)

  if(length(table_diff) > 0) {
    message("Comparing with soles db...")
    proceed_update <- menu(c("Yes", "No"),
                           title = "A more recent version of pico tables is available on OSF. Do you want to update the tables in your SOLES project db?")
    if (proceed_update == "1") {
      # find missing regexes

      pico_d_missing <- pico_d_osf %>%
        filter(id %in% table_diff)

      pico_o_missing <- pico_o_osf %>%
        filter(regex_id %in% table_diff)

      # append missing entries to soles db tables
      message("Updating pico tables in soles AWS database...")
      DBI::dbAppendTable(con, "pico_dictionary", pico_d_missing)
      DBI::dbAppendTable(con, "pico_ontology", pico_o_missing)
      message("Pico tables are up to date")
    } else if(proceed_update == "2") {
      return(FALSE)
    }
    return(TRUE)
  } else if(length(table_diff) == 0) {
    message("Pico tables are up to date")
    return(TRUE)
  }

  unlink("temp", recursive = TRUE)
}

#' Create Regular Expressions
#'
#' This function reads a file containing names and their corresponding alternate names,
#' then creates regular expressions based on these names and alternate names.
#'
#' @param file Path to the input Excel file.
#'
#' @return A new Excel file with additional columns containing regular expressions.
#'
#' @import readxl
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' create_regex("input_file.xlsx")
#' }
#'
create_regex <- function(file = ""){
  
  # Read in file
  file_for_regex <- read.xlsx(file)
  
  # Splits the df in to 2, no_alternate_names and with_alternate_names
  no_alternate_names <- file_for_regex %>%
    filter(is.na(alternate_names) | alternate_names == "")
  
  # Function to create a regex from the "name" column
  name_to_regex <- function(string) {
    
    # Split the sentence into words
    words <- strsplit(string, "\\s")[[1]]
    
    # Initialize a vector to store transformed patterns
    transformed_patterns <- character(length = length(words))
    
    # Escape any punctuation characters in the words
    words <- gsub("([[:punct:]])", "\\\\\\1", words)
    
    # Iterate over each word
    for (i in seq_along(words)) {
      
      # Check if the word starts with [a-z] (case-insensitive)
      if (grepl("^[[:alpha:]]", words[i], ignore.case = TRUE)) {
        
        # Apply pattern transformation with word boundaries at both start and end
        pattern <- paste0("[", toupper(substr(words[i], 1, 1)), tolower(substr(words[i], 1, 1)), "]", substr(words[i], 2, nchar(words[i])))
        
      } else {
        
        # If the word doesn't start with [a-z], keep it as it is
        pattern <- words[i]
        
      }
      
      # Store the transformed pattern
      transformed_patterns[i] <- pattern
      
    }
    
    # Combine transformed patterns into a single string
    collapsed_pattern <- paste(transformed_patterns, collapse = "[\\s-]*")
    
    # Add word boundaries if the collapsed pattern starts with alphabetic character
    if (grepl("^\\[[[:alpha:]]", collapsed_pattern, ignore.case = TRUE)) {
      result <- paste0("\\b", collapsed_pattern, "\\b")
    } else {
      result <- collapsed_pattern
    }
    
    return(result)
    
  }
  
  # Apply the name_to_regex function to each name in 'no_alternate_names'
  no_alternate_names$regex <- sapply(no_alternate_names$name, name_to_regex)
  
  # Create a df that does have alternate names
  with_alternate_names <- file_for_regex %>%
    filter(!(is.na(alternate_names) | alternate_names == ""))
  
  # Apply the name_to_regex function to each name in 'with_alternate_names'
  with_alternate_names$name_regex <- sapply(with_alternate_names$name, name_to_regex)
  
  # Second function to convert alternate_names to regex and collapse on "|"
  convert_alternate_names <- function(names, separate_names_by = "\\|") {
    
    split_strings <- strsplit(names, separate_names_by)[[1]]
    remove_ws <- trimws(split_strings)
    words <- sapply(remove_ws, name_to_regex)
    join_words <- paste(words, collapse = "|")
    
    return(join_words)
  }
  
  # Apply convert_alternate_names function to the alternate names
  with_alternate_names$regex_alternate_names <- sapply(with_alternate_names$alternate_names, convert_alternate_names)
  
  # Concatenates the "name_regex" and "regex_alternate_names" in to 1, separated by "|"
  with_alternate_names$regex <- apply(with_alternate_names[, c("name_regex", "regex_alternate_names")], 1, function(x) paste(x, collapse = "|"))
  
  # Binds the 2 sections back together, no_alternate_names and with_alternate_names
  file_with_regex <- with_alternate_names %>%
    select(name, type, main_category, sub_category1, sub_category2, alternate_names, regex) %>%
    rbind(no_alternate_names)
  
  # Create a new file name
  new_file_name <- paste0("updated_regex_", file)
  write.xlsx(file_with_regex, new_file_name)
  
  message(paste0("File updated with regex and written to working directory"))
  message(paste0("File named: ", new_file_name))
  
}
