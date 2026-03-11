#' Extract open data and code statements using ODDPub
#'
#' This function evaluates reporting of open data and code statements and extracts those statements.
#'
#' @param con connection to db
#' @param path folder containing full texts
#' @param output_mode specifies the output format. Can be `"summary"` (default; old format, with text processed using `pico_tag`) for a reduced overview of extracted open data/code statements, or `"full"` (new format, with text processed using `oddpub` functions) for detailed extraction results.
#' @import oddpub
#' @import dplyr
#' @import readr
#' @export
#'

ods_tag <- function(con, path, output_mode = "summary") {
  # Get records already tagged
  tagged <- DBI::dbReadTable(con, "open_data_tag")

  ft_to_tag <- dbReadTable(con, "full_texts") %>%
    filter(status == "found") %>%
    filter(!doi %in% tagged$doi)

  if (output_mode == 'summary') {
    # Get text files retrieved
    text_files <- list.files(path = path, pattern = ".txt", full.names = TRUE)
    text_files <- gsub(paste0(path, "/"), "", text_files)
    text_files <- gsub("\\.txt", "", text_files)
    text_files <- gsub("%2F", "\\/", text_files)
    text_files <- gsub("%3C", "<", text_files)
    text_files <- gsub("%3E", ">", text_files)
    text_files <- gsub("%3A", ":", text_files)
    text_files <- gsub("%22", '"', text_files)
    text_files <- gsub("%7C", "\\|", text_files)
    text_files <- gsub("%3F", "\\?", text_files)
    text_files <- gsub("%2A", "\\*", text_files)

    # Identify full texts that require tag
    ft_to_tag <- ft_to_tag %>%
      filter(doi %in% text_files)

    # Point path towards existing txt file
    ft_to_tag$path <- gsub("\\.(pdf|xml|json)", "\\.txt", ft_to_tag$path)

    # Get sample
    if (length(ft_to_tag$doi) < 1) {
      message("No more papers to tag by open data /code statement")
      return()
    } else if (length(ft_to_tag$doi) > 500) {
      message(
        "Over 500 papers to tag by open data / code statement. Selecting a sample of 500"
      )
      to_find <- ft_to_tag[1:500, ]
    } else {
      to_find <- ft_to_tag
    }

    # Deal with relative paths
    to_find$path <- sub(".*/", "", to_find$path)
    to_find$path_txt = to_find$path
    to_find$path <- paste0(path, "/", to_find$path)

    # Run tokenisation
    PDF_text_sentences <- purrr::map(to_find$path, function(path) {
      tryCatch(
        {
          readr::read_lines(path) %>%
            paste(collapse = " ") %>%
            stringi::stri_enc_toutf8(validate = TRUE) %>%
            tokenizers::tokenize_sentences(simplify = TRUE) %>%
            tolower() %>%
            stringr::str_replace_all(pattern = ",", replacement = "") %>%
            oddpub:::.correct_tokenization() %>%
            stats::na.omit()
        },
        error = function(e) {
          warning(paste("Failed to process:", path))
          return(NULL)
        }
      )
    })
    # Add text's name
    names(PDF_text_sentences) <- to_find$doi
  } else if (output_mode == 'full') {
    if (length(ft_to_tag$doi) < 1) {
      message("No more papers to tag by open data /code statement")
      return()
    } else if (length(ft_to_tag$doi) > 500) {
      message(
        "Over 500 papers to tag by open data / code statement. Selecting a sample of 500"
      )
      to_find <- ft_to_tag[1:500, ]
    } else {
      to_find <- ft_to_tag
    }

    # Deal with relative paths
    to_find <- to_find |>
      rename(subdir_path = path) |>
      mutate(
        path_pdf = sub(".*/", "", subdir_path),
        path_txt = sub(
          "\\.(pdf|xml|json)$",
          ".txt",
          path_pdf,
          ignore.case = TRUE
        )
      )

    # Load texts
    PDF_text_sentences <- oddpub::pdf_load(path)

    # Let only the ones that need to be tagged
    PDF_text_sentences <- PDF_text_sentences[
      names(PDF_text_sentences) %in% to_find$path_txt
    ]

    # Add text's names
    names(PDF_text_sentences) <-
      to_find$doi[match(names(PDF_text_sentences), to_find$path_txt)]
  } else {
    stop("output_mode should be 'summary' or 'full'")
  }

  message(paste0(
    "Sucessfully loaded ",
    length(names(PDF_text_sentences)),
    " text files"
  ))

  # Save the current plan
  oplan <- future::plan()

  # Ensure the original plan is restored when the function exits
  on.exit(future::plan(oplan), add = TRUE)

  # Run open data / code tag in multisessions
  total_cores <- future::availableCores()
  workers_to_use <- max(1, total_cores / 3)
  future::plan(future::multisession, workers = workers_to_use)

  progressr::handlers(global = TRUE)

  open_data_results <- oddpub::open_data_search(
    PDF_text_sentences,
    screen_das = "extra"
  )

  # Transform output
  open_data_results <- open_data_results %>%
    rename(doi = article) %>%
    mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
    mutate(method = "oddpub")

  if (output_mode == 'summary') {
    # Select columns for the reduced table
    open_data_results <- open_data_results |>
      select(
        doi,
        is_open_data,
        open_data_category,
        is_open_code,
        open_data_statements,
        open_code_statements,
        method
      )

    # Write to database
    dbWriteTable(con, "open_data_tag", open_data_results, append = TRUE)
    message(paste0(
      "Sucessfully written ",
      length(names(PDF_text_sentences)),
      " tagged citations to db (summary output)"
    ))
  } else if (output_mode == 'full') {
    # Write to database
    dbWriteTable(con, "open_data_tag", open_data_results, append = TRUE)

    message(paste0(
      "Sucessfully written ",
      length(names(PDF_text_sentences)),
      " tagged citations to db (full output)"
    ))
  }
}

get_doi_match <- function() {
  # read table with pdf links
  pdfs_df <- dbReadTable(con, "pdf_links")

  # keep only found pdfs - need pdf for regex full text search
  doi_match <- pdfs_df %>%
    filter(pdf == "found") %>%
    select(uid, doi)

  return(doi_match)
}
