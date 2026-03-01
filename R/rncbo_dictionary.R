# RNCBO - Mapping terms using NCBO Recommender, Search & Annotator tools

# FIRST VERSION ----------------------------------------------------------------------------------
# http://bioportal.bioontology.org/recommender
# eNanoMapper Developers | enanomapper.net

# Contact: muntisa [at] gmail [dot] com
# RNASA-IMEDIR, University of A Coruna, Spain
# source available at https://github.com/muntisa/RNCBO

# Input file    = CSV one column list of terms to be mapped
# Output result = TAB-separated files with "Term","Score","Acronym","Ontology","ID","Notation","Description","Synonyms" for mapped terms and CSV for non-mapped terms

# Notes:
# - only the REST Recommender and Search have been used to extract information (some information could not exist into results)
# - some fields such as Description and Synonyms could contain multiple items separated by "|"

# UPDATES -----------------------------------------------------------------------------------------------
# updated 09/10/2015 mko (Manfred Kohler):
# • changed "text" to "input" for URL recommender API
# • returnvalues of JSONList changed
# • replace NCBIO and NCBI with NCBO
# • use comma separated file for result instead of tab delimited
# • for performance issues restrictions to predefined Ontologies added
# • cuttoff extension before adding nonmapped.csv to results file name

# update 12/12/2025 — Tamires Martins (github/tamimart)
# • Added function to retrieve ontologies based on a list of terms.
# • Included conditional handling for irregular or malformed JSON responses.
# • Added function to fetch related terms (parents) directly from BioPortal endpoint URLs (annotator).
# • Refactored NCBOmapper and optimized for-loops.

# --------- FUNCTIONS --------------------------------------------------------------------------------------------------------------
#' NCBO_ontologies
#'
#' This function takes a list of terms and uses the NCBO Recommender API to suggest relevant ontologies. It samples the terms, sends them to the API, and collects the top recommended ontologies.
#'
#' @param terms_list A character string with the path to a file containing a list of terms, one term per line.
#' @param apikey Your NCBO API key.
#' @param file_path The path to the directory where the output file with the list of ontologies will be saved.
#' @param n_sample The number of samples to create from the terms list. Default is 5.
#' @param n_terms The number of terms to include in each sample. Default is 100.
#' @param topn_onto The number of top ontologies to select from the recommendations for each sample. Default is 5.
#'
#' @return A character vector with the unique acronyms of the recommended ontologies. It also writes a file named "ontology_list.csv" in the specified `file_path` with the list of recommended ontologies.
#'
#' @examples
#' \dontrun{
#' NCBO_ontologies(
#'  terms_list = TERM_LIST,
#'  apikey = YOUR_API_KEY,
#'  file_path = "data/dictionary/NCBO"
#' )
#' }
NCBO_ontologies <- function(
  terms_list,
  apikey,
  file_path = "",
  n_sample = 5,
  n_terms = 100,
  topn_onto = 5
) {
  set.seed(123) # reproducibility
  terms <- tibble::tibble(term = readLines(terms_list))

  # Create samples
  terms_chunks <- purrr::map(
    1:n_sample,
    ~ {
      terms |>
        dplyr::slice_sample(n = n_terms, replace = TRUE)
    }
  )

  # Process chunks and get recommended ontologies
  res_all <- terms_chunks |>
    purrr::map(\(sample_df) {
      termos <- paste(sample_df$term, collapse = ",")

      sURL <- sprintf(
        "https://data.bioontology.org/recommender?input=%s&apikey=%s",
        URLencode(termos),
        apikey
      )

      CampaignJSON <- RCurl::getURL(sURL)
      if (!jsonlite::validate(CampaignJSON)) {
        message(crayon::red(glue::glue("Invalid JSON for batch: {termos}")))
        return(NULL)
      }

      res <- NCBO_recommender_from_JSON(termos, CampaignJSON)
      if (is.null(res) || nrow(res) == 0) {
        message(crayon::yellow("No recommendations found for this batch."))
        return(NULL)
      }

      top_res <- res |>
        dplyr::slice_max(Score, n = topn_onto) |>
        dplyr::pull(Ontology)

      message(crayon::blue(glue::glue(
        "Batch recommendations: {paste(top_res, collapse=', ')}"
      )))
      top_res
    }) |>
    unlist() |>
    unique()

  print(res_all)
  # save ontology recommended list to a file ready to be called on NCBO_mapper
  writeLines(
    text = paste(res_all, collapse = "\n"),
    con = here::here(file_path, "ontology_list.csv")
  )
}

# ---------------------------------------------------------------------------------------------------------------------------------------
#' NCBO_recommender_from_JSON
#'
#' This function processes a JSON object from the NCBO Recommender API and extracts the recommended ontologies, their scores, and other details for a given term.
#'
#' @param term The term that was sent to the recommender.
#' @param CampaignJSON The JSON object returned by the NCBO Recommender API.
#'
#' @return A data frame with the recommended ontologies for the term. The data frame has the following columns: "Term", "Found_term", "Score", "Ontology", "Ontology_URI". Returns NULL if there are errors or no results.
NCBO_recommender_from_JSON <- function(term, CampaignJSON) {
  # Get NCBO Recommender ontologies for 1 term (including scores) as data frame using NSBO Recommender JSON object

  JSONList <- rjson::fromJSON(
    CampaignJSON,
    method = "C",
    unexpected.escape = "error"
  )

  if (is.null(JSONList) || length(JSONList) == 0) {
    return(NULL)
  }

  # Use map_dfr to build the dataframe functionally
  RecommenderDF <- purrr::map_dfr(
    JSONList,
    ~ {
      # Basic validation for nested elements
      if (is.null(.x$coverageResult) || is.null(.x$ontologies[[1]])) {
        return(NULL)
      }

      dplyr::tibble(
        Term = term,
        Found_term = .x$coverageResult$annotations[[1]]$text,
        Score = .x$evaluationScore,
        Ontology = .x$ontologies[[1]]$acronym,
        Ontology_URI = .x$ontologies[[1]]$links$ui
      )
    }
  )

  return(RecommenderDF)
}

#-------------------------------------------------------------------------------
#' NCBO_get_fields_from_JSON
#'
#' @description This function parses the JSON output from an NCBO REST Search query and extracts specific fields: ID, notation, definition, and synonyms.
#'
#' @param CampaignJSON A JSON object returned from an NCBO REST Search query.
#'
#' @return A character vector containing the ID, notation, definition, and synonyms for a term. If a field is not found, it returns NA. If there's an error, the ID will be "ERROR!".
NCBO_get_fields_from_JSON <- function(CampaignJSON) {
  # Get NCBO REST ontology information for 1 term from 1 ontology using JSON from NCBO REST Search

  JSONList <- rjson::fromJSON(CampaignJSON) # convert JSON object into a list
  results <- JSONList$collection # get collection class that includes descriptions for one term

  sId <- c(results[1][[1]]$`@id`) # get ID
  if (is.null(sId)) {
    # if there is no synonym class
    sId <- c("ERROR!") # pointing errors in Recommender with no Search values
  } else {
    sId <- c(paste(sId, collapse = '|'))
  } # colapse the possible list of IDs into one string separated by "|"

  sNotation <- c(results[1][[1]]$notation) # get Synonyms
  if (is.null(sNotation)) {
    # if there is no synonym class
    sNotation <- NA_character_
  } else {
    sNotation <- c(paste(sNotation, collapse = '|'))
  } # colapse the possible list of synonyms into one string separated by "|"

  sDef <- c(results[1][[1]]$definition) # get Definitions
  if (is.null(sDef)) {
    # if there is no definition class
    sDef <- NA_character_
  } else {
    sDef <- c(stringr::str_replace(
      stringr::str_replace(paste(sDef, collapse = '|'), "\n", ""),
      "\n",
      ""
    ))
  } # colapse the possible list of descriptions into one string separated by "|"; "\n" and "\n\n" are eliminated from definitions!

  sSynonyms <- c(results[1][[1]]$synonym) # get Synonyms
  if (is.null(sSynonyms)) {
    # if there is no synonym class
    sSynonyms <- NA_character_
  } else {
    sSynonyms <- c(paste(sSynonyms, collapse = '|'))
  } # colapse the possible list of synonyms into one string separated by "|"

  return(c(sId, sNotation, sDef, sSynonyms))
  # Return a vector of field information for [ID, sNotation, definition, synonyms]
}

#--------------------------------------------------------------------------------
#' NCBO_get_parents_from_JSON
#'
#' This function takes a JSON object from the NCBO Annotator and an API key, extracts the URL for the parent terms, fetches them, and returns the preferred labels of the parent terms.
#'
#' @param CampaignJSON The JSON object from the NCBO Annotator.
#' @param apikey Your NCBO API key.
#'
#' @return A character vector of parent terms' preferred labels, concatenated with "|". If no parents are found, it returns NA.
NCBO_get_parents_from_JSON <- function(CampaignJSON, apikey) {
  JSONList <- rjson::fromJSON(CampaignJSON) # convert JSON object into a list

  if (
    length(JSONList) == 0 || is.null(JSONList[[1]]$annotatedClass$links$parents)
  ) {
    return(NA_character_)
  }

  results <- JSONList[[1]]$annotatedClass$links

  parents_url <- paste0(results$parents[1], "?apikey=", apikey, "&pagesize=200")
  parents_json <- RCurl::getURL(parents_url)
  res <- rjson::fromJSON(parents_json)

  if (is.null(res) || length(res) == 0 || !is.list(res)) {
    parents <- NA_character_
  } else if (
    length(res) == 0 ||
      !is.list(res[[1]]) ||
      !"prefLabel" %in% names(res[[1]]) ||
      length(res[[1]][["prefLabel"]]) == 0
  ) {
    parents <- NA_character_
  } else {
    parents <- c(paste(res[[1]]$prefLabel, collapse = '|'))
  }
  return(c(parents))
}

# ----------------------------------------------------------------------------------------------------
#' NCBO_process_term
#'
#' This helper function is used internally by the refactored `NCBO_mapper`
#' pipeline to query BioPortal services for a single term. It performs
#' three sequential API requests:
#' \enumerate{
#'   \item Recommender API (suggest best-matching ontologies)
#'   \item Search API (retrieve term metadata, definitions and synonyms)
#'   \item Annotator API (retrieve parent concepts/relations)
#' }
#'
#' @param CurrTerm — The input term to be queried.
#' @param ontologies — BioPortal ontology short names (comma-separated).
#' @param apikey — A valid BioPortal API key.
#'
#' @return A tibble containing recommender scores, ontology names,
#'   identifiers, definitions, synonyms, and parent concepts. In case of
#'   errors, a tibble with diagnostic `ID` codes is returned.
NCBO_process_term <- function(CurrTerm, ontologies, apikey) {
  message(crayon::blue(glue::glue("# Processing: {CurrTerm}")))
  Sys.sleep(1) # avoid overuse the API

  # NCBO REST Recommender Tool
  sURL <- sprintf(
    "https://data.bioontology.org/recommender?input=%s&ontologies=%s&apikey=%s",
    URLencode(CurrTerm, reserved = TRUE),
    ontologies,
    apikey
  )

  CampaignJSON <- tryCatch(RCurl::getURL(sURL), error = function(e) e)

  # Error handling for the API call
  if (
    inherits(CampaignJSON, "error") ||
      !jsonlite::validate(CampaignJSON)
  ) {
    message(crayon::red(glue::glue(
      "API error or invalid JSON for term: {CurrTerm}"
    )))
    return(dplyr::tibble(
      Term = CurrTerm,
      Found_term = NA,
      Score = NA,
      Ontology = NA,
      Ontology_URI = NA,
      ID = "ERROR_RECOMMENDER_API",
      Notations = NA,
      Definition = NA,
      Synonyms = NA,
      Parents = NA
    ))
  }

  res <- NCBO_recommender_from_JSON(CurrTerm, CampaignJSON)

  if (is.null(res) || nrow(res) == 0) {
    message(crayon::yellow(glue::glue("--> No recommendations for {CurrTerm}")))
    return(dplyr::tibble(
      Term = CurrTerm,
      Found_term = NA,
      Score = NA,
      Ontology = NA,
      Ontology_URI = NA,
      ID = "ERROR_NO_RECOMMENDATION",
      Notations = NA,
      Definition = NA,
      Synonyms = NA,
      Parents = NA
    ))
  }

  # Use pmap to iterate over rows of the recommender results
  details <- purrr::pmap_dfr(
    list(res$Term, res$Ontology),
    function(term_from_res, iOntology) {
      # Search API call
      sURL2 <- sprintf(
        "https://data.bioontology.org/search?q=%s&ontologies=%s&exact_match=true&include=synonym,definition,notation&include_context=false&include_links=false&apikey=%s",
        URLencode(term_from_res, reserved = TRUE),
        iOntology,
        apikey
      )
      CampaignJSON2 <- tryCatch(RCurl::getURL(sURL2), error = function(e) e)
      if (
        inherits(CampaignJSON2, "error") || !jsonlite::validate(CampaignJSON2)
      ) {
        return(dplyr::tibble(
          ID = "ERROR_SEARCH_API",
          Notations = NA,
          Definition = NA,
          Synonyms = NA,
          Parents = NA
        ))
      }
      iInfo <- NCBO_get_fields_from_JSON(CampaignJSON2)

      # Annotator API call
      sURL3 <- sprintf(
        "https://data.bioontology.org/annotator?text=%s&ontologies=%s&expand_semantic_types_hierarchy=true&apikey=%s",
        URLencode(term_from_res, reserved = TRUE),
        iOntology,
        apikey
      )
      CampaignJSON3 <- tryCatch(RCurl::getURL(sURL3), error = function(e) e)
      if (
        inherits(CampaignJSON3, "error") || !jsonlite::validate(CampaignJSON3)
      ) {
        iInfo_df <- dplyr::as_tibble(as.list(iInfo))
        names(iInfo_df) <- c("ID", "Notations", "Definition", "Synonyms")
        return(dplyr::mutate(iInfo_df, Parents = "ERROR_ANNOTATOR_API"))
      }
      Parents <- NCBO_get_parents_from_JSON(CampaignJSON3, apikey)

      dplyr::tibble(
        ID = iInfo[[1]],
        Notations = iInfo[[2]],
        Definition = iInfo[[3]],
        Synonyms = iInfo[[4]],
        Parents = Parents
      )
    }
  )

  if (nrow(details) == 0) {
    return(NULL)
  }

  # Combine recommender results with the details
  dplyr::bind_cols(res, details)
}

# ------------------------------------------------------------------------------------------------
#' NCBO_mapper
#'
#' This function reads a list of terms and a list of ontologies, and for each term, it uses the NCBO Recommender, Search, and Annotator APIs to find mappings in the specified ontologies. The results are written to a CSV file. This version is refactored to be more efficient by processing all terms and writing to disk only once.
#'
#' @param term_file The path to a CSV file with the terms to map (one term per line).
#' @param ontology_file The path to a CSV file with the ontology acronyms to use for mapping (one acronym per line).
#' @param result_file The path to the output CSV file where the mappings will be saved.
#' @param apikey Your NCBO API key.
#'
#' @return This function does not return a value but writes the mapping results to `result_file` and a list of non-mapped terms to a file with the suffix "NonMapped.csv".
NCBO_mapper <- function(term_file, ontology_file, result_file, apikey) {
  dfTerms <- read.csv(term_file, header = FALSE, stringsAsFactors = FALSE) |>
    dplyr::distinct()

  dfOntologies <- read.csv(
    ontology_file,
    header = FALSE,
    stringsAsFactors = FALSE
  ) |>
    dplyr::distinct()

  ontologies <- as.character(paste(dfOntologies[[1]], collapse = ","))

  # Process all terms and collect results into a single data frame
  all_results <- purrr::map_dfr(
    dfTerms[[1]],
    ~ NCBO_process_term(.x, ontologies, apikey)
  )

  if (is.null(all_results) || nrow(all_results) == 0) {
    message(crayon::red("No results were generated from any term."))
    return()
  }

  # Separate mapped from non-mapped
  mapped_results <- all_results |>
    dplyr::filter(!stringr::str_detect(ID, "ERROR"))
  non_mapped_terms <- setdiff(dfTerms[[1]], unique(mapped_results$Term))

  # Write mapped results once
  sErrorFile <- paste0(tools::file_path_sans_ext(result_file), "NonMapped.csv")

  if (nrow(mapped_results) > 0) {
    write.csv(mapped_results, result_file, row.names = FALSE)
  } else {
    # Create empty file with header if it exists, otherwise empty file
    if (!is.null(names(all_results))) {
      write(paste(names(all_results), collapse = ","), file = result_file)
    } else {
      file.create(result_file)
    }
  }

  # Write non-mapped terms once
  write("NON-MAPPED", file = sErrorFile)
  if (length(non_mapped_terms) > 0) {
    write.table(
      non_mapped_terms,
      file = sErrorFile,
      append = TRUE,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    )
  }

  message(crayon::green(glue::glue(
    "Processing complete. Mapped {length(unique(mapped_results$Term))} terms. See {result_file}."
  )))
  message(crayon::yellow(glue::glue(
    "{length(non_mapped_terms)} terms could not be mapped. See {sErrorFile}."
  )))
}

# ----------------------------------------------------------------------------------------------------
#' NCBO_consensus_terms
#'
#' This function orchestrates a multi-step process to generate a clean,
#'   deduplicated PICO (Population, Intervention, Comparison, Outcome) dictionary.
#'   It starts with a raw list of terms, enriches them with synonyms and parent
#'   concepts from NCBO BioPortal results, and refines them through clustering
#'   and similarity-based merging.
#' @details The process involves the key stages:
#'   1.  **Hierarchical Clustering:** Groups similar terms together based on
#'       Jaro-Winkler string distance to form initial synonym clusters.
#'   2.  **Enrichment:** Merges user-provided alternate names and synonyms/parents
#'       from the NCBO mapping results.
#'   3.  **Normalization & Summarization:** Cleans and aggregates the data,
#'       consolidating synonyms and extracting the top parent concepts for each term.
#'   4.  **Deduplication:** Iteratively compares synonym lists using cosine
#'       similarity to find and merge highly similar entries, ensuring each
#'       conceptual entity is represented only once.
#'   5.  **Finalization:** Constructs the final PICO dictionary with a standardized
#'       set of columns (solesR).
#'
#' @param terms `character`. The file path to a single-column CSV containing the
#'   initial list of terms to process.
#' @param ncbo_results_path `character`. The file path to the CSV file containing
#'   the ontology mapping results generated by the `NCBO_mapper` function.
#' @param terms_alternate_names_df `data.frame`. An optional data frame
#'   with two columns: `terms` and `alternate_names`. This is used to supplement
#'   the synonym generation with manually curated alternatives. Default is `NULL`.
#' @param type `character`. The PICO element type to assign to all
#'   terms in the dictionary (e.g., "Outcome", "Intervention"). Default is `NULL`.
#' @param main_category `character`. A broad category to assign to all
#'   terms, describing the general domain (e.g., "Sample", "Biomarker"). Default is `NULL`.
#' @param q `numeric`. The q-gram size used for the cosine similarity calculation
#'   during the deduplication step (compare list of alternate_names). Default is 5.
#' @param dist `numeric`. The distance used for the cosine similarity calculation
#'   during the deduplication step (compare list of alternate_names). Default is 0.15.
#'
#' @return A `tibble` representing the final PICO dictionary. The data frame has
#'   the following columns:
#'   \itemize{
#'     \item `name`: The chosen primary name for the term/concept.
#'     \item `alternate_names`: A pipe-separated string of synonyms.
#'     \item `type`: The PICO type (e.g., "Outcome").
#'     \item `main_category`: The main domain category (e.g., "Biomarker").
#'     \item `sub_category1`: The most common parent concept from NCBO.
#'     \item `sub_category2`: The second most common parent concept.
#'     \item `regex`: An empty column, intended as a placeholder for regex patterns.
#'   }
NCBO_consensus_terms <- function(
  terms,
  ncbo_results_path,
  terms_alternate_names_df = NULL,
  type = NULL,
  main_category = NULL,
  q = 5,
  dist = 0.15
) {
  # Read the raw list of terms from the specified CSV file.
  terms <- readr::read_csv(
    terms,
    col_name = FALSE,
    show_col_types = FALSE
  ) |>
    dplyr::pull(1)

  # --- 1. Hierarchical Clustering of Terms ---
  # Goal: Group similar-looking terms to form initial synonym clusters.

  # Calculate the Jaro-Winkler distance between all pairs of terms. This metric is
  # effective for short strings like names or keywords.
  dist_matrix <- stringdist::stringdistmatrix(
    terms,
    terms,
    method = "jw",
    useNames = TRUE
  )
  dist_matrix[is.na(dist_matrix)] <- 1 # Handle NA values, treating them as max distance.

  # Perform hierarchical clustering on the distance matrix.
  hc <- hclust(as.dist(dist_matrix))

  # Cut the dendrogram to form clusters. The height is set at the 5th percentile
  # of all branch heights, a heuristic to group only very similar terms.
  hc_groups <- cutree(hc, h = quantile(hc$height, probs = 0.05))

  # Create a data frame with terms and their assigned cluster ID.
  # Within each cluster, concatenate all terms into a pipe-separated synonym list.
  clustered_terms <- data.frame(term = terms, cluster = hc_groups) |>
    dplyr::arrange(cluster) |>
    dplyr::group_by(cluster) |>
    dplyr::mutate(Synonyms = paste(term, collapse = "|"))

  # --- 2. Enrich with Alternate Names (Optional) ---
  # If a data frame of manually curated alternate names is provided, merge them.
  if (!is.null(terms_alternate_names_df)) {
    clustered_terms <- clustered_terms |>
      dplyr::left_join(
        terms_alternate_names_df,
        by = join_by(term == terms),
        relationship = "many-to-many"
      ) |>
      # Append the manual alternate names to the generated synonym lists.
      dplyr::mutate(
        Synonyms = if_else(
          is.na(alternate_names),
          Synonyms,
          paste(Synonyms, alternate_names, sep = "|")
        )
      ) |>
      dplyr::select(-alternate_names)
  }

  # --- 3. Combine with NCBO Ontology Results ---
  # Read the pre-generated results from the NCBO mapper.
  ncbo_info <- readr::read_csv(ncbo_results_path, show_col_types = FALSE)

  # Join the NCBO data with the clustered terms and merge their synonym lists.
  ncbo_info_cluster_syns <- ncbo_info |>
    dplyr::left_join(
      clustered_terms,
      by = join_by(Term == term),
      relationship = "many-to-many"
    ) |>
    # Consolidate synonyms from both sources (NCBO and our clustering).
    dplyr::mutate(
      Synonyms = case_when(
        !is.na(Synonyms.x) & !is.na(Synonyms.y) ~ paste(
          Synonyms.x,
          Synonyms.y,
          sep = "|"
        ),
        !is.na(Synonyms.x) & is.na(Synonyms.y) ~ Synonyms.x,
        is.na(Synonyms.x) & !is.na(Synonyms.y) ~ Synonyms.y,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(-Synonyms.x, -Synonyms.y)

  # --- 4. Summarize, Normalize, and Clean ---
  clean_info <- ncbo_info_cluster_syns |>
    # Unpack all pipe-separated synonyms into individual rows for processing.
    tidyr::separate_rows(Synonyms, sep = "\\|") |>
    dplyr::group_by(Term) |>
    # For each original term, consolidate synonyms and find top parent concepts.
    dplyr::summarise(
      # Create a unique, pipe-separated list of all synonyms for the term.
      Synonyms = Synonyms |>
        na.omit() |>
        unique() |>
        paste(collapse = "|"),
      # Extract the top 2 parent concepts from NCBO, excluding generic "concept" terms.
      {
        top_parents <- Parents |>
          na.omit() |>
          (function(x) x[!grepl("concept", x, ignore.case = TRUE)])() |>
          table() |>
          sort(decreasing = TRUE) |>
          names() |>
          head(2)
        dplyr::tibble(
          Parent_1 = top_parents[1] %||% NA_character_,
          Parent_2 = top_parents[2] %||% NA_character_
        )
      },
      .groups = "drop"
    ) |>
    # Normalize by converting terms to lowercase to group different casings.
    dplyr::mutate(term_norm = tolower(Term)) |>
    dplyr::group_by(term_norm) |>
    # Aggregate data for terms that are identical after normalization.
    dplyr::summarise(
      Term = first(Term), # Keep the first encountered casing as the primary name.
      # Consolidate synonyms from all case-variants.
      Synonyms = {
        all_syns <- c(Synonyms, setdiff(Term, first(Term)))
        all_syns <- unique(na.omit(all_syns))
        if (length(all_syns) == 0) {
          NA_character_
        } else {
          paste(all_syns, collapse = "|")
        }
      },
      Parent_1 = dplyr::first(Parent_1),
      Parent_2 = dplyr::first(Parent_2),
      .groups = "drop"
    ) |>
    # Rename columns to the final PICO dictionary schema.
    dplyr::rename(
      name = Term,
      alternate_names = Synonyms,
      sub_category1 = Parent_1,
      sub_category2 = Parent_2
    ) |>
    # Add standard metadata columns for the dictionary.
    dplyr::mutate(
      type = type,
      main_category = main_category,
      regex = NA_character_,
      keep = TRUE,
      op = NA_character_,
      dist = NA_character_
    ) |>
    dplyr::select(-term_norm) |>
    dplyr::arrange(name)

  # --- 5. Deduplicate via Synonym Similarity ---
  # Goal: Find and merge rows that represent the same concept but have slightly different synonym lists.
  # Note: This is not the most computationally efficient approach and may be optimized or reformulated in future versions.
  for (i in seq_len(nrow(clean_info) - 1)) {
    if (!isTRUE(clean_info$keep[i])) {
      next # Skip rows already marked for removal.
    }

    for (j in seq.int(i + 1, nrow(clean_info))) {
      if (!isTRUE(clean_info$keep[j])) {
        next # Skip rows already marked for removal.
      }

      alt1 <- clean_info$alternate_names[i]
      alt2 <- clean_info$alternate_names[j]

      # Calculate cosine similarity on q-grams of the synonym strings. This is
      # robust for comparing long strings with differing word order.
      dist_alt <- stringdist::stringdist(alt1, alt2, method = "cosine", q = q)

      # A low distance (<= 0.15) indicates high similarity.
      similar <- dist_alt <= dist

      if (isTRUE(similar)) {
        # Strategy: Merge the shorter synonym list into the longer one.
        r1 <- nchar(clean_info$alternate_names[i])
        r2 <- nchar(clean_info$alternate_names[j])

        if (r1 >= r2) {
          # Merge j's synonyms into i and mark j for removal.
          clean_info$alternate_names[i] <- paste(
            clean_info$alternate_names[i],
            clean_info$alternate_names[j],
            sep = "|"
          )
          clean_info$keep[j] <- FALSE
          clean_info$op[j] <- clean_info$name[i] # Record which row it was merged into.
          clean_info$dist[j] <- dist_alt
        } else {
          # Merge i's synonyms into j and mark i for removal.
          clean_info$alternate_names[j] <- paste(
            clean_info$alternate_names[j],
            clean_info$alternate_names[i],
            sep = "|"
          )
          clean_info$keep[i] <- FALSE
          clean_info$op[i] <- clean_info$name[j]
          clean_info$dist[i] <- dist_alt

          break # Stop inner loop since row 'i' is now marked for removal.
        }
      }
    }
  }

  # --- 6. Final Cleanup and Formatting ---
  # Post-deduplication cleanup to ensure synonym lists are unique.
  clean_info <- clean_info |>
    # Create unique synonym lists again after merging.
    tidyr::separate_rows(alternate_names, sep = "\\|") |>
    dplyr::reframe(
      # Preserve metadata from the first row in the group.
      dplyr::across(
        c(
          type,
          main_category,
          sub_category1,
          sub_category2,
          regex,
          keep,
          op,
          dist
        ),
        ~ .[1]
      ),
      # Concatenate all alternate names into a final, unique, pipe-separated list.
      alternate_names = alternate_names |>
        na.omit() |>
        unique() |>
        paste(collapse = "|"),
      .by = name
    )

  # Filter to keep only the desired rows and select final columns.
  pico_dictionary <- clean_info |>
    dplyr::filter(keep) |>
    dplyr::select(
      name,
      alternate_names,
      type,
      main_category,
      sub_category1,
      sub_category2,
      regex
    )

  return(pico_dictionary)
}
