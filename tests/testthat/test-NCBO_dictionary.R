test_that("NCBO_ontologies returns unique top ontologies", {
  res <- NCBO_ontologies(
    terms_list = here::here("regex", "term_list"),
    apikey = Sys.getenv('apikey_NCBO'),
    file_path = "regex",
    n_sample = 2,
    n_terms = 5,
    topn_onto = 2
  )

  expect_true(file.exists(file.path(
    "regex",
    "ontology_list.csv"
  )))

  file_content <- readLines(file.path(
    "regex",
    "ontology_list.csv"
  ))

  expect_setequal(file_content, c("LOINC", "MESH", "NCIT"))
})

test_that("NCBO_mapper returns dataframe with NCBO infos", {
  res <- NCBO_mapper(
    term_file = "regex/term_list",
    ontology_file = "regex/ontology_list.csv",
    result_file = "regex/NCBOmapped_test.csv",
    apikey = Sys.getenv('apikey_NCBO')
  )

  expect_true(file.exists(file.path(
    "regex",
    "NCBOmapped_test.csv"
  )))

  expect_true(file.exists(file.path(
    "regex",
    "NCBOmapped_testNonMapped.csv"
  )))

  file_content <- read.csv(file.path(
    "regex",
    "NCBOmapped_test.csv"
  ))

  expect_equal(ncol(file_content), 10)
})

test_that("NCBO_consensus_terms returns a valid dataframe with solesR format", {
  alternate_df <- read.csv("regex/terms_alternate_names_df.csv")

  res <- NCBO_consensus_terms(
    "regex/term_list",
    "regex/NCBOmapped_test.csv",
    terms_alternate_names_df = alternate_df,
    type = "test",
    main_category = "test"
  )

  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 7)
})
