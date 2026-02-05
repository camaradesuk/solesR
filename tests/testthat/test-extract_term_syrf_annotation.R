test_that("extract_term_syrf_annotation works as expected", {
  syrf_df <- data.frame(
    StudyId = 1:4,
    `Disease._48b9.a2b3.32eeeb0b43de_Answer` = c(
      "cancer (tumor)",
      "diabetes [D]; blood sugar (glicose)",
      "heart disease and stroke",
      "asthma or {breathing difficulty}"
    )
  )

  expected_terms <- c(
    "cancer",
    "tumor",
    "diabetes",
    "blood sugar",
    "glicose",
    "heart disease",
    "stroke",
    "asthma",
    "breathing difficulty"
  )

  extracted_terms <- extract_term_syrf_annotation(
    annotation_df = syrf_df,
    annotated_column = "Disease._48b9.a2b3.32eeeb0b43de_Answer",
    res_filename = "clean_term_list.csv",
    file_path = "regex"
  )

  saved_terms <- readLines("regex/clean_term_list.csv")

  expect_equal(sort(extracted_terms), sort(expected_terms))
  expect_equal(sort(saved_terms), sort(expected_terms))
})
