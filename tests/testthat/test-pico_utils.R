test_that("clean_regex_file() works as expected", {
  regex_file <- data.table::fread(testthat::test_path("regex/regex_file.csv"))
  req_cols = c(
    "name",
    "regex",
    "type",
    "main_category",
    "sub_category1",
    "sub_category2"
  )
  # input col names must be correct
  expect_contains(colnames(regex_file), req_cols)

  # output must be dataframe
  dat <- clean_regex_file(regex_file)
  expect_true(is.data.frame(dat))

  # output col names must be correct
  expect_contains(colnames(dat), req_cols)
})
