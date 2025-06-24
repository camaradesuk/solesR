# Test manual search upload function ===========================================

# Expect dataframe return on valid input
test_that("manual_upload() function works as expected", {
  # Define test cases
  wos1 <- manual_upload("search_manual/wos.ris", source = "wos")
  wos2 <- manual_upload("search_manual/wos.bib", source = "wos")
  medline <- manual_upload("search_manual/medline.xml", source = "endnote")
  scopus <- manual_upload("search_manual/scopus.ris", source = "scopus") 
  psychinfo <- manual_upload("search_manual/psychinfo.ris", source = "psychinfo")
  embase <- manual_upload("search_manual/embase.ris", source = "embase")
  eric <- manual_upload("search_manual/eric.xml", source="endnote")
  
  # Write assertions for each test case
  expect_true(is.data.frame(wos1), info = "wos1 should be a data frame")
  expect_true(is.data.frame(wos2), info = "wos2 should be a data frame")
  expect_true(is.data.frame(medline), info = "medline should be a data frame")
  expect_true(is.data.frame(scopus), info = "scopus should be a data frame")
  expect_true(is.data.frame(psychinfo), info = "psychinfo should be a data frame")
  expect_true(is.data.frame(embase), info = "embase should be a data frame")
  expect_true(is.data.frame(eric), info = "eric should be a data frame")
})

# Expect error on invalid inputs
test_that("manual_upload() function handles errors", {
  # Error on invalid file path
  expect_error(manual_upload("search_manual/missingfile.bib", source = "wos"))
  # Error on invalid source
  expect_error(manual_upload("search_manual/wos.bib", source = "web of science"))
  # Expect error on invalid file type for source
  expect_error(manual_upload("seach_manual/wos.bib", source = "endnote"))
})