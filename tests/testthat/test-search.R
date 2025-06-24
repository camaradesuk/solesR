test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# 
# # upload files
# wos1 <- manual_upload("wos.ris", source = "wos")
# wos2 <- manual_upload("wos.bib", source = "wos")
# medline <- manual_upload("medline.xml", source = "xml")
# scopus <- manual_upload("scopus.ris", source = "scopus") 
# psychinfo <- manual_upload("psychinfo.ris", source = "psychinfo")
# embase <- manual_upload("embase.ris", source = "embase")
# eric <- read_xml("eric.xml", source="xml")
# 
# # Load the functions
# source("your_script_with_functions.R")  # Assuming your functions are in this script

# Define the tests
test_that("manual_upload() function works as expected", {
  # Define test cases
  wos1 <- manual_upload("wos.ris", source = "wos")
  wos2 <- manual_upload("wos.bib", source = "wos")
  medline <- manual_upload("medline.xml", source = "endnote")
  scopus <- manual_upload("scopus.ris", source = "scopus") 
  psychinfo <- manual_upload("psychinfo.ris", source = "psychinfo")
  embase <- manual_upload("embase.ris", source = "embase")
  eric <- manual_upload("eric.xml", source="endnote")
  
  # Write assertions for each test case
  expect_true(is.data.frame(wos1), info = "wos1 should be a data frame")
  expect_true(is.data.frame(wos2), info = "wos2 should be a data frame")
  expect_true(is.data.frame(medline), info = "medline should be a data frame")
  expect_true(is.data.frame(scopus), info = "scopus should be a data frame")
  expect_true(is.data.frame(psychinfo), info = "psychinfo should be a data frame")
  expect_true(is.data.frame(embase), info = "embase should be a data frame")
  expect_true(is.data.frame(eric), info = "eric should be a data frame")
})
