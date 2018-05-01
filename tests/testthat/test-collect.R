## test-collect.R

context("Excel file collation")

test_that("input is validated", {
  expect_error(collate_excel(dest = "path/to/fake"))
})

testDirectory <- "test-dir"
discard <- function() unlink("collated", force = TRUE, recursive = TRUE)

 test_that("verbosity is implemented", {
  expect_output(collate_excel(dir = testDirectory, dest = "."),
                "Root directory")
  discard()
  expect_output(collate_excel(testDirectory, dest = "."),
                "R is building the directory list")
  discard()
  expect_output(collate_excel(testDirectory, dest = "."),
                "Excel file(s) in", fixed = TRUE)
  discard()
  expect_output(collate_excel(testDirectory, dest = "."),
                              "Copying files", fixed = TRUE)
  discard()
  # expect_output(collate_excel(testDirectory, dest = "."),
  #               "Files were successfully copied into", fixed = TRUE)
  # discard()
  })

test_that("Custom function takes only character vector of length 1", {
  fileList <- .getThisDirExcelFiles(testDirectory, "/")

  expect_error(.getThisDirExcelFiles(c(testDirectory, "dud"), "/"),
               "identical(length(d), 1L) is not TRUE", fixed = TRUE)
  expect_is(fileList, "character")
  expect_equal(length(fileList), 4)
  expect_true(all(grepl(".xls$|.xlsx$", fileList)))
})
