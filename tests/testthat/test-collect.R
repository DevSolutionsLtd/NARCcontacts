## test-collect.R

context("Excel file collation")

test_that("input is validated", {
  expect_error(collate_all_excel(dest = "path/to/fake"),
               "Directory ‘path/to/fake’ does not exist")
})
