# test-consolidate.R

library(RSQLite)
library(stringi)
library(purrr)

context("Storage of consolidated data")

test_that("Input is validated", {
  expect_error(consolidate_narc_mail())
  expect_error(consolidate_narc_mail(data.frame(2), data.frame(2)))
  expect_error(consolidate_narc_mail('fake.file/path.wht', 'dud'))
})

test_that("Data are properly stored", {

  # Test saving of brand-new database
  dtTbl <- stri_rand_strings(1, 7, pattern = '[A-Za-z]')
  dFrame <-
    data.frame(a = letters, b = LETTERS, c = seq_len(length(letters)))
  database <- 'data/test.db'
  storeConsolidatedData(dFrame, database, table = dtTbl)

  # Test overwriting
  dFrame2 <- map_df(dFrame, function(x) sample(x, size = 26, replace = TRUE))
  storeConsolidatedData(dFrame2, database, table = dtTbl)
})
