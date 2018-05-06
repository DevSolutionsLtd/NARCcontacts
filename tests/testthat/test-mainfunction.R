# test-mainfunction.R

context("Input validation")

test_that("the main function recognises illegal input", {
  expect_error(harmonise_narc_excel(),
               "argument \"dir\" is missing, ",
               fixed = TRUE)
  expect_error(harmonise_narc_excel(9),
               "is.character(dir) is not TRUE",
               fixed = TRUE)
  expect_error(harmonise_narc_excel("fake"),
               "There is no directory called ",
               fixed = TRUE)
  expect_warning(harmonise_narc_excel(c("test-dir", "dud"), quiet = TRUE),
                 "Length of 'dir' is > 1",
                 fixed = TRUE)
  # expect_warning(harmonise_narc_excel("fake"), "The system cannot find ")

  unlink("test-dir/harmonised-data", recursive = TRUE, force = TRUE)
})
