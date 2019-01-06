# test-helpers.R


# Prep
header <- c("serialno", "name", "phone", "address", "email", "bday.day",
            "bday.mth", "wedann.day", "wedann.mth", "occupation", "church",
            "pastor", "info.source")
samplFile1 <- exhale::excelfile('test-file-1.xls')
samplFile2 <- exhale::excelfile('test-file-2.xlsx')
singleWrkSht1 <- extractSpreadsheets(samplFile1) %>% .[[1]]
singleWrkSht2 <- extractSpreadsheets(samplFile2) %>% .[[1]]

SheetsList <- lapply(list(singleWrkSht1, singleWrkSht2), function(dfr) {
  beacon <- locate_header(dfr, hdr = header)
  dfr <- dfr %>%
    slice(beacon$nextrow:n())
  colnames(dfr) <- beacon$header
  dfr
})

# Tests
# ......................
context('File listing')
# ``````````````````````

test_that('Messages are printed', {
  expect_message(listExcelFiles('test-dir'))
})


#...............................
context("Spreadsheet integrity")
#```````````````````````````````


singleWrkSht1 <- SheetsList[[1]]
singleWrkSht2 <- SheetsList[[2]]

test_that("Spreadsheets are properly extracted", {
    expect_is(singleWrkSht1, "tbl_df")
    expect_is(singleWrkSht1, "tbl")
    expect_is(singleWrkSht1, "data.frame")
    expect_false(inherits(singleWrkSht1, "excelfile"))
    expect_is(singleWrkSht2, "tbl_df")
    expect_is(singleWrkSht2, "tbl")
    expect_is(singleWrkSht2, "data.frame")
    expect_false(inherits(singleWrkSht2, "excelfile"))
})


#..............................
context("Regular expressions")
#``````````````````````````````
test_that("Object class with regex patterns is properly instantiated", {
    column <-
        c("24 Feb/7 Sept",
          "43322",
          "17 January",
          "15 Oct",
          NA,
          "May 5/Jun 12",
          "6/04")
    column <- hellno::as.data.frame(column)
    names(column) <- "BDAY"
    pat <- regexPatterns()

    expect_error(regexIndices(pat, column), "Numbers-only values must be")

    column <- filter(column, BDAY != "43322")
    ind <- regexIndices(pat, column)

    expect_s3_class(pat, "regexPatterns")
    expect_s3_class(ind, "regexIndices")
})



#........................
context("Data integrity")
#````````````````````````
test_that("Wrong mobile numbers are repaired or removed.", {
    numbers <-
        as.data.frame(
            c(
                "123456789",
                "0123456789",
                "8000000001",
                "9012345678",
                "07098765432",
                "08123456789",
                "09064321987",
                "O8055577889",
                "070456789011"
            )
        )
    numbers <- .fixPhoneNumbers(numbers)

    # Tests proper
    expect_type(numbers, "character")
    expect_true(is.na(numbers[1]))
    expect_true(all(nchar(na.omit(numbers)) == 11))
})


#..................................
context("Data frame restructuring")
#``````````````````````````````````

test_that("'Month' values are corrected.", {
    mths <-
        c("Apl", "oct ", "September", "Jul", "january", "aug", "decc", "  March")
    mths <- .fixMonthEntries(mths)
    failed <- c("May/Jun 12", "24 Feb/Sept")

    expect_equal(mths[[1]][1], "April")
    expect_equal(mths[[1]][2], "October")
    expect_equal(mths[[1]][3], "September")
    expect_equal(mths[[1]][4], "July")
    expect_equal(mths[[1]][5], "January")
    expect_equal(mths[[1]][6], "August")
    expect_equal(mths[[1]][7], "December")
    expect_equal(mths[[1]][8], "March")
    expect_error(.fixMonthEntries("Mar."),
                 "An invalid character was found at position 1.")
    expect_error(.fixMonthEntries(failed), "An invalid character")
})

test_that("Date entries are fixed", {
    mail1 <- data.frame(Name = c("Abe McDonald", "Max Horst", "Ada Obi"),
                        EMAIL = c("abo@mail.com", NA, "adaobi@hot.com"),
                        BIRTHDAY = c("12-oct", "13/06/1985", "Mar.18"),
                        stringsAsFactors = FALSE)
    mail2 <- data.frame(
        NAME = c("Heather Rowe", "Zainab Umar", "Tinuke"),
        `BIRTHDAY AND WED ANN` = c("12 June 1982", "Apl. 23", "23rd Dec"),
        `WED ANN` = c("", "10/4", "January 5th"),
        stringsAsFactors = FALSE, check.names = FALSE)

    newDates1 <- fixDateEntries(mail1)
    newDates2 <- fixDateEntries(mail2)

    # Add a test for cases when there are no funny looking columns
    expect_is(newDates1, "data.frame")
    expect_is(newDates2, "data.frame")
    expect_false(identical(ncol(mail1), ncol(newDates1)))
    expect_false(identical(ncol(mail2), ncol(newDates2)))
    expect_false(ncol(mail1) == ncol(newDates1))
    expect_false(ncol(mail2) == ncol(newDates2))
    expect_equal(newDates1$bday.day[1], "12")
    expect_equal(newDates1$bday.mth[1], "October")
    expect_equal(newDates1$bday.day[2], "13")
    expect_equal(newDates1$bday.mth[2], "June")
    expect_equal(newDates1$bday.day[3], "18")
    expect_equal(newDates1$bday.mth[3], "March")
    expect_equal(newDates2$bday.day[1], "12")
    expect_equal(newDates2$bday.mth[1], "June")
    expect_equal(newDates2$bday.day[2], "23")
    expect_equal(newDates2$bday.mth[2], "April")
    expect_equal(newDates2$bday.day[3], "23")
    expect_equal(newDates2$bday.mth[3], "December")
})

test_that("numeric Excel dates are converted to text", {

    expect_error(.convertNumDateToChar(c("41235", "43322")),
                 "More than one entry was provided")
    expect_equal(.convertNumDateToChar(c("41235")), "22 November")
    expect_equal(.convertNumDateToChar(c("43322")), "10 August")
})


test_that("unwanted characters/entries are removed", {

    smpl <- c("24 Feb/Sept",
              "Mar/Jun 10",
              "April. 4",
              "Aug, 6",
              "12/05/1989",
              "31 Oct/6 July")
    res <- .cleanupDateEntries(smpl)

    expect_equivalent(res[1], "24 Feb")
    expect_equivalent(res[2], "")
    expect_equivalent(res[3], "April  4")
    expect_equivalent(res[4], "Aug  6")
    expect_equivalent(res[5], "12/05")
    expect_equivalent(res[6], "31 Oct/6 July")  # returned unchanged

    ## Check for return of named character vector, source of previous bug
    expect_identical(smpl, attr(res, "name"))
})
