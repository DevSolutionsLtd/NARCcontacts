# helpers.R

globalVariables('.')

################################
##         S3 objects         ##
################################

## S3 Object for regular expressions
##  It establishes the patterns for entries that have:
##   => two numbers separated by a forward slash e.g. 10/6
##   => two day-month combos separated by a fwd slash e.g. May 6/June 12
##   => a "DD Month" e.g. 15 Oct
##   => an Excel date numeral e.g. "43322" equiv. to 12 Aug 2018
##   => entries that have three date fields e.g. 13/03/2001, 13th March 2001
regexPatterns <- function() {
    dd_mm <- "([0-3]*[0-9])(\\s+)([[:alpha:]]{3,})"
    mm_dd <- "([[:alpha:]]{3,})(\\s+)([0-3]*[0-9])"
    beg <- "^"
    end <- "$"
    slash <- "(\\s*/\\s*)"
    structure(
        list(
            date_numeral = "^[1-9][0-9]{4}$",
            num_slash_num =
                paste0(beg, "([0-3]*[0-9])", slash, "([0-1]*[0-9])", end),
            sng_dd_mm = paste0(beg, dd_mm, end),
            sng_mm_dd = paste0(beg, mm_dd, end),
            dbl_dd_mm =
                paste0(beg, dd_mm, slash, dd_mm, end),
            dbl_mm_dd =
                paste0(beg, mm_dd, slash, mm_dd, end)
        ),
        class = "regexPatterns"
    )
}












## S3 object for determinating which indices match any of the patterns
## in the 'regexPattern' object, within a particular date-related column
## @note Objects of this class must not be created until all pure 'numerical'
## date entries have been converted to character DD-MM or MM-DD values.
regexIndices <- function(rules, col) {
    if (!inherits(col, "data.frame"))
        stop("Expected 'col' to be of class 'data.frame'")
    ls <- lapply(rules, function(x) grep(x, unlist(col), ignore.case = TRUE))

    allIndices <- c(ls$numeral,
                    ls$twoNum,
                    ls$sng_dd_mm,
                    ls$sng_mm_dd,
                    ls$dbl_dd_mm,
                    ls$dbl_mm_dd)

    if (anyDuplicated(allIndices)) {
        dups <- duplicated(allIndices)
        stop(
            "A pattern-matching conflict occurred, with duplications at pos ",
             sQuote(which(dups))
            )
    }

    if (any(grepl("^\\d+$", unlist(col))))
        stop("Numbers-only values must be converted
             before creating objects of this class.")

    attr(ls, "class") <-  "regexIndices"
    invisible(ls)
}

###################################################################
##         Custom functions created for 'harmonise.R'            ##
###################################################################

## Finds all existing Excel files within a given directory
listExcelFiles <- function(path = ".", quietly = FALSE)
{
  # TODO: There are situations when a file's extension
  # may not be specified and thus there may be need to
  # test such files to know whether they are of the format.

  # Get path to EXcel files and remove any backup files (Windows, sigh!)
  xlFiles <-
    list.files(path, pattern = ".xlsx$|.xls$", full.names = TRUE) %>%
    subset(!grepl("(~\\$)+", .))

  if (isFALSE(numFiles <- length(xlFiles)))
    stop("There are no Excel files in this directory.")
  if (isFALSE(quietly)) {
    message(sprintf(
      paste0(
        "** %d Excel file",
        ngettext(numFiles, ' was', 's were'),
        ' found in %s:\n'
      ),
      numFiles,
      sQuote(path.expand(path))
    ), appendLF = FALSE)

    ## Print the list of files
    sapply(xlFiles, function(x) {
      message(sprintf("*** %s\n", basename(x)), appendLF = FALSE)
    })
  }
  xlFiles
}








#' Collect Excel Files
#'
#' Searches the machine's HOME directory tree for Excel files and puts them
#' into a separate directory.
#'
#' @details This function is a wrapper for \code{\link[exhale]{collate_excel}}
#'
#' @return The function has no return value.
#'
#' @importFrom exhale collate_excel
#'
#' @export
gather_excel_files <- function()
{
  collate_excel()
}





## Extracts spreadsheets from an excelfile object.
## Note that this function cannot retrieve spreadsheets
## directly from an unprocessed .xls/.xlsx file
extractSpreadsheets <- function(fileObj) {
    if (inherits(fileObj, "excelfile")) {
        lapply(fileObj$data, function(dat) return(dat))
    }
    else {
        stop("'fileObj' is not an 'excelfile' object.")
    }
}













## Updates old column names of the data frame with the new
## ones that are to be used in the harmonised version
updateHeader <- function(df, newCol) {
    if (!is.data.frame(df))
        stop("'df' is not a valid data frame")

    stopifnot(length(newCol) == 13)

    if (!is.character(newCol))
        stop("'newCol' is not a character vector")

    initialHdr <- colnames(df)
    modifiedHdr <- sapply(initialHdr, function(thisCol) {
        ## These are the values we're picking from 'newCol':
        ### [1]  "serialno"     [2]  "name"        [3]  "phone"
        ### [4]  "address"      [5]  "email"       [6]  "bday.day"
        ### [7]  "bday.mth"     [8]  "wedann.day"  [9]  "wedann.mth"
        ### [10] "occupation"   [11] "church"      [12] "pastor"
        ### [13] "info.source"
        if (identical(thisCol, "S/N"))
            thisCol <- newCol[1]
        else if (grepl("name", tolower(thisCol)))
            thisCol <- newCol[2]
        else if (grepl("number|phone", tolower(thisCol)))
            thisCol <- newCol[3]
        else if (grepl("ress$", tolower(thisCol))
                 && !grepl("email", tolower(thisCol)))
            thisCol <- newCol[4]
        else if (newCol[5] %in% tolower(thisCol))
            thisCol <- newCol[5]
        else if ("bday.day" %in% thisCol)
            thisCol <- newCol[6]
        else if ("bday.mth" %in% thisCol)
            thisCol <- newCol[7]
        else if ("wedann.day" %in% thisCol)
            thisCol <- newCol[8]
        else if ("wedann.mth" %in% thisCol)
            thisCol <- newCol[9]
        else if (grepl("occupation", thisCol, ignore.case = TRUE))
            thisCol <- newCol[10]
        else if (grepl("church", tolower(thisCol)))
            thisCol <- newCol[11]
        else if (grepl("pastor$", tolower(thisCol)))
            thisCol <- newCol[12]
        else if (grepl("know", thisCol, ignore.case = TRUE))
            thisCol <- newCol[13]
    })
    colnames(df) <- modifiedHdr
    invisible(df)
}







#' Rearrange data frame columns
#'
#' Rearranges the columns of data frames to suit the prescribed
#' format. Columns without values are assigned NA.
#'
#' @param df An object of class \code{data.frame}
#' @param hdr A character vector containing names of outputted data frame
#' appearing in the desired order.
#'
#' @return A data frame.
#'
#' @export
# TODO: Write test.
rearrange_df <- function(df, hdr) {
    if (!is.data.frame(df))
        stop("'df' is not a valid data frame.")
    if (!is.character(hdr))
        stop("'hdr' ought to be a character vector.")

    height <- nrow(df)
    oldHeader <- colnames(df)
    newDf <- data.frame(seq_along(height))

    for (i in 2:length(hdr)) {
        x <- hdr[i]

        if (x %in% oldHeader) {
            index <- match(x, oldHeader)
            newCol <- df[, index]
        } else {
            newCol <- rep(NA, height)
        }

        newDf <- cbind(newDf, newCol)
    }

    colnames(newDf) <- hdr
    newDf
}







## Combines all the data frames in the list into one master data frame
#' @importFrom dplyr bind_rows
combineDataFrames <- function(dfs) {
    final <- dfs[[1]]
    for (i in 2:length(dfs)) {
        singleton <- dfs[[i]]
        final <- bind_rows(final, singleton)
    }
    final
}








## Sets the columns to the appropriate data types
#' @importFrom dplyr %>%
setDataTypes <- function(df) {
    stopifnot(inherits(df, "data.frame"))

    colm <- colnames(df)
    chars <- colm[c(2:5)]
    cats <- colm[c(7, 9:13)]
    nums <- colm[c(1, 6, 8)]

    lapply(colm, function(var) {
        if (var %in% chars) {
            if (identical(var, "phone"))
                df[[var]]  <- fix_phone_numbers(df[[var]])
            df[[var]] <- as.character(df[[var]])
        }
        else if (var %in% cats) {
            if (endsWith(var, "mth"))
                df[[var]] <-
                    factor(df[[var]], levels = month.name, ordered = TRUE)
            else
                df[[var]]  <- as.factor(df[[var]])
        }
        else if (var %in% nums) {
            if (identical(var, "serialno"))
                df[[var]]  <- seq_len(nrow(df))
            df[[var]]  <- as.integer(df[[var]])
        }
        else warning(sprintf(
            "The data type of the '%s' variable could not be set.", var
        ))
    }) %>%
        as.data.frame(col.names = colm, stringsAsFactors = FALSE)
}









## Corrects the various kinds of date entries found in the dataset by
## creating new columns that contain only day and month values
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#' @importFrom stats na.exclude
fixDateEntries <- function(df) {
  ## Identify by name columns in the original data frame
    ## that are likely to contain the awkard entries
    awkward <- c(
        "BDAY/WED ANN",
        "BIRTHDAY AND WEDDING ANN",
        "WED ANN",
        "BIRTHDAY",
        "BDAY",
        "BIRTHDAY AND WED ANN"
    )

    fields <- colnames(df)

    indexAwk <- match(awkward, fields) %>%
        na.exclude() %>%
        sort()

    if (length(indexAwk)) {
        awkColHdr <- fields[indexAwk]
        tempdf <- .processDateEntries(df, awkColHdr)
        df <- bind_cols(df, tempdf)

        df <- df[, -indexAwk]
    }
    df
}










## Moves down a column with date entries to carry out
## necessary processing. Principally these 3 actions take place:
## 1. Cleaning up of entries
## 2. Conversion of 'pure' numbers to day/month (in words)
## 3. Assignment of day and month values to new distinct columns
## 4. Changing of all abbreviated month names to full version
## @return a four-column data frame with days and months of
## birthdays and wedding anniversaries, respectively
##
#' @import hellno
.processDateEntries <- function(df, awkardColNames) {
  stopifnot(is.character(awkardColNames)) # TODO: Rather pattern compatibility?
  newColHdr <- c("bday.day", "bday.mth", "wedann.day", "wedann.mth")
  temp.df <-
    data.frame(matrix("", nrow = nrow(df), ncol = length(newColHdr)))
  lapply(awkardColNames, function(name) {
    dfColumn <- df[[name]] %>%
      .cleanupDateEntries()
    dfColumn <-
      sapply(dfColumn, .convertNumDateToChar, USE.NAMES = FALSE) %>%
      as.data.frame()
    temp.df <<- .distributeDateValues(temp.df, dfColumn, name)
  })
  colnames(temp.df) <- newColHdr
  temp.df <- .changeToFullMonthNames(temp.df)
}










## Converts entries that consist of two numbers separated by a forward
## slash into a leading numeral for days and fully spelt out month
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
.convertDblDigitsToChar <- function(entry, thisPat) {
  if (grepl(thisPat, entry)) {
    mnthString <-
      entry %>%
      str_replace(thisPat, "\\3") %>%
      str_trim() %>%
      as.numeric()
    mnthString <- month.name[mnthString]
  }
  else {
    return(entry)
  }

  updatedStr <-
    paste(str_trim(gsub(thisPat, "\\1", entry)), mnthString)
}












## Takes all abbreviated momth names and changes them to the full thing
.changeToFullMonthNames <- function(tempDF) {
    monthColumns <- which(endsWith(colnames(tempDF), "mth"))
    for (col in monthColumns) {
        months <- unlist(tempDF[, col])
        tempDF[, col] <- .fixMonthEntries(months)
    }
    invisible(tempDF)
}









## Removes characters that are not required for date entries
## - 'months' that have no 'day' specified,
## - 'year' entries,
## - ordinal qualifiers,
## - dots, commas and hyphens, and
## - whitespace
#' @importFrom stringr %>%
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
.cleanupDateEntries <- function(datesCol) {
  res <- sapply(datesCol, function(cell) {
    cell %>%
      str_replace("(^\\s*[[:digit:]]{1,2}\\s+[[:alpha:]]+)/[[:alpha:]]+\\s*$",
                  replacement = "\\1") %>%
      str_replace(
        "^\\s*[[:alpha:]]{3,}/[[:alpha:]]{3,}\\s*[[:digit:]]{1,2}\\s*$",
        replacement = ""
      ) %>%
      str_replace("(/|\\s)([[:alnum:]]{2,})(/|\\s)([[:digit:]]{4}$)",
                  replacement = "\\1\\2") %>%
      str_replace("nd|rd|st|th", replacement = "") %>%
      str_replace_all("[,|.|-]", replacement = " ") %>%
      str_trim()
  })
  res
}








## Works on the Excel 'Date' numeric values
## Note that we have set aside a correction of 2:
## - one for the origin, which Excel includes unlike
##   the POSIX standard that we are using in R, and
## - the 1900 that EXcel erroneously identifies as a leap year.
## Receives as input a single numerical date entry
.convertNumDateToChar <- function(entry) {
  if (length(entry) > 1L)
    stop("More than one entry was provided")
  number <- suppressWarnings(as.numeric(entry))
    if (is.na(number))
      return(entry)

  # TODO:
  ## 1. Add condition for case where full date is counted
  ## and the year is not the present year (use min reasonable value)
  ## 2. Also do not accept dates that are in the future

  correct <- 2L
  invisible(
    format(as.Date(number - correct, origin = "1900-01-01"), "%d %B")
    )
}









## Apportions a component of an entered date to an appropriate new column
## Before setting the indices for the patterns, we have to convert
## those entries with the numerical (D)D/MM format character-based
## month values e.g. from 13/06 to 13 June
#' @import hellno
#' @importFrom dplyr %>%
.distributeDateValues <- function(tempDF, column, col.name) {
    stopifnot(identical(nrow(tempDF), nrow(column)))
    rules <- regexPatterns()
    column <- sapply(unlist(column), function(x) {
      .convertDblDigitsToChar(x, rules$num_slash_num)
        }) %>% as.data.frame()
    indices <- regexIndices(rules, column)
    replaceValues <- function(rule, index, backref) {
        column <- unlist(column)
        if (any(grepl(rule, column))) {
            vals <- .extractDateValues(column, rule, index, backref)
            tempDF[[i]] <<- replace(tempDF[[i]], index, vals)
        }
    }
    for (i in 1:ncol(tempDF)) {
        if (startsWith(col.name, "B") & (i < 3L) |
            startsWith(col.name, "W") & (i > 2L)) {
            beacon <- rep(c("\\1", "\\3"), 2)
            replaceValues(rules$sng_dd_mm, indices$sng_dd_mm, beacon[i])
            beacon <- rep(c("\\3", "\\1"), 2)
            replaceValues(rules$sng_mm_dd, indices$sng_mm_dd, beacon[i])
        }
        beacon <- c("\\1", "\\3", "\\5", "\\7")
        replaceValues(rules$dbl_dd_mm, indices$dbl_dd_mm, beacon[i])
        beacon <- c("\\3", "\\1", "\\7", "\\5")
        replaceValues(rules$dbl_mm_dd, indices$dbl_mm_dd, beacon[i])
    }
    invisible(tempDF)
}












## Extracts day and month as the case may be.
## @param rules a regular expression
## @param beacon a backreference used for substition of grouped patterns
## @details whitespace trimming is also carried out
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
.extractDateValues <- function(column, rules, index, beacon) {
    column[index] %>%
        str_replace(rules, beacon) %>%
        str_trim()
}









## Corrects abbreviated or badly entered 'month' values.
## Should accept only alphabetical characters.
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom stringr str_to_title
#' @importFrom stringr str_trim
.fixMonthEntries <- function(mth.col) {
  stopifnot(is.vector(mth.col))

  isInvalid <- grepl("[[:punct:]]|[[:digit:]]", mth.col)
  if (any(isInvalid)) {
    pos <- which(isInvalid)
    stop(sprintf("An invalid character was found at position %d.", pos))
  }

  sapply(mth.col, function(string) {
    str <- str_trim(string)
    str <-
      gsub(
        "^(Ja|F|Mar|Ap|May|Jun|Jul|Au|S|O|N|D)[[:alpha:]]*$",
        "\\1",
        str,
        ignore.case = TRUE
      )
    str <- str_to_title(str)
    str <- pmatch(str, month.name)
    month.name[str]
  }) %>% as_tibble()
}
