#' Harmonise Nehemiah's Data
#'
#' Merge mailing list belonging to Nehemiah Apostolic Resource Centre
#'
#' @details Mailing list data that exist in different MS Excel files are
#' nto one are discovered and combined into a single SQLite database file.
#' Necessary transformations are carried out to ensure fitness.
#'
#' @param dir character vector of length 1; the path to the folder contains
#' the Excel files.
#' @param dest directory where the data will be stored.
#' @param newcols The new columns that are to be created.
#' @param dbTable The database table where the end-product are stored.
#' @param quiet logical; whether to print informative output or not.
#'
#' @import dplyr
#' @import RSQLite
#' @importFrom exhale excelfile
#' @importFrom exhale locate_header
#' @importFrom tools toTitleCase
#'
#' @export
harmonise_narc_excel <- function(dir,
                                 dest = "./data",
                                 newcols,
                                 dbTable = "NARC_mail",
                                 quiet = FALSE)
{
  # TODO: This function is way too large!
  stopifnot(is.character(dir))
  if (length(dir) > 1) {
    dir <- dir[1]
    warning("Length of 'dir' is > 1 and only the first element was used")
  }
  dir <- suppressWarnings(normalizePath(dir))
  if (!dir.exists(dir))
    stop(paste("There is no directory called", sQuote(basename(dir))))

  msgIndex <- .print_msg()
  filepaths <- listExcelFiles(dir, quietly = quiet)
  msgIndex <- .print_msg(msgIndex)
  excelList <- lapply(filepaths, excelfile)

  ## In case there's more than one spreadsheet in a workbook
  df.ls <- extractSpreadsheets(excelList[[1]])
  len <- length(excelList)
  if (len > 1) {
    for (i in 2:len) {
      tmp <- extractSpreadsheets(excelList[[i]])
      df.ls <- append(df.ls, tmp)
    }
    df_row_num <- sapply(df.ls, nrow)
    df.ls <- df.ls[which(df_row_num != 0)]
  }
  .success()

  msgIndex <- .print_msg(msgIndex)

  df.ls <- lapply(df.ls, function(df) {
    val <- exhale::locate_header(df, hdr = columnNames)
    if (!is.null(val)) {
      df <- df %>%
        slice(val$nextrow:n())
      if (!identical(ncol(df), length(val$header)))
        stop("Mismatched dimensions of existing and updated headers.")
      colnames(df) <- val$header
      df
    }
    else {
      df <- data.frame(0)
    }
  })
  .success()

  msgIndex <- .print_msg(msgIndex)
  df.ls <- lapply(df.ls, fixDateEntries)
  .success()

  msgIndex <- .print_msg(msgIndex)
  df.ls <- lapply(df.ls, updateHeader, newCol = columnNames)
  .success()

  msgIndex <- .print_msg(msgIndex)
  df.ls <- lapply(df.ls, rearrange_df, columnNames)
  .success()

  msgIndex <- .print_msg(msgIndex)
  master <- combineDataFrames(df.ls)
  .success()

  msgIndex <- .print_msg(msgIndex)
  master <- setDataTypes(master)
  .success()

  msgIndex <- .print_msg(msgIndex)
  master$name <- toTitleCase(master$name)
  .success()

  msgIndex <- .print_msg(msgIndex)
  if (!dir.exists(dest))
    dir.create(dest)
  .success()

  msgIndex <- .print_msg(msgIndex)
  con <-
    dbConnect(SQLite(), file.path(dest, "NARC-mailing-list.db"))
  if (!dbIsValid(con))
    stop("Connection to database failed.")

  dbWriteTable(conn = con, dbTable, master, append = TRUE)

  ## Deal with wholesale replications and empty records
  master <- dbReadTable(con, dbTable) %>%
    distinct()
  all_empty <- apply(master, 1, function(x) all(is.na(x)))
  master <- master[!all_empty, ]
  master$serialno <- seq_along(master$serialno)
  dbWriteTable(con, dbTable, master, overwrite = TRUE)

  ## Close shop...
  dbDisconnect(con)
  if (dbIsValid(con)) {
    warning("The database connection was not properly closed.")
  } else {
    .success()
  }
  msgIndex <- .print_msg(msgIndex)
}




## Informational messages
.print_msg <- function(n = 1) {
  n <- as.integer(n)
  message(sprintf("* %s", msgs[n]), appendLF = FALSE)
  nxt <- n + 1
}






## Internal functions for output
.success <- function(){
    message(sprintf("Done\n"), appendLF = FALSE)
}






## Output Messages
msgs <-
  c(
    "\nChecking for Excel files in the directory...\n",
    "Importing details of Excel file(s) into R... ",
    "Identifying and afixing original headers... ",
    "Working on date-related columns... ",
    "Updating original headers... ",
    "Rearranging columns to suit the prescribed format... ",
    "Merging data frames... ",
    "Setting the data types... ",
    "Converting all names to title case... ",
    "Creating output directory... ",
    "Writing to database... ",
    "\nThat's all.\n"
  )




## The names of the columns of the new table
columnNames <- c(
  "serialno",
  "name",
  "phone",
  "address",
  "email",
  "bday.day",
  "bday.mth",
  "wedann.day",
  "wedann.mth",
  "occupation",
  "church",
  "pastor",
  "info.source"
)
