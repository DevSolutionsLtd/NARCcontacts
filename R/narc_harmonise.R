#' Harmonise NARC Data From Different Excel Spreadsheets.
#'
#' \code{narc_harmonise}
#'
#' @param path Path to the folder (hopefully) containing Excel files
#'
#' @import dplyr
#' @import RSQLite
#' @importFrom readxl read_excel
#'
#' @export
narc_harmonise <- function(path = NULL)
{
  notice()

  cat("Checking for Excel files in the directory..,\n")
  if (is.null(path))
    path <- "."
  excelfiles <- list.files(path = path, pattern = ".xlsx$|.xls$")
  numFiles <- length(excelfiles)
  if (!numFiles) {
    cat("Not Found")
    stop("There are no Excel files in this directory")
  } else {
    cat(sprintf(
      ngettext(
        numFiles,
        "\t%d Excel file was found:\n",
        "\t%d Excel files were found:\n"
      ),
      numFiles
    ))

    ## List the files
    invisible(sapply(excelfiles, function(x) {
      cat(sprintf("\t  * %s\n", x))
    }))
  }

  cat("Importing the data from Excel into R... ")
  excelfiles <- sapply(excelfiles, function(x) file.path(path, x))
  df.ls <- sapply(excelfiles, read_excel)
  cat("Done\n")

  cat("Creating a header for the new data frame... ")
  columnNames <- c(
    "serialno",
    "name",
    "phone",
    "address",
    "email",
    "birthday",
    "anniversary",
    "occupation",
    "church",
    "pastor",
    "info.source"
  )
  cat("Done\n")

  cat("Identifying and afixing original headers... ")
  df.ls <- lapply(df.ls, function(df) {
    val <- locate_header(df, hdr = columnNames)
    df <- df %>%
      slice(val$nextrow:n())

    if (!identical(ncol(df), length(val$header)))
      stop("Mismatched dimensions of existing and updated headers.")
    colnames(df) <- val$header
    df
  })
  cat("Done\n")

  cat("Updating original headers... ")
  df.ls <- lapply(df.ls, update_header, newCol = columnNames)
  cat("Done\n")

  cat("Rearranging columns to suit the prescribed format... ")
  df.ls <- lapply(df.ls, rearrange_df, columnNames)
  cat("Done\n")

  cat("Merging data frames... ")
  master <- combine_dfs(df.ls)
  cat("Done\n")

  cat("Setting types... ")
  master <- set_datatypes(master)
  cat("Done\n")

  cat("Creating output directory... ")
  folder <- "harmonised-data"
  if (!dir.exists(file.path(path, folder)))
    dir.create(file.path(path, folder))
  cat("Done\n")

  cat("Writing to database... ")
  dbFile <- "NARC-mailing-list.db"
  con <- dbConnect(SQLite(), file.path(path, folder, dbFile))
  dbWriteTable(conn = con, "NARC_mail", master)
  dbDisconnect(con)
  cat("Done\n")

  cat("\nThat's all.\n")
}
