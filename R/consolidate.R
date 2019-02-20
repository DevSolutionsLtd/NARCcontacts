# consolidate.R

globalVariables(c('name', 'serialno', 'phone', 'email', '.'))

#' Consolidation of Database entries (with focus on duplications)
#'
#' @param db The database file (SQLite).
#' @param table The database table to be consolidated; a character vector of
#' length 1.
#'
#' @details The default argument for the \code{table} parameter is
#' \emph{NARC_mail}. This is kept for backward compatibility with earlier
#' code that was specifically tailored for use by this function.
#'
#' @return The functions returns a copy of the consolidated data frame. A side
#' effect is the creation of a table through internal implementation. That
#' table is named \emph{consolidated}.
#'
#' @importFrom utils menu
#' @export
consolidate_narc_mail <- function(db, table = "NARC_mail")
{
  stopifnot({
    inherits(db, "character")
    inherits(table, "character")
  })
  if (!file.exists(db))
    stop("File ", sQuote(db), " does not exist.") # TODO: Write tests to capture validation
  if (length(db) > 1) {
    db <- db[1]
    warning("'db' must be of length 1L; elements beyond the first were removed")
  }
  cat("* Overview of the data:\n")
  dat <- importDataFromDb(db, table) %>%
    as_tibble() %>%
    print()

  checkIdVarDuplicates(dat)

  df_pre <- aggregateDuplicatedVals(dat)
  skip <-
    menu(c('Yes', 'No'),
         title = "NEXT: Fill in missing values. Continue?") %>%
    {
      isTRUE(. == 2)
    }
  if (!skip)
    pause()
  df_post <- fillMissingVals(df_pre, skip = skip)
  checkDataIntegrity(df_pre, df_post, skip = skip)
  storeConsolidatedData(df_post, db)
  df_post
}



checkIdVarDuplicates <- function(dframe)
{
  cat("NEXT: Check identifier variables for duplications\n")
  pause()
  invisible(dframe %>%
              select(name, phone, email) %>%
              {
                cat("* Number of duplications:\n")
                sapply(colnames(.), function(x) {
                  cat("** Column",
                      sQuote(x),
                      "has",
                      sum(duplicated(.[[x]]), na.rm = TRUE),
                      "duplications\n")
                })
              })
}


aggregateDuplicatedVals <- function(x)
{
  cat("NEXT: Aggregate the duplicated values\n")
  pause()
  cat("* Sort the data frame by 'name':\n")
  arr <- x %>%
    as_tibble() %>%
    distinct() %>%
    arrange(name)
  print(arr)
  arr
}







fillMissingVals <- function(d, skip = FALSE)
{
  if (skip)
    return(d)

  ## Find repeated names and associated records
  cons <- .fixMultipleValues(d)
  if(exists("cons")) {
    cat("* Merge completed.\n\n* View the data:\n")
    print(cons)
  }
  cons
}












#' @import RSQLite
#' @importFrom utils menu
storeConsolidatedData <- function(df, db, table = "consolidated")
{
  cat("NEXT: Save consolidated data to disk\n")
  if (interactive())
    pause()
  cat(sprintf("* Store data in table '%s'... ", table))
  dbcon <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(dbcon))
  if (table %in% dbListTables(dbcon)) {
    if (interactive()) {
      write <-
        menu(choices = c("Yes", "No"),
             title = "\nYou are about to overwrite an existing table. Continue?")
      if (identical(write, 1L))
        message(sprintf("You opted to overwrite '%s'", table))
      if (identical(write, 2L)) {
        message("The data were not stored on disk.")
        invisible(return(NULL))
      }
    }
    if (is.null(write))
      warning(sprintf("Automatically overwriting existing table '%s'.", table))
  }
  dbWriteTable(dbcon, table, df, overwrite = TRUE)
  cat("Done\n")
}








## Imports a table from a database
## @param db An SQLite database
## @param table The table to be imported
## @return Returns an object of class \code{data.frame}
importDataFromDb <- function(db, table) {
  stopifnot(is.character(db), is.character(table))
  stopifnot(file.exists(db))
  bin <- readBin(db, what = 'raw', n = 16L)
  if (!identical(rawToChar(bin), "SQLite format 3"))
    stop("'db' is not a supported file format")
  dbcon <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(dbcon))
  if (!dbIsValid(dbcon))
    stop("There was a problem making the database connection")

  if (table %in% dbListTables(dbcon))
    df <- dbReadTable(dbcon, table)
  else {
    message("No table called ", sQuote(table), "in ", sQuote(basename(db)))
    return(NULL)
  }
}






## Utility function to enhance interaction with user
pause <- function() {
  if (interactive()) {
    readline("Press ENTER to continue...")
  }
}








## Allows user to interactively fix multiple entries (by variable)
#' @importFrom utils menu
.fixMultipleValues <- function(dataframe) {
  uniq <- unique(dataframe$name)
  lapply(uniq, function(N) {
    ## Extract a data frame of a given name
    one_name <- filter(dataframe, name == N)
    if (nrow(one_name) > 1) {
      cat(sprintf("* Merging available records for '%s'\n", N))
      one_name <- colnames(one_name) %>%
        sapply(
          simplify = FALSE,
          FUN = function(var) {
            val <- unique(one_name[[var]])
            ## Don't present NAs as options
            if(!all(is.na(val))) {
              val <- val[!is.na(val)]
            }
            ## where there is more than one distinct
            ## value, present the user with options
            if (length(val) > 1) {
              pick <-
                menu(
                  choices = val,
                  title = sprintf("** Pick a value from the column '%s':", var)
                )
              val[pick]
            }
            else
              val
          }
        )
    }
    else {
      one_name
    }
  }) %>%
    lapply(as_tibble) %>%
    bind_rows()
}
