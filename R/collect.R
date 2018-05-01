## collect.R

#' Collect all Excel files
#'
#' Collects all the Excel files within a given parent directory and its
#' children.
#'
#' @param dir Root directory to commence search for files.
#' @param dest Directory where collated files are stored.
#' @param verbose Logical. Whether to display a printout of copy operations.
#'
#' @export
collate_excel <- function(dir, dest = NULL, verbose = TRUE)
{
  slsh <- "/"
  if (missing(dir))
    dir <- "~/"
  dir <- normalizePath(dir, winslash = slsh)

  ## Make sure we have a directory to keep the files.
  if (is.null(dest)) {
    dest <-
      suppressWarnings(
        normalizePath(
          file.path(getwd(), "collated"), winslash = slsh))
    if (!dir.exists(dest))
      dir.create(dest)
  }
  else if (!is.null(dest) & !dir.exists(dest)) {
    stop(paste("Directory", sQuote(dest), "does not exist"))
  }

  if (verbose)
    cat("Root directory for this search is", sQuote(dir), ":\n\n")
  cat("R is building the directory list. Please wait...\n")
  dirLst <- normalizePath(list.dirs(dir), winslash = slsh, mustWork = TRUE)

  ## We're getting the absolute path of Excel files that exist in the
  ## directories found, and copying them to the final destination.
  ## Note that '.lf()' is defined outside this scope.
  paths <- lapply(dirLst, function(d) {
    files <- .getThisDirExcelFiles(d, slsh)
    if (length(files)) {
      if (verbose) {
        cat("Excel file(s) in", sQuote(basename(d)), ":\n* ")
        cat(paste(basename(files), collapse = "\n* "), "\n")
        cat("Copying files...\n")
      }
      success <- try(file.copy(from = files, to = dest))
    }
  })
  paths <- unlist(paths)
  sp <- sum(paths)
  lp <- length(paths)
  if (verbose & (sp > 0)) {
    cat("Files were successfully copied into",
        sQuote(basename(dest)),
        "\n")
    if (!identical(lp, sp))
      warning(sprintf("Only %d out of %d files found were collected", sp, lp))
  }
}






## Customises base::list.files()
.getThisDirExcelFiles <- function(d, s) {
  stopifnot(identical(length(d), 1L))
  normalizePath(
    list.files(
      path = d,
      pattern = ".xls$|.xlsx$",
      recursive = FALSE,
      all.files = TRUE,
      ignore.case = TRUE,
      full.names = TRUE
    ),
    winslash = s
  )
}
