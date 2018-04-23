## collect.R

#' Collect all Excel files
#'
#' Collects all the Excel files within a given parent directory and its
#' children.
#'
#' @param dir Root directory to commence search for files.
#' @param dest Directory where collated files are stored.
#'
#' @export
collate_all_excel <- function(dir, dest = NULL)
{
  if (missing(dir)) dir <- "~/"
  dir <- normalizePath(dir)

  if (is.null(dest)) {
    dest <- suppressWarnings(normalizePath(file.path(getwd(), "collate")))
    if (!dir.exists(dest))
      dir.create(dest)
  }
  else if (!is.null(dest) & !dir.exists(dest)) {
    stop(paste("Directory", sQuote(dest), "does not exist"))
  }

  paths <- list.files(
    dir,
    pattern = ".xls$|.xlsx$",
    recursive = TRUE,
    all.files = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )
  sapply(paths, function(x) {
    try(file.copy(from = x, to = file.path(dest, basename(x))))
  })
  lp <- length(paths)
  ld <- length(list.files(dest, all.files = TRUE))
  if (!identical(lp, ld))
    warning(sprintf("Only %d out of %d files found were collected", ld, lp))
}
