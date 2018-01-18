###
###
###
###   Purpose:   Helpers for document management
###   started:   2017/09/20 (pvr)
###
### ############################################# ###

#' Clean up all files after a failed bookdown build
#'
#' Whenever an error occurs during a build of a bookdown document
#' the cleanup procedure of RStudio does not delete all
#' output files. Files starting with _main
#' seam to survive the ordinary clean up process. This helper
#' function removes those files.
#'
#' @param pspath directory where the bookdown files are built
#' @param pspattern pattern matching the files to be deleted
#'
#' @export bookdown_cleanup
bookdown_cleanup <- function(pspath = ".", pspattern = "_main") {
  suppressWarnings( rmarkdown::clean_site(input = pspath) )
  bfrmresult <- file.remove(list.files(path = pspath,
                                       pattern = pspattern,
                                       full.names = TRUE))
  return(invisible(all(bfrmresult)))
}
