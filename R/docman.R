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


#' Write data-time and user that has done latest changes to a document
#'
#' The information about who has done the latest changes to a document
#' is usually added at the end of a website. This allows the reader
#' of a website to check whether the site was recently updated. For
#' the author, it is an easy way to check whether the most recent
#' version has been uploaded to the server.
#'
#' The information that is returned by this function consists of the
#' time returned by \code{Sys.time()} and the username that is returned
#' by \code{Sys.info()}. The intended way to use this function is to
#' \code{cat()} the result of the function in the last code-junk of
#' a document. See examples for more details.
#'
#' @examples
#' \dontrun{
#' ```{r}
#' cat(rmddochelper::get_latest_change(), "\n")
#' ```
#' }
#'
#' @param ps_msg special message to be used in front of time and user info, defaults to Latest Changes
#' @return st_result containing time and user-part of system info
#' @export get_latest_change
get_latest_change <- function(ps_msg = "Latest Changes"){
  st_result <- paste0("\n---\n\n"," _", ps_msg, ": ",
                      Sys.time(), " (", Sys.info()[["user"]], ")_", collapse = "")
  return(st_result)
}
