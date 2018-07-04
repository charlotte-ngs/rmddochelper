###
###
###
###   Purpose:   Helpers for document management
###   started:   2017/09/20 (pvr)
###
### ############################################# ###

## --- Bookdown related helper functions -------------------------
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


## --- Document meta-information --------------------------------------
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


#' @title Get the name and the path to the current rmd-file
#'
#' @description
#' Analogously, to \code{rprojroot::this_file()}, we want to determine
#' the name and the path to the currently active rmd-source document.
#' The solution with \code{rprojroot::this_file()} works only, when the
#' document is knitted, but not when run from inside an R-chunk.
#' Given the name and the path a a rmd-source document project
#' follow a certain pattern, the full path and the name of an
#' rmd-source document can be determined based on the result
#' of the function \code{getwd()}.
#'
#' @param ps_wd full path of current working directory
#' @return name and full path to current rmd-source file.
#' @export get_this_rmd
get_this_rmd <- function(ps_wd = getwd()){
  # subdirectory for the format
  s_format <- basename(ps_wd)
  # full path of document
  s_fp_doc <- dirname(ps_wd)
  # document name
  s_doc_name <- basename(s_fp_doc)
  # content of ps_wd
  vec_content <- list.files(path = ps_wd)
  # in case we have more than one file in vec_content,
  #  determine which is the one to be returned
  s_res_idx <- which(tolower(paste(s_doc_name, s_format, sep=".")) == (tolower(vec_content)))
  # return the result
  return(file.path(ps_wd, vec_content[s_res_idx]))
}
