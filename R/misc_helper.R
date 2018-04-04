###
###
###
###   Purpose:   Collection of misc helper functions
###   started:   2018-03-22 (pvr)
###
### ################################################### ###
#
#
#' Get name of OS we are running on
#'
#' @description
#' This function returns a simple string depending whether
#' the current instance of R runs on windows, mac osx or
#' linux.
#'
#' @details
#' The function was proposed on the blog post at
#' \url{http://conjugateprior.org/2015/06/identifying-the-os-from-r/}
#' found on R-bloggers.
#'
#' The case of windows was not included in the original
#' version from the blog post. It was included here
#' at the beginning to filter them out at the beginning
#' based on the value of \code{.Platform$OS.type}
#'
#' @return name of operating system which is either
#'   "windows", "osx" or "linux".
#' @export get_os
get_os <- function(){
  ### # added special case of windows which is not
  ### #  in origina version
  if (.Platform$OS.type == "windows")
    return("windows")

  ### # here we are just separating mac osx and linux
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}
