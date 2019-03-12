###
###
###
###   Purpose:   Functions related to managing files
###              and directories in the project
###   started:   2016/05/26 (pvr)
###
### ################################################### ###

#' @title Move a document to a separate subdirectory
#'
#' @description
#' For a given document specified by parameter psDocuName
#' create a new subdirectory and move the document and
#' everything belonging to that document to the new
#' subdirectory. If the parameter pbMoveEverything is
#' set to TRUE, then not only the document itself,
#' but the whole content of the current directory is
#' moved. The reason for that is that TeX-based documents
#' can be spread over multiple directories and can
#' include information from other files.
#'
#' @details
#' The path to the document file to be moved can be specified
#' either via the parameter psDocuPath or it can also be
#' included in the parameter psDocuName. But if the latter
#' option is chosen, then psDocuPath must be NULL.
#'
#'
#' @param psDocuName   Name of the document to be moved
#' @param psDocuPath   Path where psDocuName is stored
#' @param pbMoveEverything   flag whether complete content of current directory should be moved to new directory
#' @export move_doc_to_subdir
move_doc_to_subdir <- function(psDocuName, psDocuPath = NULL, pbMoveEverything = TRUE){
  ### # devide psDocuName in path and filename
  sDocuPath <- dirname(psDocuName)
  sDocuBaseName <- basename(psDocuName)
  ### # if psDocuPath specified, then add it
  if (!is.null(psDocuPath)){
    if (sDocuPath == "."){
      sDocuPath <- psDocuPath
    } else {
      sDocuPath <- file.path(psDocuPath, sDocuPath)
    }
  }

  ### # new subdirectory should be without extension
  sNewSubDir <- file.path(sDocuPath, tools::file_path_sans_ext(sDocuBaseName))
  ### # if new directory already exists, we stop here
  if (dir.exists(sNewSubDir))
    stop("ERROR: New directory to be created: ", sNewSubDir, " already exists. CANNOT overwrite")

  ### # get a list of files/directories to be moved
  if (pbMoveEverything) {
    vItemsToMove <- list.files(path = sDocuPath, include.dirs = TRUE)
  } else {
    vItemsToMove <- sDocuBaseName
  }

  ### # create new dirctory and move items
  dir.create(sNewSubDir)
  for (itms in vItemsToMove)
    file.rename(from = file.path(sDocuPath,itms), to = file.path(sNewSubDir, itms))

  ### # nothing to be returned
  return(invisible())
}

### ################################################### ###

#' @title Cleanup a directory from temporary output files
#'
#' @description
#' Remove all files and possibly also directories that
#' were generated as ouput from a previous compilation
#' of a document in directory ps_docu_path. All files that
#' match the pattern specified by pvec_pattern are selected
#' for deletion. The user is asked whether the files should
#' be deleted before actually deleting them. By default,
#' directories are ignored, even if they match the given
#' pattern. In case directories are to be included the
#' argument pb_ignore_dir must be set to FALSE.
#'
#' @param ps_docu_path     Name of the document path
#' @param pvec_docu_root   Vector of prefixes matching document output files
#' @param pvec_pattern     Pattern to match by files to be deleted
#' @param pb_ignore_dir      Flag indicating whether directories should be ignored
#' @param pb_interactive    Should user be asked whether to delete files
#' @export cleanup_output
cleanup_output <- function(ps_docu_path    = "vignettes",
                           pvec_docu_root  = NULL,
                           pvec_pattern    = c("pdf$", "png$"),
                           pb_ignore_dir   = TRUE,
                           pb_interactive  = TRUE){
  ### # loop over vector of document roots
  if (is.null(pvec_docu_root)){
    ### # find matching file names based on pvec_pattern and delete them
    find_match_delete( ps_docu_path   = ps_docu_path,
                       ps_pattern     = pvec_pattern,
                       pb_ignore_dir  = pb_ignore_dir,
                       pb_interactive = pb_interactive )

  } else {
    ### # find matching file names based on pvec_docu_root and delete them
    find_match_delete( ps_docu_path   = ps_docu_path,
                       ps_pattern     = pvec_docu_root,
                       pb_ignore_dir  = pb_ignore_dir,
                       pb_interactive = pb_interactive )
  }
  ### # invisible return
  invisible(TRUE)
}


#' @title Helper to find all files match a certain pattern
#'
#' @description
#' A given pattern is used in function \code{list.files()}
#' to obtain a list of files matching the given pattern.
#' The list of files is then used as input for the function
#' \code{delete.files()}.
#'
#' @param ps_docu_path directory containing files to be matched
#' @param ps_pattern   pattern used for matching files
#' @param pb_ignore_dir flag indicating whether directories are ignored
#' @param pb_interactive flag for interactive mode
find_match_delete <- function( ps_docu_path,
                               ps_pattern,
                               pb_ignore_dir  = TRUE,
                               pb_interactive = TRUE ) {
  for (p in ps_pattern) {
    ### # list the files and directories that match the current pattern p
    vec_cur_match <- list.files(path = ps_docu_path, pattern = p, full.names = TRUE)
    if (pb_ignore_dir)
      vec_cur_match <- vec_cur_match[!file.info(vec_cur_match)[,"isdir"]]
    ### # ask whether to delete files
    if (length(vec_cur_match) > 0){
      delete_files(ps_docu_path = ps_docu_path,
                   pvec_filenames = vec_cur_match,
                   pb_interactive = pb_interactive)

    }

  }
  return(invisible(TRUE))
}


#' @title Helper function to delete a set of files
#'
#' @description
#' If \code{pb_interactive} is true, the function loops
#' over a given vector of file names and asks to user
#' whether the files should be deleted. Otherwise the
#' files in the vector are deleted without asking.
#'
#' @param pvec_filenames vector of filenames to be deleted
#' @param pb_interactive flag for interactive mode
delete_files <- function(ps_docu_path,
                         pvec_filenames,
                         pb_interactive = TRUE){
  message("Files to be removed from directory: ", ps_docu_path,
          "\n", paste(basename(pvec_filenames), collapse = ", "))
  ### # ask user in interactive mode
  if (pb_interactive){
    for (f in pvec_filenames){
      sAnswer <- readline(prompt = paste0("Remove file: ", f, " [y/N]: ", collapse = ""))
      if (tolower(sAnswer) == "y")
        file.remove(f)

    }

  } else {
    file.remove(pvec_filenames)
  }
  return(invisible(TRUE))
}


#' Recursively cleanup all directories of ps_docu_path
#'
#' This is a wrapper over cleanup_output using sapply
#' over the result of list.files(ps_docu_path)
#'
#' @param ps_docu_path   Name of the document path
#' @param pvec_pattern    Pattern to match by files to be deleted
#' @export cleanup_all
cleanup_all <- function(ps_docu_path  = "vignettes",
                        pvec_pattern   = c("pdf$", "png$"),
                        pb_ignore_dir = TRUE){
  sapply(list.files(path = ps_docu_path, full.names = TRUE),
         function(x) cleanup_output(ps_docu_path   = x,
                                    pvec_docu_root = NA,
                                    pvec_pattern   = pvec_pattern,
                                    pb_ignore_dir  = pb_ignore_dir,
                                    pb_interactive = TRUE))
  invisible(TRUE)
}
