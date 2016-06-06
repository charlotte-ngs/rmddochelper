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

#' @title Cleanup a directory from old output
#'
#' @description
#' Remove all files and directories that were generated
#' as ouput from a previous compilation of a document
#' given by psDocuName and match the pattern specified by
#' psPattern. By default, directories are ignored,
#' even if they match the given pattern.
#'
#' @param psDocuName   Name stem of the directory source
#' @param psDocuPath   Name of the document path
#' @param psPattern    Pattern to match by files to be deleted
#' @export cleanup_output
cleanup_output <- function(psDocuName,
                           psDocuPath  = file.path("vignettes", psDocuName),
                           psPattern   = "pdf$",
                           pbIgnoreDir = TRUE){
  ### # loop over vector of patterns given in psPattern
  for (p in psPattern) {
    ### # list the files and directories that match the current pattern p
    curMatch <- list.files(path = psDocuPath, pattern = p, full.names = TRUE)
    if (pbIgnoreDir)
      curMatch <- curMatch[!file.info(curMatch)[,"isdir"]]
    if (!exists("files_to_remove")){
      files_to_remove <- curMatch
    } else {
      files_to_remove <- c(files_to_remove, curMatch)
    }
  }
  message("Files to be removed: ", paste(basename(files_to_remove), collapse = ", "))
  sAnswer <- readline(prompt = "Should above files be removed [y/N]: ")
  if (tolower(sAnswer) == "y")
    file.remove(files_to_remove)

  invisible(TRUE)


}
