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
#' of a document in directory psDocuPath. All files that
#' match the pattern specified by psPattern are selected
#' for deletion. The user is asked whether the files should
#' be deleted before actually deleting them. By default,
#' directories are ignored, even if they match the given
#' pattern. In case directories are to be included the
#' argument pbIgnoreDir must be set to FALSE.
#'
#' @param psDocuPath     Name of the document path
#' @param psPattern      Pattern to match by files to be deleted
#' @param pbIgnoreDir    Flag indicating whether directories should be ignored
#' @param pbInteractive  Should user be asked whether to delete files
#' @export cleanup_output
cleanup_output <- function(psDocuPath    = "vignettes",
                           psPattern     = c("pdf$", "png$"),
                           pbIgnoreDir   = TRUE,
                           pbInteractive = TRUE){
  ### # loop over vector of patterns given in psPattern
  for (p in psPattern) {
    ### # list the files and directories that match the current pattern p
    curMatch <- list.files(path = psDocuPath, pattern = p, full.names = TRUE)
    if (pbIgnoreDir)
      curMatch <- curMatch[!file.info(curMatch)[,"isdir"]]
    ### # ask whether to delete files
    if (length(curMatch) > 0){
      message("Files to be removed from directory: ", psDocuPath,
              "\n", paste(basename(curMatch), collapse = ", "))
      sAnswer <- readline(prompt = "Should above files be removed [y/N]: ")
      if (tolower(sAnswer) == "y" | !pbInteractive)
        file.remove(curMatch)
    }

  }
  invisible(TRUE)
}


#' Recursively cleanup all directories of psDocuPath
#'
#' This is a wrapper over cleanup_output using sapply
#' over the result of list.files(psDocuPath)
#'
#' @param psDocuPath   Name of the document path
#' @param psPattern    Pattern to match by files to be deleted
#' @export cleanup_all
cleanup_all <- function(psDocuPath  = "vignettes",
                        psPattern   = c("pdf$", "png$"),
                        pbIgnoreDir = TRUE){
  sapply(list.files(path = psDocuPath, full.names = TRUE),
         function(x) cleanup_output(psDocuPath = x,
                                    psPattern = psPattern,
                                    pbIgnoreDir = pbIgnoreDir,
                                    pbInteractive = TRUE))
  invisible(TRUE)
}
