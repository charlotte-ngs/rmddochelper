###
###
###
###   Purpose:   Creation and Conversion of ODG graphics
###   started:   2016/06/27 (pvr)
###
### ###################################################### ###

#' @title Convert documents from source format into a given target output format
#'
#' @description
#' \code{convertLibOToGraphic} assumes that LibreOffice is installed
#' and available on the search path. Source files are converted on the
#' fly to the specified output format which are then included in the
#' source R markdown document
#'
#' @param psLibOFile    name of the libre office graphics file
#' @param psLibODir     source directory of Libre Office files
#' @param psFigOutDir   output directory where figure pdfs are expected to be
convertLibOToGraphic <- function(psLibOFile,
                                 psOutFormat = NULL,
                                 psLibODir = "odg",
                                 psFigOutDir = "."){
  ### # output format cannot be null
  if (is.null(psOutFormat))
    stop(" *** Missing output format: ", psOutFormat)
  sOutFormat <- tolower(psOutFormat)
  sOdgDir <- psLibODir
  sOdgDirWin <- gsub("/", "\\", sOdgDir, fixed = TRUE)
  sConvCmdStem <- ifelse(.Platform$OS.type == "windows",
                         paste('"C:/Program Files (x86)/LibreOffice 5/program/soffice" --headless --convert-to',
                               sOutFormat),
                         paste("soffice --headless --convert-to", sOutFormat))
  sFigFile <- ifelse(.Platform$OS.type == "windows",
                     paste(sOdgDirWin, psLibOFile, sep = "\\"),
                     file.path(sOdgDir, psLibOFile))
  if (!file.exists(sFigFile))
    stop("Cannot find Graphics File: ", sFigFile,
         ". Please run create_odg_graphic(psGraphicName = ",
         file.path(sOdgDir, psLibOFile), ") first.")
    #create_odg_graphic(psGraphicName = file.path(sOdgDir, psLibOFile))
  sConvCommand <- paste(sConvCmdStem, sFigFile)
  system(command = sConvCommand)
  sOutFile <- gsub("odg$", sOutFormat, psLibOFile)
  sFigOutFile <- file.path(psFigOutDir, sOutFile)
  file.rename(from = sOutFile, sFigOutFile)
  return(sFigOutFile)
}


#' @title Create an empty odg graphic
#'
#' @description
#' \code{create_odg_graphic} uses templates which are either provided
#' by the caller or which are stored in the rmarkdown/template directory
#' of this package `rmddochelper`. When calling  \code{create_odg_graphic}
#' without any arguments, then the template is opened using soffice draw.
#' When a filename for the odg graphic which should be created, is
#' specified the template is renamed to the given name.
#'
#' @param psGraphicName   Format of diagram to be created
#' @param psGraphicPath   Path where created odg file should be stored
#' @param psOdgTemplate   name of the template to be used
#' @param psTemplatePkg   package where template is stored
#' @param create_dir      should created odg file be stored in separate directory
#' @param pbRecursive     recursively create complete path to graphic file
#' @param pbEdit          directly edit created odg file
#' @export create_odg_graphic
create_odg_graphic <- function(psGraphicName  = "skeleton.odg",
                               psGraphicPath  = "vignettes",
                               psRmdSrcFile,
                               psGrFmt        = "pdf",
                               psOdgTemplate  = "odg_figure",
                               psTemplatePkg  = "rmddochelper",
                               create_dir     = "default",
                               pbRecursive    = TRUE,
                               pbEdit         = TRUE){

  ### # check whether graphcis path exist, o/w create it
  if (!dir.exists(psGraphicPath))
    dir.create(path = psGraphicPath, recursive = pbRecursive)
  sFile <- file.path(psGraphicPath,psGraphicName)
  ### # use the local function rmd_draft to copy the template
  sGraphicTrgName <- odg_draft(file        = sFile,
                               template    = psOdgTemplate,
                               package     = psTemplatePkg,
                               create_dir  = create_dir)
  ### # if the graphics target name does not exist, there is a problem
  if (!file.exists(sGraphicTrgName))
    stop(" *** ERROR: could not create graphics file: ", sGraphicTrgName)

  ### # insert command to include graphics file into source file
  conRmdSrc <- file(description = psRmdSrcFile)
  vRmdSrc <- readLines(con = conRmdSrc)
  close(con = conRmdSrc)
  vRmdSrc <- include_graphics_cmd(psGraphicName = psGraphicName,
                                  pvRmdSrc = vRmdSrc,
                                  psGrFmt = psGrFmt)
  cat(vRmdSrc, "\n", file = psRmdSrcFile, sep = "\n")


  ### # depending on flag, open graphics file
  if (pbEdit){
    ### # depending on platform start open the template file differently
    if (.Platform$OS.type == "windows"){
      file.show(sGraphicTrgName)
    } else {
      sSofficeCmd <- paste("soffice --draw", sGraphicTrgName)
      system(sSofficeCmd)
    }

  }

  cat(" * Odg Graphics created in: ", sGraphicTrgName, "\n")

  ### # return graphics name
  return(sGraphicTrgName)
}


#' @title Add statement to include graphic into Rmarkdown source
#'
#' @description
#' \code{include_graphics_cmd} assumes that in the Rmarkdown source
#' file a chunk is inserted which will contain the statement that
#' includes the graphic file. The chunk needs to be labelled with
#' the name of the graphic file to be included. This function searches
#' for this chunk and inserts the statement. If the chunk is not
#' found, the graphics statement is included above the end of
#' document marker.
#'
#' @param psGraphicName   name of the graphic file to be included
#' @param pvRmdSrc        name of the Rmarkdown source file
#' @param psGrFmt         graphic format
#' @return vRmdSrc        vector with extended Rmarkdown sources
#'
include_graphics_cmd <- function(psGraphicName, pvRmdSrc, psGrFmt) {
  ### # insertion command depends on psGrFmd
  if (psGrFmt == "png"){
    sGrInsCmd <- 'rmddochelper::insertOdgAsPng(psOdgFileStem = "'
  } else {
    sGrInsCmd <- 'rmddochelper::insertOdgAsPdf(psOdgFileStem = "'
  }
  vRmdSrc <- pvRmdSrc
  ### # the search pattern is define by what RStudio inserts when
  ### #  inserting a new code-chunk
  sSearchPattern <- paste0("```{r ", psGraphicName, "}")
  nGrInclLineIdx <- grep(pattern = sSearchPattern, vRmdSrc, fixed = TRUE)
  ### # in case the graphics inclusion statement was found add the command here
  if (length(nGrInclLineIdx) > 0){
    ### # depending on format
    vRmdSrc[nGrInclLineIdx+1] <- paste0(sGrInsCmd, psGraphicName, '")')
  } else {
    nGrInclLineIdx <- grep(pattern = "<!-- END of document", vRmdSrc, fixed = TRUE)
    vRmdSrc <- c(vRmdSrc[1:(nGrInclLineIdx-1)],
                 sSearchPattern,
                 paste0(sGrInsCmd, psGraphicName, '")'),
                 "```",
                 vRmdSrc[nGrInclLineIdx:length(vRmdSrc)])
  }
  return(vRmdSrc)
}


#' @title Copy a draft template file for a odg graphics
#'
#' @description
#' This function \code{odg_draft} works analogously to
#' \code{rmarkdown::draft}, but for ODG graphics files.
#' The template can either be specified with an explicit
#' path or in connection with the parameter package where
#' the latter assumes that package is installed and contains
#' a directory \code{rmarkdown/templates/<template_name>}.
#' In both options the template-directory must contain a file
#' called template.yaml with meta information about the template
#' and a subdirectory skeleton with all the files that
#' are to be copied to the directory where the target file
#' is expected to be. When the parameter \code{create_dir} is
#' specified either as function parameter or as meta-information
#' a separate directory for the target file is created. In case
#' it is needed, the appropriate file extension is pasted to the
#' name of the target file. In this specific case the extension
#' we are using here is .odg. The list of files in the
#' skeleton subdirectory of the template directory is copied
#' to the path where the target file is supposed to be.
#' The copying of the files can be specified with an
#' option that indicates whether existing files should be
#' overwritten. The last step consists of renaming the
#' skeleton-file to the basename given in the file
#' parameter.
#'
#' @param   file          name of the and path to the new document
#' @param   template      name of the template
#' @param   package       package where template can be found
#' @param   create_dir    whether or not to create a new directory for this document
#' @param   pbOverwrite   should existing files be overwritten
#' @return  file          name of the and path to the new document
odg_draft <- function(file,
                      template    = "odg_figure",
                      package     = NULL,
                      create_dir  = "default",
                      pbOverwrite = FALSE){
  ### # determine the template path which is contained
  ### #  in package "package"
  if (!is.null(package)) {
    template_path = system.file("rmarkdown", "templates",
                                template, package = package)
    if (!nzchar(template_path)) {
      stop("The template '", template, "' was not found in the ",
           package, " package")
    }
  } else {
    template_path <- template
  }
  ### # read info in template.yaml
  template_yaml <- file.path(template_path, "template.yaml")
  if (!file.exists(template_yaml)) {
    stop("No template.yaml file found for template '",
         template,"'")
  }
  ### # read yaml info from file into variable
  template_meta <- rmarkdown:::yaml_load_file_utf8(template_yaml)
  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain name and description fields")
  }
  ### # check whether function parameter or meta info specify whether a
  ### #  separate new directory for file must be created
  if (identical(create_dir, "default"))
    create_dir <- isTRUE(template_meta$create_dir)
  if (create_dir) {
    file <- tools::file_path_sans_ext(file)
    if (dir.exists(file))
      stop("The directory '", file, "' already exists.")
    dir.create(file, recursive = TRUE)
    file <- file.path(file, basename(file))
  }
  ### # error, in case file itself already exists
  if (!identical(tolower(tools::file_ext(file)), "odg"))
    file <- paste(file, ".odg", sep = "")
  if (file.exists(file))
    stop("The file '", file, "' already exists.")
  ### # generate a list of skeleton files
  skeleton_files <- list.files(file.path(template_path, "skeleton"),
                               full.names = TRUE)
  to <- dirname(file)
  for (f in skeleton_files) {
    file.copy(from = f, to = to, overwrite = pbOverwrite, recursive = TRUE)
  }
  ### # rename skeleton to final name
  file.rename(file.path(dirname(file), "skeleton.odg"), file)

  ### # return result file to caller
  return(file)

}


## ---- Insert a Odg draw graphic -------------------------------------------
#' Inserts an odg graphic in pdf format into a rmarkdown text
#'
#' @description
#' This function is a wrapper to the more generic function
#' \code{includeOdgGraphic} which allows to include graphics
#' into rmarkdown source files.
#'
#' @param  psOdgFileStem    stem of odg figure file
#' @param  psOdgDir         directory where odg figure file is stored
#' @param  pbMustGenerate   flag to indicate whether pdf-graphics must be regenerated
#' @param  psFigOutDir      directory where output should be placed
#' @export insertOdgAsPdf
insertOdgAsPdf <- function(psOdgFileStem,
                           psOdgDir = "odg",
                           psFigOutDir = ".",
                           pbMustGenerate = FALSE,
                           pnPaperWidthScale = NULL) {
  includeOdgGraphic(psOdgFileStem     = psOdgFileStem,
                    psOutFormat       = "pdf",
                    psOdgDir          = psOdgDir,
                    psFigOutDir       = psFigOutDir,
                    pbMustGenerate    = pbMustGenerate,
                    pnPaperWidthScale = pnPaperWidthScale)
}


#' @title Include an odg graphic in png format into a rmarkdown source file
#'
#' @param  psOdgFileStem    stem of odg figure file
#' @param  psOdgDir         directory where odg figure file is stored
#' @param  pbMustGenerate   flag to indicate whether pdf-graphics must be regenerated
#' @param  psFigOutDir      directory where output should be placed
#' @export insertOdgAsPng
insertOdgAsPng <- function(psOdgFileStem,
                           psOdgDir = "odg",
                           psFigOutDir = ".",
                           pbMustGenerate = FALSE,
                           pnPaperWidthScale = NULL) {
  includeOdgGraphic(psOdgFileStem     = psOdgFileStem,
                    psOutFormat       = "png",
                    psOdgDir          = psOdgDir,
                    psFigOutDir       = psFigOutDir,
                    pbMustGenerate    = pbMustGenerate,
                    pnPaperWidthScale = pnPaperWidthScale)
}

#' @title Include a graphic file based on an odg template in a given output format
#'
#' \code{includeOdgGraphic} takes the name of a file containing a graphic
#' in odg format, converts the content of that file into a given output format using
#' function \code{convertLibOToGraphic} and outputs the string in markdown
#' format to include the figure. Graphic files are only re-generated
#' if the outputfile does not exist or, if the flag pbMustGenerate is TRUE.
#'
#' @param  psOdgFileStem    stem of odg figure file
#' @param  psOutFormat      output format of the graphic file to be included
#' @param  psOdgDir         directory where odg figure file is stored
#' @param  pbMustGenerate   flag to indicate whether pdf-graphics must be regenerated
#' @param  psFigOutDir      directory where output should be placed
includeOdgGraphic <- function(psOdgFileStem,
                              psOutFormat,
                              psOdgDir          = "odg",
                              psFigOutDir       = ".",
                              pbMustGenerate    = FALSE,
                              pnPaperWidthScale = NULL){
  ### # check wether pdf file already exists, if so, do nothing
  sGraphicFilename <- paste(psOdgFileStem, tolower(psOutFormat), sep = ".")
  sGraphicFile <- file.path(psFigOutDir,sGraphicFilename)
  if (!file.exists(sGraphicFile) | pbMustGenerate){
    ### # if pdf files cannot be found, regenerate them, check that psOdgFileName exists
    sOdgFilename <- paste(psOdgFileStem, "odg", sep = ".")
    sOdgFile <- file.path(psOdgDir, sOdgFilename)
    # if (!file.exists(sOdgFile))
    #   stop("Cannot find Odg figure file: ", sOdgFile)
    ### # convert odg file to pdf
    sConvGraphicFileName <- convertLibOToGraphic(psLibOFile = sOdgFilename,
                                                 psOutFormat = psOutFormat,
                                                 psLibODir = psOdgDir,
                                                 psFigOutDir = psFigOutDir)
    if (!file.exists(sConvGraphicFileName))
      stop("Cannot find created graphics file: ", sConvGraphicFileName)
  }
  ### # at this point the pdf file must exist, either from previous conversion
  ### #  or from converting it right now
  if (!file.exists(sGraphicFile))
    stop("Cannot find graphic file: ", sGraphicFile)
  ### # in case a width scale was specified, use it
  if (!is.null(pnPaperWidthScale))
    genericScaledPlot(pnPaperWidthScale = pnPaperWidthScale)
  ### # output the command to include the figure
  cat("![", psOdgFileStem, "](", sGraphicFile, ")\n", sep = "")

}


#' Insert a plot into a document using a given width scale
#'
#' @description
#' \code{genericScaledPlot} is a work-around due to the issue with setkeys{Gin}
#' which, we have to use because of the background picture in the header and which
#' has to be specified before each plot because otherwise, the plot is not visible.
#'
#' @details
#' The function \code{genericScaledPlot} can also be used to just produce the
#' LaTeX statement that sets the Gin key. This can be done when leaving
#' pfPlotMethod NULL.
#'
#' @param pData               data to be plotted
#' @param pnPaperWidthScale   scale factor for graphics width
#' @param pfPlotMethod        function that should produce the plot
#' @export genericScaledPlot
genericScaledPlot <- function(pData = NULL, pnPaperWidthScale, pfPlotMethod = NULL, ...){
  cat("\\setkeys{Gin}{width=", pnPaperWidthScale, "\\paperwidth}\n", sep = "")
  if (!is.null(pfPlotMethod))
    pfPlotMethod(pData, ...)
}
