###
###
###
###   Purpose:   Creation and Conversion of ODG graphics
###   started:   2016/06/27 (pvr)
###
### ###################################################### ###

#' Convert documents from source format into target format used in the document
#'
#' \code{convertLibOToPdf} assumes that graphics or diagrams are produced by
#' LibreOffice Draw. Source files are converted on the fly to pdf which are
#' then included in the source R markdown document
#'
#' @param psLibOFile    name of the libre office graphics file
#' @param psLibODir     source directory of Libre Office files
#' @param psFigOutDir   output directory where figure pdfs are expected to be
#' @export convertLibOToPdf
convertLibOToPdf <- function(psLibOFile, psLibODir = "odg", psFigOutDir = "."){
  sOdgDir <- psLibODir
  sOdgDirWin <- gsub("/", "\\", sOdgDir, fixed = TRUE)
  sConvCmdStem <- ifelse(.Platform$OS.type == "windows",
                         '"C:/Program Files (x86)/LibreOffice 5/program/soffice" --headless --convert-to pdf',
                         "soffice --headless --convert-to pdf")
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
  sPdfFile <- gsub("odg$", "pdf", psLibOFile)
  sFigOutFile <- file.path(psFigOutDir, sPdfFile)
  file.rename(from = sPdfFile, sFigOutFile)
  return(sFigOutFile)
}


#' Create an empty odg graphic
#'
#' @description
#' \code{create_odg_graphic} uses the empty template skeleton.odg which
#' is stored in the rmarkdown template directory of this package.
#' When calling  \code{create_odg_graphic} without any arguments, then the
#' template is opened using soffice draw. When a filename for the odg graphic
#' which should be created, is specified the template is renamed to the given
#' name.
#'
#' @param psGraphicName   Format of diagram to be created
#' @export create_odg_graphic
create_odg_graphic <- function(psGraphicName  = "skeleton.odg",
                               psOdgTemplate  = "odg_figure",
                               psTemplatePkg  = "rmddochelper",
                               create_dir     = "default",
                               pbRecursive    = TRUE,
                               pbEdit         = TRUE){

  ### # use the local function rmd_draft to copy the template
  sGraphicTrgName <- odg_draft(file        = psGraphicName,
                               template    = psOdgTemplate,
                               package     = psTemplatePkg,
                               create_dir  = create_dir)

  if (pbEdit & file.exists(sGraphicTrgName)){
    ### # depending on platform start open the template file differently
    if (.Platform$OS.type == "windows"){
      file.show(sGraphicTrgName)
    } else {
      #sSofficeCmd <- paste("soffice --draw", sGraphicFile)
      #system(sSofficeCmd)
      file.edit(sGraphicTrgName)
    }

  }

  cat(" * Odg Graphics created in: ", sGraphicTrgName, "\n")

  ### # return graphics name
  return(sGraphicTrgName)
}



#' @title Copy a draft template file for a odg graphics
#'
#' @description
#' This function \code{odg_draft} works analogously to
#' \code{rmarkdown::draft}, but for ODG graphics files
#'
#'
#' @param   file          name of the new document
#' @param   template      name of the template
#' @param   package       package where template can be found
#' @param   create_dir    whether or not to create a new directory for this document
#' @param   pbOverwrite   should existing files be overwritten
#' @param   plReplace     list with replacement key-values
#' @return  file          name of the new document
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
    stop("No template.yaml file found for template '", template,
         "'")
  }
  template_meta <- rmarkdown:::yaml_load_file_utf8(template_yaml)
  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain name and description fields")
  }
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
    if (pbOverwrite)
      file.copy(from = f, to = to, overwrite = pbOverwrite, recursive = TRUE)
    if (!file.exists(file.path(to, basename(f))))
      # stop("The file '", basename(f), "' already exists")
      file.copy(from = f, to = to, overwrite = FALSE, recursive = TRUE)
  }
  ### # rename skeleton to final name
  file.rename(file.path(dirname(file), "skeleton.odg"), file)

  ### # return result file to caller
  return(file)

}


## ---- Insert a Odg draw graphic -------------------------------------------
#' Inserts an odg draw graphic into a rmarkdown text
#'
#' @description
#' \code{insertOdgAsPdf} takes the name of a file containing a graphic
#' in odg format, converts the content of that file into pdf using
#' function \code{convertLibOToPdf} and outputs the string in markdown
#' format to include the figure. Pdf-formatted graphics are only re-generated
#' if the pdf-file does not exist or, if the flag pbMustGenerate is TRUE.
#'
#' @param  psOdgFileStem    stem of odg figure file
#' @param  psOdgDir         directory where odg figure file is stored
#' @param  pbMustGenerate   flag to indicate whether pdf-graphics must be regenerated
#' @param  psFigOutDir      directory where output should be placed
#' @export insertOdgAsPdf
insertOdgAsPdf <- function(psOdgFileStem, psOdgDir = "odg",
                           psFigOutDir = ".",
                           pbMustGenerate = FALSE,
                           pnPaperWidthScale = NULL) {
  ### # check wether pdf file already exists, if so, do nothing
  sPdfFilename <- paste(psOdgFileStem, "pdf", sep = ".")
  sPdfFile <- file.path(psFigOutDir,sPdfFilename)
  if (!file.exists(sPdfFile) | pbMustGenerate){
    ### # if pdf files cannot be found, regenerate them, check that psOdgFileName exists
    sOdgFilename <- paste(psOdgFileStem, "odg", sep = ".")
    sOdgFile <- file.path(psOdgDir, sOdgFilename)
    # if (!file.exists(sOdgFile))
    #   stop("Cannot find Odg figure file: ", sOdgFile)
    ### # convert odg file to pdf
    sConvPdfFileName <- convertLibOToPdf(psLibOFile = sOdgFilename, psLibODir = psOdgDir, psFigOutDir = psFigOutDir)
    if (!file.exists(sConvPdfFileName))
      stop("Cannot find created pdf graphics file: ", sConvPdfFileName)
  }
  ### # at this point the pdf file must exist, either from previous conversion
  ### #  or from converting it right now
  if (!file.exists(sPdfFile))
    stop("Cannot find pdf figure file: ", sPdfFile)
  ### # in case a width scale was specified, use it
  if (!is.null(pnPaperWidthScale))
    genericScaledPlot(pnPaperWidthScale = pnPaperWidthScale)
  ### # output the command to include the figure
  cat("![", psOdgFileStem, "](", sPdfFile, ")\n", sep = "")
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
