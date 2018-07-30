###
###
###
###   Purpose:   Creation and Conversion of ODG graphics
###   started:   2016/06/27 (pvr)
###
### ###################################################### ###


## ---- Creation and drafting of  odg graphics -------------------------------------------

#' @title Create New ODG Graphics Object
#'
#' @description
#' Similar to the \code{devtools::use_*} functions, we want to
#' create a new ODG graphics object. This new graphics object
#' will be stored in a file which is copied from
#' a template from a given R-package. By default, the template
#' used is called odg_figure from the rmddochelper package.
#' After copying the template, it can be modified using a
#' pre-defined tool. By default this tool is LibreOffice draw.
#'
#' @examples
#' \dontrun{
#' rmddochelper::use_odg_graphic(ps_odg_file = "my_odg_graphic.odg")
#' }
#'
#' @param ps_path             file name including path of ODG graphics object file
#' @param ps_cwd              current working directory
#' @param ps_rmd_src          explicit file name of rmd-source file
#' @param ps_odg_template     name of the template to be used
#' @param ps_template_package package from which template should be taken from
#' @param pb_recursive        flag whether missing directory should be created
#' @param pb_edit             flag to indicate whether odg file should be opened
#' @param pb_insert_include   flag indicating whether graphic include command should be inserted into rmd source file
#' @return s_odg_trg          name of and path to the created odg graphics file
#' @export use_odg_graphic
use_odg_graphic <- function(ps_path,
                            ps_cwd              = getwd(),
                            ps_rmd_src          = NULL,
                            ps_odg_template     = "odg_figure",
                            ps_template_package = "rmddochelper",
                            pb_recursive        = TRUE,
                            pb_edit             = TRUE,
                            pb_insert_include   = TRUE){
  ### # extract basename and dirname from ps_odg_file
  s_odg_dir <- dirname(ps_path)
  s_odg_base <- basename(ps_path)
  ### # in recursive mode, if s_odg_dir does not exist, create it
  if (!dir.exists(s_odg_dir)) {
    if (pb_recursive){
      dir.create(s_odg_dir)
    } else {
      stop(" *** ERROR: Cannot find s_odg_dir: ", s_odg_dir, ".\n",
           " ***        If it should be created set pb_recusive = TRUE")
    }
  }

  ### # copying the draft from the template, if it does not exist
  ### # use the local function odg_draft to copy the template
  if ( !file.exists(ps_path) ) {
    s_odg_trg <- odg_draft( file        = ps_path,
                            template    = ps_odg_template,
                            package     = ps_template_package )

  } else {
    s_odg_trg <- ps_path
  }

  ### # in case pb_edit is TRUE, open the created draft file
  if (pb_edit){
    s_odg_tool_path <- get_odg_prog_path()
    s_odg_edit_cmd <- paste(s_odg_tool_path, "--draw", s_odg_trg)
    system(s_odg_edit_cmd)
  }

  ### # try to insert include_graphics command into rmd
  if (pb_insert_include)
    insert_include_command(ps_path = ps_path, ps_rmd_src = ps_rmd_src, ps_cwd = ps_cwd)

  ### # return name of odg target
  return(s_odg_trg)

}


#' @title Insert Graphics Inclusion Command Into Rmd-Source
#'
#' @description
#' When creating a new graphics object, this function tries to automatically
#' generate the associated command to include the generated graphics object
#' into the respective Rmd-source document. This process needs as input
#' the current working directory where of the Rmd-source document and the
#' name of the file in which the graphics object is stored.
#'
#' @param ps_path    path to the odg graphics file
#' @param ps_rmd_src name of and path to the rmd-source file
#' @param ps_cwd     current working directory of the rmd-source file
insert_include_command <- function(ps_path,
                                   ps_rmd_src = NULL,
                                   ps_cwd     = get_wd()){
  ### # in case rmd-source file is not given by ps_rmd_src parameter, try
  ### #  to determine it via a search of ps_path in all files in ps_cwd
  if (is.null(ps_rmd_src)){
    # s_rmd_src <- get_current_rmd_src(ps_path = ps_path, ps_cwd = ps_cwd)
    l_rmd_src <- get_rmd_src_with_pos(ps_path = ps_path, ps_cwd = ps_cwd)

  } else {
    l_rmd_src <- list(name = ps_rmd_src,
                      position = get_pat_pos(ps_file = ps_rmd_src, ps_pattern = ps_path))
  }

  ### # if rmd-source file was specified or was found, we try and insert
  if (!is.null(l_rmd_src$position)){
    con_rmd <- file(description = l_rmd_src$name)
    vec_rmd_src <- readLines(con = con_rmd)
    close(con_rmd)
    ### # loop over positions and insert the include command
    for (pids in seq_along(l_rmd_src$position)){
      p <- l_rmd_src$position[pids]
      ### # change chunk options
      vec_rmd_src[p-1] <- paste0('```{r ', tools::file_path_sans_ext(basename(ps_path)),
                                 ', echo=FALSE, hook_convert_odg=TRUE, fig_path="',
                                 dirname(ps_path), '"}')
      vec_rmd_src[p] <- paste0('#', vec_rmd_src[p])
      vec_rmd_src[p+1] <- paste0('knitr::include_graphics(path = "',
                                 gsub(pattern = "odg$", replacement = "png", x = ps_path),
                                 '")\n',
                                 vec_rmd_src[p+1], collapse = '')

    }
    ### # write output back again
    cat(paste0(vec_rmd_src, collapse = "\n"), "\n", file = l_rmd_src$name)

  }

  return(invisible(TRUE))
}



# Odg Conversion functions ---------------------------------------------------------


#' Odg Graphics File Conversion Hook
#'
#' @description
#' Graphics objects in odg-format are converted to png
#' or pdf format by this knitr-hook-function. We assume
#' that this hook-function is included in the chunk where
#' the graphics object is included using a call to
#' \code{knitr::include_graphics()}, hence the conversion
#' of the odg-file must be run before the code in the chunk.
#'
#' @param before   flag whether hook is run before or after chunk
#' @param options  list of options passed from chunk to hook
#' @param envir    environment
#'
#' @examples
#' \dontrun{
#' # registration of hook function
#' knitr::knit_hooks$set(hook_convert_odg = rmddochelper::hook_convert_odg)
#' ...
#' }
#'
#' @export hook_convert_odg
hook_convert_odg <- function(before, options, envir){
  ### # in the chunk that contains the call to knitr::include_graphics,
  ### #  the conversion must be run before the code in the chunk
  if (before){
    ### # set some defaults for parameter or take them from options
    odg_path <- paste(options$label, "odg", sep=".")
    ### # take only fig_path from options and file name from label
    if (!is.null(options$fig_path)){
      odg_path <- file.path(options$fig_path, paste(options$label, "odg", sep="."))
    }
    if (!is.null(options$odg_path)){
      odg_path <- options$odg_path
    }
    ### # check that file specified by odg_path exists, o/w stop
    if (!file.exists(odg_path))
      stop(" *** * ERROR [rmddochelper::hook_convert_odg]: Cannot find odg-file: ", odg_path)

    ### # determine to which output formats we want to convert the odg file
    out_format <- c("pdf", "png")
    if (!is.null(options$out_format)){
      out_format <- options$out_format
    }
    ### # loop over formats and convert
    for(fmt in out_format){
      convert_odg(ps_odg_path = odg_path, ps_out_format = fmt)
    }

  }
  return(invisible(NULL))

}

#' Conversion Hook for Odg Graphics Objects
#'
#' @description
#' A graphics object in an odg-formatted file cannot be
#' included directly into an Rmarkdown source document.
#' The odg-file must first be converted into a format
#' such as pdf or png. This conversion is done automatically
#' by this hook-function whenever a R-code-chunk with a
#' statement to include a graphics object is executed.
#'
#' @param before   running before chunk code
#' @param options  chunk label options
#' @param envir    environment
#' @return command to include the converted graphics object files
#' @export odg_convert_hook
odg_convert_hook <- function(before, options, envir){
  ### # run conversion after chunk has been evaluated, following
  ### #  the example given in https://yihui.name/knitr/hooks/
  if (!before){
    ### # set some defaults for parameter or take them from options
    odg_path <- file.path("odg", paste(options$label, "odg", sep="."))
    ### # take only fig_path from options and file name from label
    if (!is.null(options$fig_path)){
      odg_path <- file.path(options$fig_path, paste(options$label, "odg", sep="."))
    }
    if (!is.null(options$odg_path)){
      odg_path <- options$odg_path
    }
    ### # check that file specified by odg_path exists, o/w stop
    if (!file.exists(odg_path))
      stop(" *** * ERROR [rmddochelper::odg_convert_hook]: Cannot find odg-file: ", odg_path)

    ### # determine to which output formats we want to convert the odg file
    out_format <- c("pdf", "png")
    if (!is.null(options$out_format)){
      out_format <- options$out_format
    }
    ### # loop over formats and convert
    for(fmt in out_format){
      convert_odg(ps_odg_path = odg_path, ps_out_format = fmt)
    }

    ### # return the command to be included
    s_out_file <- paste0(tools::file_path_sans_ext(basename(odg_path)), '.png')
    return(paste0('\n\n```{r}\n','knitr::include_graphics(path = "',
                  file.path(dirname(odg_path), s_out_file),
                  '")\n```\n\n'))

  }

  return(invisible(TRUE))


}


#' Converter Function from Odg to Other Formats
#'
#' @description
#' The conversion is done using the tool returned by \code{get_odg_prog_path()}.
#' The current version of this function is not vectorized, hence the arguments
#' can only be single odg-files in ps_odg_path and single formats in ps_out_format.
#' The conversion is only done, if the result file does not exist.
#'
#' @param ps_odg_path   path to odg file to be converted
#' @param ps_out_format output format into which ps_odg_path should be converted to
convert_odg <- function(ps_odg_path, ps_out_format){
  ### # check restriction of only one odg-file and only
  ### #  one format
  if (length(ps_odg_path) > 1 | length(ps_out_format) > 1)
    stop(" *** * ERROR [rmddochelper:::convert_odg] works only on single arguments")

  ### # check whether conversion result already exists
  s_out_file <- paste(tools::file_path_sans_ext(basename(ps_odg_path)), ps_out_format, sep = ".")
  s_out_path <- file.path(dirname(ps_odg_path), s_out_file)
  ### # return here if result file s_out_path exists
  if (file.exists(s_out_path))
    return(invisible(TRUE))

  ### # get the path to the conversion tool
  s_odg_prog_path <- get_odg_prog_path()
  ### # add options and format to conversion command
  s_conv_cmd <- paste0(s_odg_prog_path, " --headless --convert-to ", ps_out_format, " ", ps_odg_path)
  ### # do the conversion
  system(command = s_conv_cmd)
  ### # put the generated result file in the same directory as ps_odg_path
  file.rename(from = s_out_file, to = s_out_path)

}

## --- Helper functions related to Odg-graphics -------------------------------------------------
#
#' @title Return Path To Program Used To Create ODG-graphics
#'
#' @description
#' The path to the program used to create odg-graphics depends
#' on the OS. We use the function get_os() to determine on which
#' OS, we are running and based on that, we are returning a
#' fixed string corresponding to the path to the program for
#' odg-graphics. In most cases,
#' LibreOffice draw is used to create the odg-graphics files.
#'
#' @examples
#' \dontrun{
#' get_odg_prog_path()
#' # returns /Applications/LibreOffice.app/Contents/MacOS/soffice on OSX
#' }
#' @export get_odg_prog_path
get_odg_prog_path <- function(){
  ### # first we have to know the os
  s_os <- get_os()
  ### # fix the path according to the os
  if (s_os == "windows"){
    return("c/Program Files/LibreOffice/program/soffice")
  } else if (s_os == "osx"){
    return("/Applications/LibreOffice.app/Contents/MacOS/soffice")
  } else {
    return("soffice")
  }
}


#' @title Find Name of Current Rmd-Source Document
#'
#' @description
#' Given the value passed by the parameter ps_path, we can
#' assume that there must be a Rmd-source file in the
#' current working directory ps_cwd which contains the path
#' to the odg graphics file given in ps_path. Hence, we
#' can search through all Rmd-files in ps_cwd for the
#' value in ps_path. If we find a match in a single Rmd-source
#' file, we return the name of that file, otherwise we
#' return NULL.
#'
#' @param ps_path name of and path to odg-graphics file
#' @param ps_cwd  current working directory
#'
#' @return s_rmd_src_result resulting Rmd-source file
get_current_rmd_src <- function(ps_path, ps_cwd){
  ### # initialize the result to be NULL
  s_rmd_src_result <- NULL
  ### # get the name of all Rmd-source documents in ps_cwd
  vec_rmd_src <- list.files(path = ps_cwd, pattern = "Rmd$")
  ### # in case any Rmd-source files are found search through them
  if (length(vec_rmd_src) > 0){
    n_rmd_src_idx <- which(sapply(vec_rmd_src,
                            function(x) has_file_search_pat(ps_file = x, ps_pattern = ps_path),
                            USE.NAMES = FALSE))
    ### # result is only used, if s_pattern occurs in just one file
    if (length(n_rmd_src_idx) == 1)
      s_rmd_src_result <- vec_rmd_src[n_rmd_src_idx]
  }

  ### # return result
  return(s_rmd_src_result)
}



#' Return rmd source file with positions where search pattern occurs
#'
#' Given a search pattern in ps_path and given the path of the current
#' working directory, all files with extension .Rmd are searched whether
#' the string in ps_path occurs in the rmd-source file. If the pattern
#' is found in exactly one Rmd-source file, the name of the Rmd-source
#' file and the positions where the pattern was found is returned. If
#' the pattern is not found or the pattern occurs in more than one
#' Rmd-source file, NULL is returned.
#'
#' @param ps_path name of and path to odg-graphics file
#' @param ps_cwd current working directory
#'
#' @return l_rmd_src_result list with name of rmd source file and positions where search pattern occurs
get_rmd_src_with_pos <- function(ps_path, ps_cwd){
  ### # initialize the result to be NULL
  l_rmd_src_result <- NULL
  ### # get the name of all Rmd-source documents in ps_cwd
  vec_rmd_src <- list.files(path = ps_cwd, pattern = "Rmd$", full.names = TRUE)
  ### # in case any Rmd-source files are found search through them
  if (length(vec_rmd_src) > 0){
    l_rmd_src <- lapply(vec_rmd_src, function(x) get_pat_pos(x, ps_pattern = ps_path))
    ### # get indices of l_rmd_src of entries which are not NA
    n_rmd_src_idx <- which(!is.na(l_rmd_src))

    ### # result is only used, if s_pattern occurs in just one file
     if (length(n_rmd_src_idx) == 1)
       l_rmd_src_result <- list(name = vec_rmd_src[n_rmd_src_idx], position = l_rmd_src[[n_rmd_src_idx]])
  }
  ### # return result
  return(l_rmd_src_result)
}




#' Get position where search pattern occurs in a file
#'
#' Given a search pattern and given a file that is specified
#' by its complete path, the positions where the search
#' pattern occurs is returned. Positions correspond to
#' vector indices when the content of the file is read
#' using the function \code{readLines}. These vector indices
#' are equivalent to line numbers of the text. In case
#' when the search pattern is not found, NA is returned.
#'
#' @param ps_pattern search pattern for which we search in the file
#' @param ps_file name of the file to search for pattern
#' @return vec_pos_found vector of positions where pattern occurs,
#'                       NA if pattern was not found
get_pat_pos <- function(ps_file, ps_pattern){
  ### # check wheter ps_file is found
  if (!file.exists(ps_file)) stop(" *** * ERROR[has_file_search_pat]: cannot find file ", ps_file)
  ### # open connection to ps_file
  con <- file(description = ps_file)
  ### # read content into character vector
  vec_rmd_src <- readLines(con = con)
  ### # close connection con
  close(con)
  ### # search for pattern and return the positions
  ### #  where the pattern was found
  vec_pos_found <- grep(pattern = ps_pattern, x = vec_rmd_src, fixed = TRUE)
  ### # return the result, if pattern was not found
  ### #  return NA
  if (length(vec_pos_found) == 0) return(NA)
  return(vec_pos_found)
}


#' Check whether ps_pattern is found in ps_file
#'
#' The check is done via grep for the pattern in the
#' file. First, the file content is read into a character
#' vector, then we search for the pattern. We are
#' only interested in exact matches, hence the argument
#' fixed=TRUE is specified for grep().
#'
#' @param ps_file name of file in which search is done
#' @param ps_pattern pattern for which we search for
#'
#' @return TRUE, if we have an exact match of ps_pattern in ps_file, FALSE otherwise
has_file_search_pat <- function(ps_file, ps_pattern){
  ### # check wheter ps_file is found
  if (!file.exists(ps_file)) stop(" *** * ERROR[has_file_search_pat]: cannot find file ", ps_file)
  ### # open connection to ps_file
  con <- file(description = ps_file)
  ### # read content into character vector
  vec_rmd_src <- readLines(con = con)
  ### # close connection con
  close(con)
  ### # search for pattern
  b_pattern_found <- grepl(pattern = ps_pattern, x = vec_rmd_src, fixed = TRUE)
  ### # return TRUE, if pattern was found at least once
  return(any(b_pattern_found))
}


## ---- Old versions of Creation and drafting of  odg graphics -------------------------------------------

#' @title Convert documents from source format into a given target output format
#'
#' @description
#' \code{convertLibOToGraphic} assumes that LibreOffice is installed
#' and is ideally available on the search path. Because more recent
#' versions of windows and mac osx appear to have problems with that
#' we started to use absolute paths to LibO installations. Source files
#' which are assumed to LibO-draw files in .odg-format are converted on the
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
  s_os_name <- get_os()
  if (s_os_name == "windows"){
    sConvCmdStem <- '"C:/Program Files (x86)/LibreOffice 5/program/soffice"'
  } else if (s_os_name == "osx"){
    sConvCmdStem <- '/Applications/LibreOffice.app/Contents/MacOS/soffice'
  } else {
    sConvCmdStem <- 'soffice'
  }
  sConvCmdStem <- paste(sConvCmdStem, '--headless --convert-to', sOutFormat)
  ### # construct path to figure file
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
  # sOutFile <- gsub("odg$", sOutFormat, psLibOFile)
  sOutFile <- paste(tools::file_path_sans_ext(psLibOFile), psOutFormat, sep = ".")
  sFigOutFile <- file.path(psFigOutDir, sOutFile)
  if (!dir.exists(psFigOutDir))
    dir.create(path = psFigOutDir)
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
#' @details
#' If the parameter psRmdSrcFile is specified, a command to include the
#' created graphic file in the specified format is inserted into the
#' rmd-source file. If psRmdSrcFile is left at its default (null) then
#' no include command is inserted. The command to include graphics
#' is based on the function \code{knitr::include_graphics()}. When
#' specifying the parameter psChunkLabel an additional chunk label
#' can be specified.
#'
#' @param psGraphicName   Format of diagram to be created
#' @param psGraphicPath   Path where created odg file should be stored
#' @param psRmdSrcFile    rmd source file where include statement is inserted
#' @param psGrFmt         graphics format
#' @param psOdgTemplate   name of the template to be used
#' @param psTemplatePkg   package where template is stored
#' @param create_dir      should created odg file be stored in separate directory
#' @param pbRecursive     recursively create complete path to graphic file
#' @param pbEdit          directly edit created odg file
#' @param psChunkLabel    string with additional chunk labels
#' @export create_odg_graphic
create_odg_graphic <- function(psGraphicName  = "skeleton.odg",
                               psGraphicPath  = "vignettes",
                               psRmdSrcFile   = NULL,
                               psGrFmt        = "pdf",
                               psOdgTemplate  = "odg_figure",
                               psTemplatePkg  = "rmddochelper",
                               create_dir     = "default",
                               pbRecursive    = TRUE,
                               pbEdit         = TRUE,
                               psChunkLabel   = 'echo=FALSE, odg.conv=TRUE, odg.path="odg", odg.graph.cache=TRUE'){

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

  ### # insert command to include graphics file into source file, if
  ### #  the name of the rmd-src-file is specified
  if (!is.null(psRmdSrcFile)){
    conRmdSrc <- file(description = psRmdSrcFile)
    vRmdSrc <- readLines(con = conRmdSrc)
    close(con = conRmdSrc)
    vRmdSrc <- knitr_include_graphics_pdf(psGraphicName = psGraphicName,
                                          pvRmdSrc      = vRmdSrc,
                                          psGrFmt       = psGrFmt,
                                          psChunkLabel  = psChunkLabel )
    cat(vRmdSrc, "\n", file = psRmdSrcFile, sep = "\n")
  }


  ### # depending on flag, open graphics file
  if (pbEdit){
    ### # depending on platform start open the template file differently
    ### # decide os-type based on built-in function get_os()
    s_os_type <- get_os()
    if (s_os_type == "windows"){
      file.show(sGraphicTrgName)
    } else if (s_os_type == "osx") {
      sSofficeCmd <- paste('/Applications/LibreOffice.app/Contents/MacOS/soffice --draw', sGraphicTrgName)
    } else {
      sSofficeCmd <- paste("soffice --draw", sGraphicTrgName)
      system(sSofficeCmd)
    }

  }

  cat(" * Odg Graphics created in: ", sGraphicTrgName, "\n")

  ### # return graphics name
  return(sGraphicTrgName)
}


#' @title Add statement to include graphic via knitr::include_graphics()
#'
#' @description Statement of knitr::include_graphics with correct graphic name
#' is added on a new line inside the Rmarkdown (rmd) source document text given
#' in the parameter pvRmdSrc. The parameter psGrFmt can be used to specify
#' different formats of the graphics file to be included. The string in
#' psChunkLabel is treated as additional chunk labels which are inserted in the
#' chunk that includes the graphics file.
#'
#' @details The use of this function is deprecated. Use the newer functionality
#' around the function \code{rmddochelper::use_odg_graphic()}
#'
#' @param psGraphicName   name of the graphic file to be included
#' @param pvRmdSrc        vector with Rmd-source text
#' @param psGrFmt         graphics format
#' @param psChunkLabel    string of chunk labels to be added
#' @return vector with extended Rmarkdown sources
#'
knitr_include_graphics_pdf <- function(psGraphicName, pvRmdSrc, psGrFmt, psChunkLabel){
  sGrInsCmd <- 'knitr::include_graphics(path = "'
  vRmdSrc <- pvRmdSrc
  ### # the search pattern is define by what RStudio inserts when
  ### #  inserting a new code-chunk
  sSearchPattern <- paste0("```{r ", psGraphicName)
  nGrInclLineIdx <- grep(pattern = sSearchPattern, vRmdSrc, fixed = TRUE)
  ### # in case the graphics inclusion statement was found,
  ### #  add the command here, if it is not found we do nothing
  if (length(nGrInclLineIdx) > 0){
    ### # if list of chunk labels is specified, add them
    if (!is.null(psChunkLabel)) {
      ### # put additional
      vRmdSrc[nGrInclLineIdx] <- paste0("```{r ", psGraphicName, ",", psChunkLabel, "}")
    }
    ### # depending on format
    vRmdSrc[nGrInclLineIdx+1] <- paste0(sGrInsCmd, psGraphicName, ".", psGrFmt, '")')
  }
  return(vRmdSrc)
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
#' @param  pnPaperWidthScale  scale factor for produced graphic
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
#' @param  pnPaperWidthScale  scale factor for produced graphic
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
#' @param  pnPaperWidthScale  scale factor for produced graphic
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


## --- Insert an Ods-table as graphic -----------------------------------------------------
##
#' Insert an LibreOffice table into a Rmd document
#'
#'
#' @param  psOdsFileStem    stem of ods file
#' @param  psOdsDir         directory where ods file is stored
#' @param  pbMustGenerate   flag to indicate whether pdf-graphics must be regenerated
#' @param  psFigOutDir      directory where output should be placed
#' @param  pnPaperWidthScale  scale factor for produced graphic
#' @export insertOdsAsPdf
insertOdsAsPdf <- function(psOdsFileStem,
                           psOdsDir = "ods",
                           psFigOutDir = ".",
                           pbMustGenerate = FALSE,
                           pnPaperWidthScale = NULL) {
  includeOdsTable( psOdsFileStem     = psOdsFileStem,
                   psOutFormat       = "pdf",
                   psOdsDir          = psOdsDir,
                   psFigOutDir       = psFigOutDir,
                   pbMustGenerate    = pbMustGenerate,
                   pnPaperWidthScale = pnPaperWidthScale )
}

#' Generic Method to include Ods tables as graphics
#'
#' This function works the same as includeOdgGraphic, but
#' it converts LibreOffice Tables from ods-format to pdf.
#'
#' @param  psOdsFileStem    stem of odg figure file
#' @param  psOutFormat      output format of the graphic file to be included
#' @param  psOdsDir         directory where odg figure file is stored
#' @param  pbMustGenerate   flag to indicate whether pdf-graphics must be regenerated
#' @param  psFigOutDir      directory where output should be placed
#' @param  pnPaperWidthScale  scale factor for produced graphic
includeOdsTable <- function( psOdsFileStem,
                             psOutFormat       = "pdf",
                             psOdsDir,
                             psFigOutDir       = ".",
                             pbMustGenerate    = FALSE,
                             pnPaperWidthScale = NULL ){
  ### # check wether pdf file already exists, if so, do nothing
  sTableFilename <- paste(psOdsFileStem, tolower(psOutFormat), sep = ".")
  sTableFile <- file.path(psFigOutDir,sTableFilename)
  if (!file.exists(sTableFile) | pbMustGenerate){
    ### # if pdf files cannot be found, regenerate them, check that psOdgFileName exists
    sOdsFilename <- paste(psOdsFileStem, "ods", sep = ".")
    sOdsFile <- file.path(psOdsDir, sOdsFilename)
    ### # convert Ods file to pdf
    sConvTableFileName <- convertLibOToGraphic( psLibOFile  = sOdsFilename,
                                                psOutFormat = psOutFormat,
                                                psLibODir   = psOdsDir,
                                                psFigOutDir = psFigOutDir)
    if (!file.exists(sConvTableFileName))
      stop("Cannot find created graphics file: ", sConvTableFileName)
  }
  ### # at this point the pdf file must exist, either from previous conversion
  ### #  or from converting it right now
  if (!file.exists(sTableFile))
    stop("Cannot find graphic file: ", sTableFile)
  ### # in case a width scale was specified, use it
  if (!is.null(pnPaperWidthScale))
    genericScaledPlot(pnPaperWidthScale = pnPaperWidthScale)
  ### # output the command to include the figure
  cat("![", psOdsFileStem, "](", sTableFile, ")\n", sep = "")

  invisible(NULL)

}

## --- Odg-graphics converter hook-function -------------------------------------------------
##
#' @title Hook function for odg-graphics conversion
#'
#' @description
#' \code{odg.graphics.conv.hook} is a hook function for knitr chunks which include
#' given graphics using function \code{knitr::include_graphics()}. Whenever graphics
#' are produced using LibreOffice Draw and saved as odg-files this hook function can
#' be used to automatically convert the odg-graphics files into files of a different format.
#'
#' @details
#' The output format is either set by default to pdf or it is taken from options$out.format.
#' In the current version no validity checks for the output format is done. Whatever can be
#' handled by LibreOffice which is doing the conversion is fine.
#'
#' Please have a look at the examples section, to see how this hook function can be used. At
#' least two things must be done. First the hook function must be registered using the
#' function \code{knitr::knit_hooks$set()} and secondly the code junk must activate the
#' usage of the hook-function by setting the label used in the activation function to TRUE.
#'
#' Paths in the chunk-labels are either absolute or relative to the location of the Rmd-file
#'
#' @param before  flat to indicate which statements must be executed before the chunk
#' @param options list of options passed from the chunk header to the hook function
#' @param envir   environment
#' @examples
#' \dontrun{
#' # activation of hook function somewhere at the top of the document
#' ```{r setup, include=FALSE}
#' knitr::knit_hooks$set(odg.conv = rmddochelper::odg.graphics.conv.hook)
#' ```
#' # ... later in the document use the hook function in a junk
#' ```{r some_graphic, odg.conv = TRUE, odg.path="<path-to-odg-files", odg.out.dir="path-to-odg.out.dir"}
#' knitr::include_graphics(path = "<path-to-odg.out.dir>/some_graphic")
#' ```
#'  }
#' @export odg.graphics.conv.hook
odg.graphics.conv.hook <- function(before, options, envir) {
  rdhlogfile <- NULL
  if (!is.null(getOption("rdhlogfile"))){
    rdhlogfile <- getOption("rdhlogfile")
  }
  ### # add some statements for debugging
  if (getOption("verbose") & !is.null(rdhlogfile))
    cat(" *** * calling odg.graphics.conv.hook with root.dir: ", knitr::opts_knit$get("root.dir"), "\n",
        file = rdhlogfile)

  ### # make sure that chunk has a label
  if (is.null(options$label))
    stop(" *** ERROR: chunk must be labelled\n")
  if (getOption("verbose"))
    cat(" *** * chunk label: ", options$label, "\n",
        file = rdhlogfile)

  ### # set path either to odg per default, or take it from options
  if (is.null(options$odg.path)){
    odg.path <- "odg"
  } else {
    odg.path <- options$odg.path
  }
  ### # add some statements for debugging
  if (getOption("verbose") & !is.null(rdhlogfile))
    cat(" *** * odg.path: ", odg.path, "\n",
        file = rdhlogfile)
  ### # set vector of output formats to be "pdf" by default or take it from options
  if (is.null(options$odg.out.format)){
    vecOutFormat <- c("pdf")
  } else {
    vecOutFormat <- options$odg.out.format
  }
  ### # add some statements for debugging
  if (getOption("verbose") & !is.null(rdhlogfile)) {
    cat(" *** * output formats: \n",
        file = rdhlogfile)
    print(vecOutFormat)
  }

  ### # odg file name and source
  odg.fname <- paste(options$label, "odg", sep = ".")
  odg.fig.src  <- file.path(odg.path, odg.fname)
  if (getOption("verbose") & !is.null(rdhlogfile))
    cat(" *** * odg.filename: ", odg.fname, "\n", " *** * odg.figure.source: ", odg.fig.src, "\n",
        file = rdhlogfile)
  ### # output directory
  out.dir <- "png"
  if (!is.null(options$odg.out.dir))
    out.dir <- options$odg.out.dir
  if (getOption("verbose") & !is.null(rdhlogfile))
    cat(" *** * output dir: ", out.dir, "\n",
        file = rdhlogfile)

  ### # vector of output formats
  for (sOutFormat in vecOutFormat) {
    trg.fig <- paste(options$label, sOutFormat, sep = ".")
    if (before) {
      if (!file.exists(odg.fig.src))
        stop("Cannot find diagram source file: ", trg.fig)
      if (!file.exists(file.path(out.dir, trg.fig)) | !options$odg.graph.cache){
        if (getOption("verbose") & !is.null(rdhlogfile))
          cat(" *** * call convertLibOToGraphic with args:",
              "\n   *** * > psLibOFile: ", odg.fname,
              "\n   *** * > psOutFormat: ", sOutFormat,
              "\n   *** * > psLibODir: ", odg.path,
              "\n   *** * > psFigOutDir: ", out.dir,
              file = rdhlogfile)
          convertLibOToGraphic(psLibOFile  = odg.fname,
                             psOutFormat = sOutFormat,
                             psLibODir   = odg.path,
                             psFigOutDir = out.dir)

      }
    }

  }
  return(invisible(TRUE))
}




