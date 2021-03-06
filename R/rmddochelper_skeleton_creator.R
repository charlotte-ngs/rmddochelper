###
###
###
###   Purpose:   Routines for document creation
###   started:   2016/06/06 (pvr)
###
### ################################################### ###

#' @title Create a Rmarkdown (rmd) document with html_output
#'
#' @description
#' \code{create_html_output_skeleton} is a wrapper to
#' \code{create_docu_skeleton} for easily creating html-documents
#' with reasonable default settings.
#'
#' @param   psDocuName           name of the new document
#' @param   psPkgPath            path where package is located under which document should be created
#' @param   psRmdTemplate        name of the template to be used
#' @param   psTemplatePkg        package from where the template should be taken
#' @param   psDocuSubdir         subdirectory in which document should be saved to
#' @param   pbDocuHasOwnSubdir   should document be stored in separate subdir
#' @param   pbOverwrite          flag whether existing files are overwritten
#' @param   pbEdit               directly open newly created document
#' @param   plReplace            list with replacement key-values
#' @export  create_html_output_skeleton
create_html_output_skeleton <- function(psDocuName,
                                        psPkgPath         = ".",
                                        psRmdTemplate     = "default_html_output",
                                        psTemplatePkg     = "rmddochelper",
                                        psDocuSubdir      = "vignettes",
                                        pDocuHasOwnSubdir = TRUE,
                                        pbOverwrite       = FALSE,
                                        pbEdit            = TRUE,
                                        plReplace         = NULL){
  create_docu_skeleton(psDocuName        = psDocuName,
                       psPkgPath         = psPkgPath,
                       psRmdTemplate     = psRmdTemplate,
                       psTemplatePkg     = psTemplatePkg,
                       psDocuSubdir      = psDocuSubdir,
                       pDocuHasOwnSubdir = pDocuHasOwnSubdir,
                       pbOverwrite       = pbOverwrite,
                       pbEdit            = pbEdit,
                       plReplace         = plReplace)

}

#' @title Create a Rmarkdown (Rmd) document with pdf_output
#'
#' @description
#' Function {create_pdf_output_skeleton} is a wrapper to
#' \code{create_docu_skeleton} for easily creating pdf-documents
#' with reasonable default settings.
#'
#' @param   psDocuName           name of the new document
#' @param   psPkgPath            path where package is located under which document should be created
#' @param   psRmdTemplate        name of the template to be used
#' @param   psTemplatePkg        package from where the template should be taken
#' @param   psDocuSubdir         subdirectory in which document should be saved to
#' @param   pbDocuHasOwnSubdir   should document be stored in separate subdir
#' @param   pbOverwrite          flag whether existing files are overwritten
#' @param   pbEdit               directly open newly created document
#' @param   plReplace            list with replacement key-values
#' @export  create_pdf_output_skeleton
create_pdf_output_skeleton <- function(psDocuName,
                                       psPkgPath         = ".",
                                       psRmdTemplate     = "default_pdf_output",
                                       psTemplatePkg     = "rmddochelper",
                                       psDocuSubdir      = "vignettes",
                                       pDocuHasOwnSubdir = TRUE,
                                       pbOverwrite       = FALSE,
                                       pbEdit            = TRUE,
                                       plReplace         = NULL){
  create_docu_skeleton(psDocuName        = psDocuName,
                       psPkgPath         = psPkgPath,
                       psRmdTemplate     = psRmdTemplate,
                       psTemplatePkg     = psTemplatePkg,
                       psDocuSubdir      = psDocuSubdir,
                       pDocuHasOwnSubdir = pDocuHasOwnSubdir,
                       pbOverwrite       = pbOverwrite,
                       pbEdit            = pbEdit,
                       plReplace         = plReplace)
}

### ################################################### ###

#' Create a beamer presentation Rmarkdown (Rmd) document
#'
#' @description
#' The function \code{create_beamer_presentation} is a wrapper
#' to create_docu_skeleton using template default_pdf_beamer_presentation
#' to generate a skeleton Rmarkdown (Rmd) document. The skeleton
#' can be used to generate a beamer presentation.
#'
#' @param   psDocuName           name of the new document
#' @param   psPkgPath            path where package is located under which document should be created
#' @param   psRmdTemplate        name of the template to be used
#' @param   psTemplatePkg        package from where the template should be taken
#' @param   psDocuSubdir         subdirectory in which document should be saved to
#' @param   pbDocuHasOwnSubdir   should document be stored in separate subdir
#' @param   pbOverwrite          flag whether existing files are overwritten
#' @param   pbEdit               directly open newly created document
#' @param   plReplace            list with replacement key-values
#' @export  create_beamer_presentation
create_beamer_presentation <- function(psDocuName,
                                       psPkgPath         = ".",
                                       psRmdTemplate     = "default_pdf_beamer_presentation",
                                       psTemplatePkg     = "rmddochelper",
                                       psDocuSubdir      = "vignettes",
                                       pDocuHasOwnSubdir = TRUE,
                                       pbOverwrite       = FALSE,
                                       pbEdit            = TRUE,
                                       plReplace         = NULL){
  create_docu_skeleton(psDocuName        = psDocuName,
                       psPkgPath         = psPkgPath,
                       psRmdTemplate     = psRmdTemplate,
                       psTemplatePkg     = psTemplatePkg,
                       psDocuSubdir      = psDocuSubdir,
                       pDocuHasOwnSubdir = pDocuHasOwnSubdir,
                       pbOverwrite       = pbOverwrite,
                       pbEdit            = pbEdit,
                       plReplace         = plReplace)

}

### ################################################### ###

#' Create a skeleton for a new Rmarkdown (Rmd) document
#'
#' @description
#' \code{create_docu_skeleton} creates a skeleton document from
#' a template. The template is either specified by a filepath
#' to the template file or by a template name and a package which
#' contains the template. The latter case assumes that psPkgPath is a
#' directory that contains an R-package. By default the new
#' document is created in subdirectory "vignettes". If this
#' subdirectory does not exist, it is created. The document
#' is generated by a function that is similar to \code{rmarkdown::draft}.
#'
#' @details
#' The basic functionality follows the function
#' \code{devtools::use_vignette}, except for the possibility
#' of specifying any given template from any package.
#'
#' @param   psDocuName           name of the new document
#' @param   psPkgPath            path where package is located under which document should be created
#' @param   psRmdTemplate        name of the template to be used
#' @param   psTemplatePkg        package from where the template should be taken
#' @param   psDocuSubdir         subdirectory in which document should be saved to
#' @param   pbDocuHasOwnSubdir   should document be stored in separate subdir
#' @param   pbOverwrite          flag whether existing files are overwritten
#' @param   pbEdit               directly open newly created document
#' @param   plReplace            list with replacement key-values
#' @export create_docu_skeleton
create_docu_skeleton <- function(psDocuName,
                                 psPkgPath         = ".",
                                 psRmdTemplate,
                                 psTemplatePkg     = NULL,
                                 psDocuSubdir      = "vignettes",
                                 pDocuHasOwnSubdir = TRUE,
                                 pbOverwrite       = FALSE,
                                 pbEdit            = TRUE,
                                 plReplace         = NULL) {
  ### # if psPkgPath contains a package, do the preparation similar to devtools::use_vignette
  if (devtools::is.package(psPkgPath)){
    pkg <- devtools::as.package(psPkgPath)
    devtools:::check_suggested("rmarkdown")
    devtools:::add_desc_package(pkg, "Suggests", "knitr")
    devtools:::add_desc_package(pkg, "Suggests", "rmarkdown")
    devtools:::add_desc_package(pkg, "VignetteBuilder", "knitr")
    sPkgPath <- pkg$path
  } else {
    sPkgPath <- psPkgPath
  }
  ### # put together path and file name
  sDocuPath <- file.path(sPkgPath, psDocuSubdir, paste0(psDocuName, ".Rmd"))
  sCreatedFile <- rmd_draft(file        = sDocuPath,
                            template    = psRmdTemplate,
                            package     = psTemplatePkg,
                            create_dir  = pDocuHasOwnSubdir,
                            pbOverwrite = pbOverwrite,
                            plReplace   = plReplace)

  if (pbEdit) file.edit(sCreatedFile)
  message("Draft vignette created in ", sCreatedFile)

}

#' Custom local copy of rmarkdown::draft
#'
#' @description
#' \code{rmd_draft} corresponds to a local copy of
#' \code{rmarkdown::draft}. In contrast to the original
#' version, this version allows for the use of templates
#' with skeleton files which are already found in the
#' target directory.
#'
#'
#' @param   file          name of the new document
#' @param   template      name of the template
#' @param   package       package where template can be found
#' @param   create_dir    whether or not to create a new directory for this document
#' @param   pbOverwrite   should existing files be overwritten
#' @param   plReplace     list with replacement key-values
#' @return  file          name of the new document
rmd_draft <- function(file, template,
                      package     = NULL,
                      create_dir  = "default",
                      pbOverwrite = FALSE,
                      plReplace   = NULL){
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
  if (!identical(tolower(tools::file_ext(file)), "rmd"))
    file <- paste(file, ".Rmd", sep = "")
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
  ### # in case a replacement list was specified, use it to replace
  ### #  placeholders in skeleton file
  if (is.null(plReplace)){
    file.rename(file.path(dirname(file), "skeleton.Rmd"), file)
  } else {
    replace_placeholders(psTrgFile = file, plReplace = plReplace)
  }
  ### # return result file to caller
  return(file)

}


#' @title Replacement of placeholders in skeletonfile
#'
#' @description
#' Placeholders in skeleton files are replaced using gsub
#' functionalities
#'
#' @param psTrgFile   path to and including name of targetfile
#' @param plReplace   replacement list
replace_placeholders <- function(psTrgFile, plReplace){
  ### # read skeleton file into a string
  sSkelFile <- file.path(dirname(psTrgFile), "skeleton.Rmd")
  con <- file(description = sSkelFile)
  sSkel <- paste0(readLines(con = con), collapse = "\n")
  close(con)
  ### # do the replacement using gsub based function
  sReplResult <- gsub_templ_pattern(ps_docu_template = sSkel, pl_repl_info = plReplace)
  ### # writing result to result file
  cat(sReplResult, "\n", file = psTrgFile)
  ### # remove skeleton
  file.remove(sSkelFile)
}

#' Convert a tag to a replacement placeholder
#'
#' @param    psTag   tag to be converted to a placeholder
#' @return   replacement placeholder
get_repl_tag <- function(psTag){
  return(paste0("[REPLACE_WITH_", toupper(psTag), "]"))
}

#' Replacement of placeholder by replacement list values
#'
#' @param    ps_docu_template   document template string
#' @param    pl_repl_info       replacement info list
#' @return   s_docu_result      document result string
gsub_templ_pattern <- function(ps_docu_template, pl_repl_info){
  s_docu_result <- ps_docu_template
  for (sTag in names(pl_repl_info)){
    s_docu_result <- gsub(get_repl_tag(psTag = sTag),
                          pl_repl_info[[sTag]],
                          s_docu_result,
                          fixed = TRUE)
  }
  return(s_docu_result)
}

