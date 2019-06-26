###
###
###
###    Purpose:   R6Class for a MendeleyExporterTool
###    started:   2019-02-07 (pvr)
###
### ################################################## ###

#' @title R6 Class For A Mendeley Exporter Tool
#'
#' @docType class
#' @importFrom R6 R6Class
#' @description
#' Rmarkdown documents use BibTeX files to include literature references. BibTeX files
#' are ordinary text files conaining the literature reference records in a pre-defined
#' format. BibTeX files can either be created manually, but for larger documents the
#' manual creation and maintenance of BibTeX files becomes untractable. This can be
#' remedied by using a reference manager such as Mendeley. Literature references
#' can be exported automatically from Mendeley into BibTeX files. The exported BibTeX
#' records go either to a given directory or to a specified file. The main purpose of
#' this tool is to read the exported BibTeX file from Mendeley and collect them into
#' a local BibTeX file.
#'
#' @export MendeleyExportToolR6
#' @usage MendeleyExportToolR6$new()
#' @return Object of \code{\link{R6Class}} to bridge the exported BibTeX references
#'         exported by Mendeley to be used in a RMarkdown document
#' @examples
#' \dontrun{
#' # instantiate a new MendeleyExportTool object
#' met <- rmddochelper::MendeleyExportToolR6$new()
#' # setting the current rmd-file
#' mrmt$set_this_rmd_file(ps_this_rmd_file = ifelse(rstudioapi::isAvailable(),
#'                               rstudioapi::getActiveDocumentContext()$path,
#'                               rprojroot::thisfile()))
#' # check whether mendeley export dir exists
#' met.exists_mendeley_export_dir()
#' # add a new reference to the local BibTeX file
#' met$add("Phocas1998")
#' }
#'
#' @field s_men_export_dir directory where mendeley exports its BibTeX records
#' @field s_local_bib_file name of local BibTeX file
#' @field s_this_rmd_file full path to current Rmarkdown source document
#'
#' @section Public Methods:
#' \describe{
#'   \item{\code{new(ps_men_export_dir, ps_local_bib_dir)}}{Instantiation-Constructor method}
#'   \item{\code{add(ps_new_ref, pb_is_rstudio_available)}}{Add a new reference with the key ps_new_ref}
#'   \item{\code{set_mendeley_export_dir(ps_men_export_dir)}}{Setter for s_men_export_dir}
#'   \item{\code{get_mendeley_export_dir()}}{Getter for s_men_export_dir}
#'   \item{\code{set_local_bib_file(ps_local_bib_file)}}{Setter for s_local_bib_file}
#'   \item{\code{get_local_bib_file()}}{Getter for s_local_bib_file}
#'   \item{\code{set_this_rmd_file(ps_this_rmd_file)}}{Setter for s_this_rmd_file}
#'   \item{\code{get_this_rmd_file()}}{Getter for s_this_rmd_file}
#' }
#'
#' @section Private Methods:
#' \describe{
#'   \item{\code{get_bib_from_mendeley_export(ps_new_ref)}}{Get BibTeX record based on citation key ps_new_ref}
#'   \item{\code{write_local_bib(pvec_bib_new_ref)}}{Adding pvec_bib_new_ref BibTeX record to the local bibliography file}
#'   \item{\code{convert_add_to_ref(ps_new_ref)}}{Convert the add() statement to the Rmarkdown reference}
#'   \item{\code{get_rmd_ref(ps_new_ref)}}{Get Rmarkdown reference from citation key ps_new_ref}
#' }
MendeleyExportToolR6 <- R6::R6Class(classname = "MendeleyExporter",
                                    public = list(
                                      initialize = function(ps_men_export_dir = getOption("rmddh.mendeley.export.dir"),
                                                            ps_local_bib_file = "bibliography.bib",
                                                            ps_this_rmd_file  = ifelse(rstudioapi::isAvailable(),
                                                                                       rstudioapi::getActiveDocumentContext()$path,
                                                                                       rprojroot::thisfile())){
                                        private$s_men_export_dir <- ps_men_export_dir
                                        private$s_local_bib_file <- ps_local_bib_file
                                        private$s_this_rmd_file <- ps_this_rmd_file
                                      },
                                      ### # setter and getter for s_men_export_dir
                                      set_mendeley_export_dir = function(ps_men_export_dir){private$s_men_export_dir <- ps_men_export_dir},
                                      get_mendeley_export_dir = function(){return(private$s_men_export_dir)},
                                      ### # setter and getter for s_local_bib_file
                                      set_local_bib_file = function(ps_local_bib_file){private$s_local_bib_file <- ps_local_bib_file},
                                      get_local_bib_file = function(){return(private$s_local_bib_file)},
                                      ### # setter and getter for s_this_rmd_file
                                      set_this_rmd_file = function(ps_this_rmd_file){private$s_this_rmd_file <- ps_this_rmd_file},
                                      get_this_rmd_file = function(){return(private$s_this_rmd_file)},
                                      ### # add a BibTeX-record to the local bib file
                                      add = function(ps_new_ref, pb_is_rstudio_available = rstudioapi::isAvailable()){
                                        ### # obtain ps_ref from mendeley export dir
                                        vec_bib_new_ref <- private$get_bib_from_mendeley_export(ps_new_ref)
                                        ### # write the new version of the local bib file
                                        private$write_local_bib(pvec_bib_new_ref = vec_bib_new_ref)
                                        ### # in case the path to the current rmd_file was set, we can convert the add command into a reference
                                        s_rmd_ref <- private$get_rmd_ref(ps_new_ref = ps_new_ref)
                                        if (!is.na(private$s_this_rmd_file) & pb_is_rstudio_available){
                                          private$convert_add_to_ref(ps_new_ref)
                                        }
                                        return(s_rmd_ref)
                                      }
                                    ),
                                    private = list(
                                      s_men_export_dir = NA,
                                      s_local_bib_file = NA,
                                      s_this_rmd_file = NA,
                                      ### # private functions
                                      ### # get the bib-record for ps_new_ref from mendeley export
                                      get_bib_from_mendeley_export = function(ps_new_ref){
                                        ### # check that we know were mendeley export dir is and that it exists
                                        if (!dir.exists(private$s_men_export_dir))
                                          stop("[ERROR -- get_bib_from_mendeley_export] Cannot find mendeley export directory: ", private$s_men_export_dir)
                                        ### # distinguish between all mendeley exports in one file or in separate files
                                        vec_mendeley_export_bib <- list.files(path = private$s_men_export_dir, pattern = "bib$")
                                        if ( length(vec_mendeley_export_bib) == 1 ){
                                          ### # all references are in one file
                                          s_all_ref <- RefManageR::ReadBib(file = file.path(private$s_men_export_dir,vec_mendeley_export_bib))
                                          s_new_ref <- s_all_ref[ps_new_ref]
                                        } else {
                                          ### # each reference has a separate file
                                          s_new_bib_file <- file.path(private$s_men_export_dir, paste0(ps_new_ref, ".bib"))
                                          if (!file.exists(s_new_bib_file))
                                            stop("[ERROR -- get_bib_from_mendeley_export] Cannot find bib file for reference: ", ps_new_ref)
                                          s_new_ref <- RefManageR::ReadBib(file = s_new_bib_file)
                                        }
                                        ### # check whether, we found a bib entry for ps_new_ref
                                        if (length(s_new_ref) == 0)
                                          stop("[ERROR -- get_bib_from_mendeley_export] Cannot find bib entry for reference: ", ps_new_ref)
                                        return(s_new_ref)
                                      },
                                      ### # write new version of local bib-file with pvec_bib_new_ref in it, if it is not already included
                                      write_local_bib = function(pvec_bib_new_ref){
                                        ### # read existing local references from s_local_bib_file, if it exists already
                                        if (file.exists(private$s_local_bib_file)) {
                                          vec_local_bib <- RefManageR::ReadBib(file = private$s_local_bib_file)
                                          ### # add pvec_bib_new_ref, if it is not yet included
                                          if ( length(vec_local_bib[names(pvec_bib_new_ref)]) == 0 )
                                            vec_local_bib <- c(vec_local_bib, pvec_bib_new_ref)
                                        } else {
                                          vec_local_bib <- pvec_bib_new_ref
                                        }
                                        ### # add new ref to local bib file
                                        RefManageR::WriteBib(vec_local_bib, file = private$s_local_bib_file)
                                        return(invisible(TRUE))
                                      },
                                      ### # comvert add command to reference
                                      convert_add_to_ref = function(ps_new_ref){
                                        ### # read rmd source file
                                        con_rmd <- file(description = private$s_this_rmd_file)
                                        vec_rmd <- readLines(con = con_rmd)
                                        close(con = con_rmd)
                                        ### # search for new reference
                                        s_search_pat <- paste0("`r ref$add(\"", ps_new_ref, "\")`")
                                        s_repl <- private$get_rmd_ref(ps_new_ref = ps_new_ref)
                                        ### # regex for prefix replacement
                                        s_pat_prefix <- '`r [[:alpha:]]+[$]add[(]["]'
                                        s_repl_prefix <- '[@'
                                        ### # regex for postfix replacement
                                        s_pat_postfix <- '["][)]`'
                                        s_repl_postfix <- ']'
                                        ### # get indices in text
                                        vec_found_idx <- grep(pattern = s_search_pat, vec_rmd, fixed = TRUE)
                                        ### # if new pattern was found more than once, stop here
                                        if (length(vec_found_idx) != 1) warning("[WARNING -- convert_add_to_ref] Multiple Occurences of ref: ", ps_new_ref)
                                        ### # replace pattern in rmd source vector vec_rmd
                                        l_scratch_result <- sapply(vec_found_idx,
                                                                   function(x)
                                                                     vec_rmd[x] <<- gsub(pattern = s_pat_postfix,
                                                                                         replacement = s_repl_postfix,
                                                                                         gsub(pattern = s_pat_prefix,
                                                                                         replacement = s_repl_prefix,
                                                                                         vec_rmd[x])))
                                        ### # write back vec_rmd to private$s_this_rmd_file
                                        cat(paste0(vec_rmd, collapse = "\n"), "\n", file = private$s_this_rmd_file)
                                        return(invisible(TRUE))
                                      },
                                      ### # generate the string for the reference in Rmarkdown
                                      get_rmd_ref = function(ps_new_ref){
                                        s_rmd_result <- paste0("[@", ps_new_ref, "]")
                                        return(s_rmd_result)
                                      }
                                    ))
