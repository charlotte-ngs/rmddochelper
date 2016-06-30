###
###
###
###
###   Purpose:   R6 Class representing table of abbreviation objects
###   started:   2016/06/16 (pvr)
###
### ################################################################# ###

#' @title R6 Class To Model Abbreviation Table Objects
#'
#' @description
#' Abbreviation tables are tables with two columns one of which
#' contains the abbreviations and the second column contains the
#' meaning of the abbreviations. The tables are specific for a
#' given document. The R6 class defined here provides functionlity
#' to automatically generate a table of abbreviation
#'
#' @export R6ClassTableAbbrev
R6ClassTableAbbrev <- R6::R6Class(classname = "R6ClassTableAbbrev",
                                  public    = list(
                                    setColHeader = function(pvColHeader){
                                      private$colHeader <- pvColHeader
                                    },
                                    setAbbrFile = function(psAbbrFile){
                                      private$sAbbrFile <- psAbbrFile
                                    },
                                    setAbbrTitle = function(psAbbrTitle){
                                      private$sAbbrTitle <- psAbbrTitle
                                    },
                                    add_abbrev = function(psAbbrev, psMeaning, pbquote = TRUE){
                                      ### # distinguish between first and later cases
                                      if (is.null(private$dfAbbrTable)){
                                        private$dfAbbrTable <- data.frame(abbr = psAbbrev,
                                                                          what = psMeaning,
                                                                          stringsAsFactors = FALSE)
                                      } else {
                                        private$dfAbbrTable <- rbind(private$dfAbbrTable,
                                                                     c(psAbbrev, psMeaning))
                                      }
                                      if (pbquote){
                                        sresult <- paste0("`", psMeaning, "`", " (", psAbbrev, ")")
                                      } else {
                                        sresult <- paste0(psMeaning, " (", psAbbrev, ")")
                                      }
                                      return(sresult)
                                    },
                                    writeToTsvFile = function(psFileName = NULL){
                                      ### # determine name of file to write to
                                      if (!is.null(psFileName)) {
                                        sFileName <- psFileName
                                      } else {
                                        sFileName <- private$sAbbrFile
                                      }
                                      ### # only write to file, if there private$dfAbbrTable is not null
                                      if (!is.null(private$dfAbbrTable)){
                                        names(private$dfAbbrTable) <- private$colHeader
                                        write.table(private$dfAbbrTable,
                                                    file = sFileName,
                                                    sep = "\t",
                                                    quote = FALSE,
                                                    row.names = FALSE,
                                                    fileEncoding = "UTF-8")

                                      }
                                    },
                                    include_abbr_table = function(psAbbrTitle = NULL){
                                      ### # determine title of section with abbreviation table
                                      if (!is.null(psAbbrTitle)){
                                        sAbbrTitle <- psAbbrTitle
                                      } else {
                                        sAbbrTitle <- private$sAbbrTitle
                                      }
                                      cat(sAbbrTitle, "\n", sep = "")
                                      if (!file.exists(private$sAbbrFile)){
                                        cat(" ==> Please, knit the document again to include a table of abbreviations\n")
                                      } else{
                                        dfAbbrTable <- read.table(file = private$sAbbrFile,
                                                                  header = TRUE,
                                                                  row.names = NULL,
                                                                  sep = "\t",
                                                                  stringsAsFactors = FALSE,
                                                                  fileEncoding = "UTF-8")
                                        knitr::kable(dfAbbrTable)
                                      }
                                    },
                                    is_empty_abbr = function(){
                                      return(is.null(private$dfAbbrTable))
                                    }
                                  ),
                                  private   = list(
                                    dfAbbrTable = NULL,
                                    colHeader = c("Abbreviation", "Meaning"),
                                    sAbbrFile = "ABBREVIATION",
                                    sAbbrTitle = "# Abbreviations"
                                  ))
