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
                                    initialize = function(){
                                      private$colHeader <- c("Abbreviation", "Meaning")
                                      private$sAbbrFile <- "ABBREVIATION"
                                      private$sAbbrTitle <- "# Abbreviations"
                                    },
                                    setColHeader = function(pvColHeader){
                                      private$colHeader <- pvColHeader
                                    },
                                    setAbbrFile = function(psAbbrFile){
                                      private$sAbbrFile <- psAbbrFile
                                    },
                                    setAbbrTitle = function(psAbbrTitle){
                                      private$sAbbrTitle <- psAbbrTitle
                                    },
                                    add_abbrev = function(psAbbrev, psMeaning){
                                      ### # distinguish between first and later cases
                                      if (is.null(private$dfAbbrTable)){
                                        private$dfAbbrTable <- data.frame(abbr = psAbbrev,
                                                                          what = psMeaning,
                                                                          stringsAsFactors = FALSE)
                                      } else {
                                        private$dfAbbrTable <- rbind(private$dfAbbrTable,
                                                                     c(psAbbrev, psMeaning))
                                      }
                                      cat(psMeaning, "(", psAbbrev, ")", sep = "")
                                    },
                                    writeToTsvFile = function(psFileName = "ABBREVIATION"){
                                      sFileName <-psFileName
                                      if (!is.null(private$sAbbrFile)) {
                                        sFileName <- private$sAbbrFile
                                      }
                                      names(private$dfAbbrTable) <- private$colHeader
                                      write.table(private$dfAbbrTable,
                                                  file = sFileName,
                                                  sep = "\t",
                                                  quote = FALSE,
                                                  row.names = FALSE,
                                                  fileEncoding = "UTF-8")
                                    },
                                    include_abbr_table = function(){
                                      cat(private$sAbbrTitle, "\n", sep = "")
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
                                    }
                                  ),
                                  private   = list(
                                    dfAbbrTable = NULL,
                                    colHeader = NULL,
                                    sAbbrFile = NULL,
                                    sAbbrTitle = NULL
                                  ))
