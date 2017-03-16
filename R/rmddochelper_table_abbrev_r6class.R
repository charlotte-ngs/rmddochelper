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
#' @docType class
#' @importFrom R6 R6Class
#' @description
#' Abbreviation tables are tables with two columns one of which
#' contains the abbreviations and the second column contains the
#' meaning of the abbreviations. The tables are specific for a
#' given document. The R6 class defined here provides functionlity
#' to automatically collect all abbreviations together with the
#' associated meanings and to generate the resulting table of
#' abbreviations. Method \code{add_abbrev} is used to add new
#' abbreviations. At the end of a document the list of collected
#' abbreviations together with their meanings is written to a file.
#' This file is the source for automatically generating the table
#' of abbreviations. Since, we want to allow for including the
#' table of abbreviations at any position within the document,
#' two compilation runs are required when new abbreviations are
#' added to the list of abbreviations. The first run collects
#' all abbreviations and writes them to the file and the second
#' run is used to generate the table of abbreviations.
#'
#' @export R6ClassTableAbbrev
#' @usage R6ClassTableAbbrev$new()
#' @return R6 Object of type \code{\link{R6ClassTableAbbrev}} with
#' fields and methods to collect abbreviations and generate a table
#' of abbreviations
#' @examples
#' r6objAbr <- R6ClassTableAbbrev$new()
#' r6objAbr$setAbbrFile(psAbbrFile = "ABBREVIATIONS")
#' r6objAbr$add_abbrev(psAbbrev = "Abr", psMeaning = "Abbreviation")
#' \dontrun{
#' r6objAbr$writeToTsvFile()
#' r6objAbr$include_abbr_table()
#' }
#' @field dfAbbrTable dataframe with abbreviations
#' @field colHeader column header for table of abbreviations
#' @field sAbbrFile name of the file where abbreviations are written to
#' @field sAbbrTitle title to be used in document above table of abbreviations
#' @field bQuote flag whether to put text in quotes
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{new()}}{Instantiation of R6 object of class R6ClassTableAbbrev}
#'   \item{\code{setColHeader(pvColHeader)}}{setter for column header}
#'   \item{\code{setAbbrFile}}{setter for abbreviation file}
#'   \item{\code{setAbbrTitle}}{setter for abbreviation title}
#'   \item{\code{setQuote}}{setter for bQuote flag}
#'   \item{\code{add_abbrev(psAbbrev,psMeaning,psShowText=NULL,pbQuote=NULL,pbOut=TRUE)}}{add new pair of abbreviation(psAbbrev) and meaning(psMeaning) to list of abbreviations. In case psShowText is not null, use it to be shown in the text, otherwise use psMeaning in the text. Flag pbQuote determines whether abbreviation and meaning are enclosed in quotes. Flag pbOut determines whether abbreviation and meaning should be written to Rmarkdown source file.}
#'   \item{\code{writeToTsvFile}}{Write list of abbreviations in tab-separated format to file}
#'   \item{\code{include_abbr_table}}{Include table of abbreviations in a document}
#'   \item{\code{is_empty_abbr}}{Check whether list of abbreviations is empty}
#' }
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
                                    setQuote = function(pbQuote){
                                      private$bQuote <- pbQuote
                                    },
                                    add_abbrev = function(psAbbrev, psMeaning, psShowText = NULL,
                                                          pbQuote = NULL, pbOut = TRUE){
                                      ### # in case psShowText != NULL use it in the result
                                      if (is.null(psShowText)) {
                                        sShowText <- psMeaning
                                      } else {
                                        sShowText <- psShowText
                                      }
                                      ### # determine flag for quotation of text
                                      if (!is.null(pbQuote)){
                                        bQuote <- pbQuote
                                      } else {
                                        bQuote <- private$bQuote
                                      }
                                      ### # distinguish between first and later cases
                                      if (is.null(private$dfAbbrTable)){
                                        private$dfAbbrTable <- data.frame(abbr = psAbbrev,
                                                                          what = psMeaning,
                                                                          stringsAsFactors = FALSE)
                                      } else {
                                        private$dfAbbrTable <- rbind(private$dfAbbrTable,
                                                                     c(psAbbrev, psMeaning))
                                      }
                                      ### # depending on quotation flag insert quotes
                                      if (bQuote){
                                        sresult <- paste0("`", sShowText, "`", " (", psAbbrev, ")")
                                      } else {
                                        sresult <- paste0(sShowText, " (", psAbbrev, ")")
                                      }
                                      ### # depending on whether abbreviation and meaning should appear in
                                      ### #  output
                                      if (pbOut) {
                                        return(sresult)
                                      } else {
                                        return(invisible(NULL))
                                      }

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
                                    sAbbrTitle = "# Abbreviations",
                                    bQuote = TRUE
                                  ))
