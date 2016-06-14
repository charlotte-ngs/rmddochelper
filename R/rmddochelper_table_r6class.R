###
###
###
###   Purpose:   R6 Class representing Document status objects
###   started:   2016/04/18 (pvr)
###
### ############################################################ ###


#' @title R6 Class Representing Document Status Objects
#'
#' @docType class
#' @importFrom R6 R6Class
#' @description
#' \code{R6ClassDocuStatus} objects can be used to represent the status
#' of a given document. A core requirement is that the different status
#' records should be persistent across different compilation runs. That
#' makes it necessary to store the intermediate states of an \code{R6ClassDocuStatus}
#' object in a file. The requirement of a persistent document status history
#' is implemented in private methods. These method create a small history
#' management system that can read the status history from a history file and
#' that is able to write the updated document status to a history
#' file. The only public method is the one that creates the markdown table in
#' a document. Before that table is created the private history management functions
#' are called and the complete status information of the document is collected
#' from the history file and from the current document status.
#'
#' @export R6ClassDocuStatus
#' @usage R6ClassDocuStatus$new()
#' @return Object of \code{\link{R6Class}} with methods for managing the status of a document.
#' @format \code{\link{R6Class}} object.
#' @examples
#' r6objDocuStat <- R6ClassDocuStatus$new()
#' r6objDocuStat$setProject(psProject = "DEXSeq")
#' r6objDocuStat$setVersion(psVersion = "0.0.900")
#' r6objDocuStat$setDate(psDate = "31.03.2016")
#' r6objDocuStat$setAuthor(psAuthor = "pvr")
#' r6objDocuStat$setStatus(psStatus = "Init")
#' \dontrun{
#' r6objDocuStat$writeStatusToFile()
#' r6objDocuStat$knitr_kable()
#' }
#' @field version current version of the document
#' @field author author of the change leading to the current status of the document
#' @field date date of the current change
#' @field status description of document status
#' @field project project this document belongs to
#' @field status_colnames vector of column names shown in the document table
#' @field status_history dataframe with the document history read from the history file
#' @field history_file name of the file containing the document history
#' @section Public Methods:
#' \describe{
#'   \item{\code{new()}}{This method instantiates an object of class R6ClassDocuStatus}
#'   \item{\code{initialize()}}{Initialization of field called after creating the instance}
#'   \item{\code{include_doc_stat(psTitle = "Document Status", psFormat = "tab")}}
#'              {In case a document status history file is found, the document status
#'               history is read from the history file and is assigend to a dataframe.
#'               The current status is added to the status history and is written back
#'               to the status history file. Then the document status section is written
#'               as a markdown table to the document from where the method is called. This
#'               is done using the function knitr::kable().}
#' }
#' @section Private Methods:
#' \describe{
#'   \item{\code{writeStatusToFile(psFileName = NULL)}}{Writes current status and history
#'               to a tab-separated file. Tab-separated format is chosen, because TAB-characters
#'               are less likely to occur in any of the table fields. If argument
#'               psFileName is not null the name of the output file is set to psFileName,
#'               otherwise the value in field history_file is used. File encoding is set
#'               to "UTF-8" in order to preserve German Umlauts}
#'   \item{\code{readStatusFromFile(psFileName = NULL)}}{Document status history is read
#'               from the history file. The name of the
#'               history file is either taken from the method argument psFileName or from
#'               the object field history_file.}
#'   \item{\code{readCsv2StatusFromFile(psFileName = NULL)}}{Reading method for old csv2
#'               formatted status history files. This is mainly used for converting history
#'               files from old csv2 format to new tab-separated format.}
#'   \item{\code{knitr_kable}}{Add current document status info to document history and
#'               convert it to a dataframe. Then use \code{knitr::kable} to convert the
#'               dataframe into a markdown-table}
#' }
R6ClassDocuStatus <- R6::R6Class(classname = "R6ClassDocuStatus",
                                 public    = list(
                                   initialize = function(){
                                     'Initialisation of a new document status object.'
                                     ### # initialize date and author based on Sys functionis
                                     if (is.null(private$date))
                                       self$setDate(psDate = as.character(Sys.Date()))
                                     if (is.null(private$author))
                                       self$setAuthor(psAuthor = Sys.info()[["user"]])

                                   },
                                   setVersion = function(psVersion){
                                     private$version <- psVersion
                                   },
                                   getVersion = function(){
                                     return(private$version)
                                   },
                                   setDate = function(psDate){
                                     private$date <- psDate
                                   },
                                   getDate = function(){
                                     return(private$date)
                                   },
                                   setAuthor = function(psAuthor){
                                     private$author <- psAuthor
                                   },
                                   getAuthor = function(){
                                     return(private$author)
                                   },
                                   setStatus = function(psStatus){
                                     private$status = psStatus
                                   },
                                   getStatus = function(){
                                     return(private$status)
                                   },
                                   setProject = function(psProject){
                                     private$project <- psProject
                                   },
                                   getProject = function(){
                                     return(private$project)
                                   },
                                   setStatusColnames = function(psStatusColnames){
                                     private$status_colnames <- psStatusColnames
                                   },
                                   getStatusColnames = function(){
                                     return(private$status_colnames)
                                   },
                                   include_doc_stat = function(psTitle = "Document Status", psFormat = "tab"){
                                     ### # read status history, if it exists
                                     if (file.exists(private$history_file)){
                                       if (psFormat == "csv2"){
                                         private$readCsv2StatusFromFile()
                                       } else {
                                         private$readStatusFromFile()
                                       }
                                     }
                                     ### # write current status plus history to file
                                     private$writeStatusToFile()
                                     cat(paste("#", psTitle),"\n")
                                     private$knitr_kable()
                                   }
                                 ),
                                 private   = list(version = "0.0.900",
                                                  author = NULL,
                                                  date   = NULL,
                                                  status = "Init",
                                                  project = "NA",
                                                  status_colnames = c("Version", "Date", "Author","Status","Project"),
                                                  status_history = NULL,
                                                  history_file = "DOCUMENTSTATUS",
                                                  stat_to_df = function() {
                                                    dfCurStatus <- data.frame(version = private$version,
                                                                              date    = private$date,
                                                                              author  = private$author,
                                                                              status  = private$status,
                                                                              project = private$project,
                                                                              stringsAsFactors = FALSE)
                                                    if (!is.null(private$status_history)){
                                                      ### # check whether version number already exists
                                                      cur_stat_col <- which(dfCurStatus$version == private$status_history$version)
                                                      if (length(cur_stat_col) > 0) {
                                                        dfCurStatus <- rbind(private$status_history[-cur_stat_col,], dfCurStatus)
                                                      } else {
                                                        if (!is.null(private$status_history))
                                                          dfCurStatus <- rbind(private$status_history, dfCurStatus)
                                                      }
                                                    }
                                                    return(dfCurStatus)
                                                  },
                                                  writeStatusToFile = function(psFileName = NULL){
                                                    ### # convert status to a dataframe and write it to the history file
                                                    dfCurStatus <- private$stat_to_df()
                                                    names(dfCurStatus) <- private$status_colnames
                                                    write.table(dfCurStatus, file = private$history_file,
                                                                quote = FALSE,
                                                                sep = "\t",
                                                                row.names = FALSE,
                                                                fileEncoding = "UTF-8")
                                                  },
                                                  readStatusFromFile = function(psFileName = NULL){
                                                    sFileName <- psFileName
                                                    if (is.null(psFileName))
                                                      sFileName <- private$history_file
                                                    if (!file.exists(sFileName))
                                                      stop("CANNOT FIND Status file: ", sFileName)
                                                    private$status_history <- read.table(file = sFileName,
                                                                                         header = TRUE,
                                                                                         row.names = NULL,
                                                                                         sep = "\t",
                                                                                         stringsAsFactors = FALSE,
                                                                                         fileEncoding = "UTF-8")
                                                  },
                                                  readCsv2StatusFromFile = function(psFileName = NULL){
                                                    sFileName <- psFileName
                                                    if (is.null(psFileName))
                                                      sFileName <- private$history_file
                                                    if (!file.exists(sFileName))
                                                      stop("CANNOT FIND Status file: ", sFileName)
                                                    private$status_history <- read.csv2(file = sFileName,
                                                                                        row.names = NULL,
                                                                                        stringsAsFactors = FALSE,
                                                                                        fileEncoding = "UTF-8")
                                                  },
                                                  knitr_kable = function(){
                                                    dfCurStatus <- private$stat_to_df()
                                                    knitr::kable(dfCurStatus)
                                                  }))
