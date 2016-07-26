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
#' object in a file. Given that requirement we must have methods for
#' reading status information from a file and for writing status information
#' to a file. Furthermore, we need a method to add a document status record
#' and we must be able to display all document status records as a table.
#'
#' @export R6ClassDocuStatus
#' @usage R6ClassDocuStatus$new()
#' @return Object of \code{\link{R6Class}} with methods for managing the status of a document.
#' @format \code{\link{R6Class}} object.
#' @examples
#' r6objDocuStat <- R6ClassDocuStatus$new()
#' r6obj_docstat$set_current_status(psVersion = "0.0.908",
#'                                  psStatus  = "New version of setting document status",
#'                                  psProject = "rmddochelper")
#' \dontrun{
#' r6objDocuStat$writeStatusToFile()
#' r6objDocuStat$knitr_kable()
#' }
#' @field version current version of the document
#' @field status_colnames vector of column names shown in the document table
#' @field status_history dataframe with the document history read from the history file
#' @field history_file name of the file containing the document history
#' @section Public Methods:
#' \describe{
#'   \item{\code{new()}}{This method instantiates an object of class R6ClassDocuStatus}
#'   \item{\code{initialize(psFormat)}}{Initialization after creating the instance. Reads history file if it exists}
#'   \item{\code{include_doc_stat(psTitle)}}{Saves updated document status to the status file.
#'               Write section header psTitle for document status and write
#'               markdown table containing the document status.}
#' }
#' @section Private Methods:
#' \describe{
#'   \item{\code{writeStatusToFile()}}{Writes current status and history
#'               to a tab-separated file. Tab-separated format is chosen, because TAB-characters
#'               are less likely to occur in any of the table fields. File encoding is set
#'               to "UTF-8" in order to preserve German Umlauts}
#'   \item{\code{readStatusFromFile()}}{Document status history is read
#'               from the history file.}
#'   \item{\code{readCsv2StatusFromFile()}}{Reading method for old csv2
#'               formatted status history files. This is mainly used for converting history
#'               files from old csv2 format to new tab-separated format.}
#'   \item{\code{knitr_kable}}{Complete status history is written to a table using
#'         \code{knitr::kable} to convert the dataframe into a markdown-table}
#'   \item{\code{auto_increment()}}{Autoincrementing minor version number}
#' }
R6ClassDocuStatus <- R6::R6Class(classname = "R6ClassDocuStatus",
                                 public    = list(
                                   initialize = function(psFormat = "tab") {
                                     'Initialisation of a new document status object.'
                                     if (psFormat == "csv2"){
                                       private$readCsv2StatusFromFile()
                                     } else {
                                       private$readStatusFromFile()
                                     }
                                   },
                                   set_current_status = function(psVersion = private$auto_increment(),
                                                                 psDate = as.character(Sys.Date()),
                                                                 psAuthor = Sys.info()[["user"]],
                                                                 psStatus = NULL,
                                                                 psProject = "NA"){
                                     ### # in case status is not null, add status, o/w do nothing
                                     if (!is.null(psStatus)) {
                                       dfCurStatus <- data.frame(Version = psVersion,
                                                                 Date = psDate,
                                                                 Author = psAuthor,
                                                                 Status = psStatus,
                                                                 Project = psProject,
                                                                 stringsAsFactors = FALSE)
                                       if (is.null(private$status_history)){
                                         private$status_history <- dfCurStatus
                                       } else {
                                         ### # in case psVersion already exists in private$status_history$Version,
                                         ### #  update other fields
                                         if (is.element(psVersion, private$status_history$Version)){
                                           nUpdateRow <- which(psVersion == private$status_history$Version)
                                           private$status_history[nUpdateRow,] <- dfCurStatus[1,]
                                         } else {
                                           private$status_history <- rbind(private$status_history,
                                                                           dfCurStatus,
                                                                           stringsAsFactors = FALSE)
                                         }

                                       }

                                     }

                                   },
                                   setStatusColnames = function(psStatusColnames){
                                     private$status_colnames <- psStatusColnames
                                   },
                                   getStatusColnames = function(){
                                     return(private$status_colnames)
                                   },
                                   setTitle = function(pstitle){
                                     private$stitle  <- pstitle
                                   },
                                   include_doc_stat = function(psTitle = NULL){
                                     ### # write complete status to file
                                     private$writeStatusToFile()
                                     ### # get the title in case it got changed
                                     if (!is.null(psTitle)){
                                       stitle <- psTitle
                                     } else {
                                       stitle <- private$stitle
                                     }
                                     ### # write title to document
                                     cat(stitle,"\n", sep = "")
                                     ### # convert status dataframe to a table
                                     private$knitr_kable()
                                   }
                                 ),
                                 private   = list(version         = "0.0.900",
                                                  status_colnames = c("Version", "Date", "Author","Status","Project"),
                                                  status_history  = NULL,
                                                  history_file    = "DOCUMENTSTATUS",
                                                  stitle          = "# Document Status",
                                                  get_version_col = function(pdfStatus){
                                                    return(which(tolower(names(pdfStatus)) == "version"))
                                                  },
                                                  auto_increment = function(){
                                                    if (is.null(private$status_history)){
                                                      sCurVersion <- private$version
                                                    } else {
                                                      sCurVersion <- private$status_history[nrow(private$status_history),"Version"]
                                                    }
                                                    vVersionComp <- as.numeric(unlist(strsplit(sCurVersion,
                                                                                    split = ".",
                                                                                    fixed = TRUE)))
                                                    vVersionComp[3] <- vVersionComp[3] + 1
                                                    return(paste(as.character(vVersionComp), collapse = "."))
                                                  },
                                                  writeStatusToFile = function(){
                                                    ### # write complete status history to history file
                                                    write.table(private$status_history,
                                                                file = private$history_file,
                                                                quote = FALSE,
                                                                sep = "\t",
                                                                row.names = FALSE,
                                                                fileEncoding = "UTF-8")
                                                  },
                                                  readStatusFromFile = function(){
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
                                                  readCsv2StatusFromFile = function(){
                                                    sFileName <- private$history_file
                                                    if (!file.exists(sFileName))
                                                      stop("CANNOT FIND Status file: ", sFileName)
                                                    private$status_history <- read.csv2(file = sFileName,
                                                                                        row.names = NULL,
                                                                                        stringsAsFactors = FALSE,
                                                                                        fileEncoding = "UTF-8")
                                                  },
                                                  knitr_kable = function(){
                                                    ### # in case different column titles are specified, we change it
                                                    dfDocStatus <- private$status_history
                                                    names(dfDocStatus) <- private$status_colnames
                                                    knitr::kable(dfDocStatus)
                                                  }))


