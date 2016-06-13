###
###
###
###   Purpose:   R6 Class representing Document status objects
###   started:   2016/04/18 (pvr)
###
### ############################################################ ###


#' @title R6 Class Representing Version Objects
#'
#' @description
#' Version strings can be separated into three parts
#' \enumerate{
#'   \item major version
#'   \item minor version
#'   \item fix level
#' }
#' For each of the three parts a private field is created
#'
#' @export R6ClassVersion
R6ClassVersion <- R6::R6Class(classname = "R6ClassVersion",
                              public = list(
                                initialize = function(pnMajorVersion = NA,
                                                      pnMinorVersion = NA,
                                                      pnFixLevel = NA){
                                  ### # by default we start with 0.0.900
                                  private$nMajorVersion <- 0
                                  if (!is.na(pnMajorVersion))
                                    private$nMajorVersion <- pnMajorVersion
                                  ### # minor version
                                  private$nMinorVersion <- 0
                                  if (!is.na(pnMinorVersion))
                                    private$nMinorVersion <- pnMinorVersion
                                  ### # fix level
                                  private$nFixLevel <- 900
                                  if (!is.na(pnFixLevel))
                                    private$nFixLevel <- pnFixLevel
                                },
                                incrementFixLevel = function(){
                                  private$nFixLevel <- private$nFixLevel + 1
                                },
                                incrementMinorVersion = function(){
                                  private$nMinorVersion <- private$nMinorVersion + 1
                                  private$nFixLevel <- 0
                                },
                                incrementMajorVersion = function(){
                                  private$nMajorVersion <- private$nMajorVersion + 1
                                  private$nMinorVersion <- 0
                                  private$nFixLevel <- 0
                                },
                                to_string = function(){
                                  return(paste(c(private$nMajorVersion, private$nMinorVersion, private$nFixLevel),
                                               sep = "", collapse = "."))
                                },
                                string_parse = function(psVersionString){
                                  sVersionString <- unlist(strsplit(psVersionString, split = ".", fixed = TRUE))
                                  stopifnot(identical(length(sVersionString),3L))
                                  private$nMajorVersion <- as.numeric(sVersionString[1])
                                  private$nMinorVersion <- as.numeric(sVersionString[2])
                                  private$nFixLevel <- as.numeric(sVersionString[3])
                                }
                              ),
                              private = list(
                                nMajorVersion = 0,
                                nMinorVersion = 0,
                                nFixLevel     = 0
                              ))


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
#'   \item{\code{include_doc_stat(psTitle)}}{Saves updated document status to the status file.
#'               Write section header psTitle for document status and write
#'               markdown table containing the document status.}
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
                                   include_doc_stat = function(psTitle = "Document Status",psFormat = "tab"){
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


#' R6Class Representing A Generic Table Object
#'
#'
R6ClassGenericTable <- R6::R6Class(classname = "R6ClassGenericTable",
                                   public    = list(
                                     setTableHeader = function(psTableHeader){
                                       private$sTableHeader <- psTableHeader
                                     },
                                     getTableHeader = function(){
                                       return(private$sTableHeader)
                                     },
                                     setTableBody = function(plTableBody){
                                       private$lTableBody <- plTableBody
                                     },
                                     getTableBody = function(){
                                       return(private$lTableBody)
                                     },
                                     addRow = function(plTableRow){
                                       private$lTableBody <- c(private$lTableBody, list(plTableRow))
                                     },
                                     writeTsvToFile = function(psFileName, pbRowNames = FALSE,
                                                               pbColNames = FALSE){
                                       dfTable <- as.data.frame(private$lTableBody)
                                       colnames(dfTable) <- private$sTableHeader
                                       write.table(dfTable, file = psFileName,
                                                   quote = FALSE, sep = "\t",
                                                   row.names = pbRowNames, col.names = pbColNames)
                                     }
                                   ),
                                   private   = list(sTableHeader = "",
                                                    lTableBody   = list()))


