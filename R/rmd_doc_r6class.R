###
###
###
###
###   Purpose:   R6 Class representing rmarkdown document objects
###   started:   2017/09/11 (pvr)
###
### ################################################################# ###

#' @title R6 Class Representing Rmarkdown Document Objects
#'
#' @docType class
#' @importFrom R6 R6Class
#' @description
#' Rmarkdown documents which include information that is
#' distributed over several files are ideally put into
#' a single parent directory. For organisational reasons,
#' it is easier to put the different files included in
#' a Rmarkdown document into different subfolders according
#' to their file-type. This structure is constant, except
#' for the name of the Rmarkdown file which can easily be
#' parameterized.
#' @details
#'
#' @export R6RmdDocInfo
#' @usage R6RmdDocInfo$new()
#' @return R6 Object of type \code{\link{R6RmdDocInfo}}
#' @examples
#' r6objDocInfo <- R6RmdDocInfo$new(psDocName = "MyFirstRmdDoc")
#' @field docName Name of the document project
#' @field rmdPath Path where rmd-files are stored
#' @field figPath Path where figure source files are stored
#' @field pngPath Path where figure files that should be included in the document are stored
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{new()}}{Instantiation of R6 object of class R6RmdDocInfo}
#'   \item{\code{setDocName(psDocName)}}{setter for document project name}
#'   \item{\code{getDocPath}}{getter for field docName}
#'   \item{\code{setRmdPath(psRmdPath)}}{setter for field rmdPath}
#'   \item{\code{getRmdPath}}{getter for field rmdPath}
#'   \item{\code{setFigPath(psFigPath)}}{setter for field figPath}
#'   \item{\code{getFigPath}}{getter for field figPath}
#'   \item{\code{setPngPath(psPngPath)}}{setter for field pngPath}
#'   \item{\code{getPngPath}}{getter for field pngPath}
#' }
#'
R6RmdDocInfo <- R6::R6Class(classname = "R6RmdDocInfo",
                            public    = list(
                              initialize = function(psDocName, psDocPath = ".", psRmdPath = "rmd", psFigPath = "odg"){
                                private$docName <- psDocName
                                private$docPath <- psDocPath
                                private$rmdPath <- psRmdPath
                                private$figPath <- psFigPath
                              },
                              setDocName = function(psDocName){
                                private$docName <- psDocName
                              },
                              getDocName = function(){
                                return(private$docName)
                              },
                              setDocPath = function(psDocPath){
                                private$docPath <- psDocPath
                              },
                              getDocPath = function(){
                                return(private$docPath)
                              },
                              setRmdPath = function(psRmdPath){
                                private$rmdPath <- psRmdPath
                              },
                              getRmdPath = function(){
                                return(private$rmdPath)
                              },
                              setFigPath = function(psFigPath){
                                private$figPath <- psFigPath
                              },
                              getFigPath = function(){
                                return(private$figPath)
                              },
                              setPngPath = function(psPngPath){
                                private$pngPath <- psPngPath
                              },
                              getPngPath = function(){
                                return(private$pngPath)
                              }
                            ),
                            private   = list(
                              docName = NULL,
                              docPath = ".",
                              rmdPath = "rmd",
                              figPath = "odg",
                              pngPath = "png"
                            ))
