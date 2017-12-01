###
###
###
###   Purpose:   Class representing enumeration objects
###   started:   2017-11-08 (pvr)
###
### ###################################################### ###

#' @title R6 Class representing enumerations
#'
#' @docType class
#' @importFrom R6 R6Class
#' @description
#' Simple R6 class allowing to include enumeration objects
#' in documents. The document formats must allow for R-code-junks
#' to be executed, as e.g. with rmarkdown or rnoweb documents.
#' The enumeration objects are initialized at the beginning
#' of the document and whenever an enumeration object is needed
#' an inline R-statement is used to generate the required enumeration
#' instance.
#'
#' @export R6ClassEnum
#' @usage R6ClassEnum$new()
#' @examples
#' # generate a new enumeration object
#' en <- rmddochelper::R6ClassEnum$new()
#' # increment number
#' en$incrCount()
#' # output of count
#' en$out()
#' @field count enumeration count
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{new()}}{Instantiation of enumeration object}
#'   \item{\code{setCount(pnCount)}}{setter for count field}
#'   \item{\code{getCount()}}{getter for count field}
#'   \item{\code{incrCount()}}{increment count by one}
#'   \item{\code{incrCountByN(pnIncr)}}{increment count by pnIncr}
#'   \item{\code{out()}}{output count}
#' }
#'
R6ClassEnum <- R6::R6Class(classname = "R6ClassEnum",
                           public = list(
                             setCount = function(pnCount){
                               private$count <- pnCount
                             },
                             getCount = function(){
                               return(private$count)
                             },
                             incrCountByN = function(pnIncr){
                               private$count <- private$count + pnIncr
                             },
                             incrCount = function(){
                               self$incrCountByN(pnIncr = 1)
                             },
                             out = function(){
                               cat(private$count)
                             }
                           ),
                           private = list(
                             count = 0
                           ))
