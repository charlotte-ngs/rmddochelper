###
###
###
###   Purpose:   R6 Class representation of counter objects
###   started:   2017-12-18 (pvr)
###
### ########################################################## ###

#' @title R6 Class representing object counts in documents
#'
#' @docType class
#' @importFrom R6 R6Class
#' @description
#' Simple counter of objects inside a document. Objects that
#' associated to such a counter can be sections, tables,
#' diagrams and others. Specialised counter classes for the
#' different document objects can be derived from this
#' more generic counter class. Specific document objects
#' that are associated to a counter are generated using
#' the prefix field in this class.
#'
#' @export R6ClassCount
#'
#' @usage R6ClassCount$new()
#'
#' @examples
#' cnt <- R6ClassCount$new()
#' cat(cnt$out(ps_prefix = "## Task", ps_suffix="Variance components", pn_count = 1))
#'
#' @field prefix Text shown before counter, specifies the document object type
#' @field suffix Test shown after the counter, corresponds to a description
#' @field count current value of the counter
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{new()}}{Instantiation of a new R6ClassCount object}
#'   \item{\code{out(ps_prefix, ps_suffix, pn_count)}}{Write count value with given prefix and suffix}
#'   \item{\code{increment_count()}}{Increment value of count by 1}
#'   \item{\code{set_prefix(ps_prefix)}}{Setter for field prefix}
#'   \item{\code{get_prefix()}}{Getter for field prefix}
#'   \item{\code{set_suffix(ps_suffix)}}{Setter for field suffix}
#'   \item{\code{get_suffix()}}{Getter for field suffix}
#'   \item{\code{set_count(pn_count)}}{Setter for field count}
#'   \item{\code{get_count()}}{Getter for field count}
#' }
R6ClassCount <- R6::R6Class(classname = "R6classCount",
                            public  = list(
                              initialize = function(){
                                private$n_count = 0
                              },
                              out = function(ps_prefix = NULL,
                                             ps_suffix = NULL,
                                             pn_count = NULL){
                                if (is.null(ps_prefix)){
                                  s_prefix <- private$s_prefix
                                } else {
                                  s_prefix <- ps_prefix
                                }
                                if (is.null(ps_suffix)){
                                  s_suffix <- private$s_suffix
                                } else {
                                  s_suffix <- ps_suffix
                                }
                                if (is.null(pn_count)){
                                  self$increment_count()
                                  n_count <- private$n_count
                                } else {
                                  n_count <- pn_count
                                }
                                return(paste(s_prefix, n_count, s_suffix, collapse = " "))
                              },
                              increment_count = function(){
                                private$n_count <- private$n_count + 1
                              },
                              set_prefix = function(ps_prefix){
                                private$s_prefix <- ps_prefix
                              },
                              get_prefix = function(){
                                return(private$s_prefix)
                              },
                              set_suffix = function(ps_suffix){
                                private$s_suffix <- ps_suffix
                              },
                              get_suffix = function(){
                                return(private$s_suffix)
                              },
                              set_count = function(pn_count){
                                private$n_count <- pn_count
                              },
                              get_count = function(){
                                return(private$n_count)
                              }
                            ),
                            private = list(
                              s_prefix = NULL,
                              s_suffix = NULL,
                              n_count = 0
                            ))



