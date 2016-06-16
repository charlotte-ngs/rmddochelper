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
R6ClassTableAbbrev <- R6Class(classname = "R6ClassTableAbbrev",
                              public    = list(),
                              private   = list())
