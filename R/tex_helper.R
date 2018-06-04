#' @title Convert matrix to LaTeX array
#'
#' @description
#' The matrix specified by pmatAMatrix is converted to a LaTeX array
#' using function \code{xtable::xtable}. The function \code{xtable::xtable}
#' produces a tabular LaTeX-object which is converted to a LaTeX-array using
#' simple string replacement of the LaTeX environment specifiers. Some
#' lines at the beginning and at the end are ignored. The number
#' of lines that are ignored can be specified using the parameters
#' pnOutStartLine and pnEndIgnoreLines.
#'
#' @details
#' Because in R vectors and matrices are different objects, when we want
#' to use this function for a vector, it has to be converted to a matrix
#' first.
#'
#' @param  pmatAMatrix        Matrix to be represented in tex format
#' @param  pnOutStartLine     line index where output should start, default = 5
#' @param  pnEndIgnoreLines   number of lines to be ignored at the end of the output, default = 1
#' @param  pnDigits           specify the number of digits to be used
#' @param  pnAlign            character vector denoting column alignment
#' @return string containing tex representation of matrix
#' @export sConvertMatrixToLaTexArray
sConvertMatrixToLaTexArray <- function(pmatAMatrix, pnOutStartLine = 5, pnEndIgnoreLines = 1, pnDigits = 2, pnAlign = NULL) {
  sResultTexMatrix <- capture.output(print(xtable::xtable(pmatAMatrix, align = pnAlign, digits = pnDigits),
                                           include.rownames = FALSE,
                                           include.colnames = FALSE,
                                           hline.after = NULL,
                                           sanitize.text.function=identity))
  ### # do some replacements
  sResultTexMatrix <- gsub("tabular", "array",
                           sResultTexMatrix[pnOutStartLine:(length(sResultTexMatrix)-pnEndIgnoreLines)],
                           fixed = TRUE)
  return(sResultTexMatrix)
}


#' @title Convert vector to LaTeX array
#'
#' @description
#' The specified vector is converted to a LaTeX array. We first
#' put the vector into a one column matrix and use sConvertMatrixToLaTexArray
#' on this one-column matrix
#'
#' @param pvec_avector Vector to be converted
#' @param pnDigits           specify the number of digits to be used
#' @return vector converted to LaTeX-array
#' @export sConvertVectorToLaTexArray
sConvertVectorToLaTexArray <- function(pvec_avector, pnDigits = 2){
  mat_from_vec <- matrix(pvec_avector, ncol = 1)
  return(sConvertMatrixToLaTexArray(pmatAMatrix = mat_from_vec, pnDigits = pnDigits))
}


#' @title Convert row vector to LaTeX array
#'
#' @description
#' We are given a vector and it should be printed as a row vector
#'
#' @param pvec_avector Vector to be converted
#' @param pnDigits           specify the number of digits to be used
#' @return row-vector converted to LaTeX-array
#' @export sConvertRowVectorToLaTexArray
sConvertRowVectorToLaTexArray <- function(pvec_avector, pnDigits = 2){
  mat_from_vec <- matrix(pvec_avector, nrow = 1)
  return(sConvertMatrixToLaTexArray(pmatAMatrix = mat_from_vec, pnDigits = pnDigits))

}


#' @title Matrix of strings from base-element and column and row indices
#'
#' @description
#' Given a symbolic Matrix M with a certain dimension, we want to specify
#' the components of M based on a given prefix of the components and the
#' column and row indices of each component.
#'
#' @details
#' The result can either be in raw format Mij or in LaTeX-Math notation M_{ij}.
#' The type of result is specified by the option psResultFmt. The only options
#' that are currently recogized are latex and raw. If anything else is specified
#' as format, it is raw per default.
#'
#'
#' @param psBaseElement   character prefix shown in front of indices
#' @param pnNrRow         number of rows
#' @param pnNrCol         number of columns
#' @param psResultFmt     format of result, currently only raw or latex is considered
#' @return Matrix of strings containing the components
#' @export matGetMatElem
matGetMatElem <- function(psBaseElement, pnNrRow, pnNrCol, psResult = "raw"){
  if (tolower(psResult) == "latex"){
    pastefun <- function(x,y,psPrefix){
      return(paste0(psPrefix, "_{", x%/%y+1, x%%y+1,"}"))
    }
  } else {
    pastefun <- function(x,y,psPrefix){
      return(paste0(psPrefix, x%/%y+1, x%%y+1))
    }
  }
  return(matrix(sapply(0:(pnNrRow*pnNrCol-1),
                       pastefun,
                       pnNrCol,
                       psBaseElement),
                nrow = pnNrRow,
                ncol = pnNrCol,
                byrow = TRUE))
}

#' Lower triangular matrix from a base element
#'
#' @param psBaseElement  constant prefix of each matrix element
#' @param pnNrRow        number of rows
#' @param pnNrCol        number of columns
#' @param pvecDiag       vector specifying diagonal elements
#' @export matLowerTri
matLowerTri <- function(psBaseElement, pnNrRow, pnNrCol, pvecDiag = NULL) {
  matResult <- matGetMatElem(psBaseElement = psBaseElement, pnNrRow = pnNrRow, pnNrCol = pnNrCol)
  matResult[upper.tri(matResult)] <- "0"
  if (!is.null(pvecDiag)) diag(matResult) <- pvecDiag
  return(matResult)
}

#' Diagonal matrix
#'
#' @param psBaseElement constant prefix of each matrix element
#' @param pnNrRow       number of rows
#' @param pnNrCol       number of columns
#' @export matDiag
matDiag <- function(psBaseElement, pnNrRow, pnNrCol) {
  matResult <- matGetMatElem(psBaseElement = psBaseElement, pnNrRow = pnNrRow, pnNrCol = pnNrCol)
  matResult[upper.tri(matResult) | lower.tri(matResult)]  <- "0"
  return(matResult)
}

#' Vector with base elements and index
#'
#' The result is a vector of elements that have psBaseElement
#' as prefix and that have the index of the corresponding
#' vector element as suffix. The result may be in raw format
#' or in LaTeX-Math format. The latter is produced when
#' argument psResult is set to "latex".
#'
#' @param psBaseElement   suffix of the vector elements
#' @param pnVecLen        number of elements in the vector
#' @param psResult        format of result (default = "raw")
#' @return Vector with string elements that each have psBaseElement as
#'         prefix and the index as suffix, separated by psSepChar
#' @export vecGetVecElem
vecGetVecElem <- function(psBaseElement, pnVecLen, psResult = "raw"){
  if (tolower(psResult) == "latex"){
    pastefun <- function(x, psPrefix)
      return(paste0(psPrefix, "_{", x, "}"))
  } else {
    pastefun <- function(x,psPrefix){
      return(paste0(psPrefix, x))
    }
  }
  return(sapply(1:pnVecLen, pastefun, psBaseElement))
}
