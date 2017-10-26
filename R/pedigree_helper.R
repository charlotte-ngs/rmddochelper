#' Determine parent row in demo of Meuwissen-Luo algorithm
#'
#' Given a pedigree the demo of determining the relationship
#' matrix using Meuwissen-Luo writes the parents above the
#' label-row of the relationship matrix
#'
#' @param pPed                 the pedigree
#' @param pbShowBothNaParents  flag whether to show NA parents when both parents are unknown
#' @return   a character vector with the same length as the pedigree containing the parents
#' @export sGetParentRow
sGetParentRow <- function(pPed, pbShowBothNaParents = TRUE){
  return(sapply(1:length(pPed@label),
         function(x)
           ifelse(is.na(pPed@sire[x]) & is.na(pPed@dam[x]) & !pbShowBothNaParents,
                  "",
                  paste(ifelse(is.na(pPed@sire[x]),"NA",as.character(pPed@sire[x])),
                        ifelse(is.na(pPed@dam[x]),"NA",as.character(pPed@dam[x])),
                        sep=" - ")
                  )
         )
  )
}

