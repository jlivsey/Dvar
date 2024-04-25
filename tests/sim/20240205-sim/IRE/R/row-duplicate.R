#' Duplicated Row Index Location
#' Fing the index B's rows from matrix A
#'
#' @param A rows you wish to locate in B
#' @param B larger matrix that contains rows from A
#'
#' @return logical of length nrow(B)
#' @export
#'

duplicated_rows <- function(A, B){
  AB <- rbind(A, B)
  d  <- duplicated(AB)
  outIdx <- d[(nrow(A) + 1):nrow(AB)]
  return(outIdx)
}
