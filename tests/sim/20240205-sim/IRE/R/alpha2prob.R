#' Convert valid alpha array to probabilities under log-linear formulation
#'
#' @param A alpha array. Must be valid and provide identifiable log-linear
#' specification
#' @param J index List of variable interactions to include
#' @param I vector. Number of categories for each variable in sence
#'
#' @return an array of dimension I with probabilities
#' @export
#'


alpha2prob <- function(A, J, I){
  Jcheck(J)
  lenJ <- sapply(J, length)
  P <- array(dim = I)
  # all indicies of P array (by construction all values are NA)
  Pidx <- which(is.na(P), arr.ind = TRUE)
  for(i in 1:nrow(Pidx)){
    x <- Pidx[i, ]
    Px <- 0
    for(k in 1:max(lenJ)) {  # loop over different length of J
      for(j in which(lenJ == k)){
        Px <- Px + A[[j]][matrix(x[J[[j]]], nrow = 1)]
      }
    }
    P[matrix(x, nrow = 1)] <- exp(Px) / (1 + exp(Px))
  }
} # end alpha2prob()

