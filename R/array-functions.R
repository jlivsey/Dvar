# -----------------------------------------------------------------------------
#' Evaluate Array at Specified margin
#'
#' @param A array
#' @param mar list of vectors of specified margin
#'
#' @return Array compressed my mar
#' @export
#'
arrayEval <- function(A, mar){
  A[mar[[1]], mar[[2]], mar[[3]]]
}


# -----------------------------------------------------------------------------
#' Initalize array with zeros and ones
#'
#' @param arrayDim Dimension of output array
#' @param idx list of vectors specifying location of ones
#'
#' @return array of dimension arrayDim with ones at idx
#' @export
#'
arraySetOnes <- function(arrayDim, idx){
  Z <- array(0, arrayDim)
  Z[idx[[1]], idx[[2]], idx[[3]]] <- 1
  return(Z)
}


# -----------------------------------------------------------------------------
#' Convert zero entries of mar to full vector
#'
#' @param mar list of of margins
#' @param A full array used only for dimensionality alternatively pass mydim
#' @param mydim dimensionality of full array
#'
#' @return mar with zero replaced by full vector
#' @export
#'
marginZero2vec <- function(mar, mydim = NULL, A = NULL){
  if(class(mar) != 'list'){
    print('mar needs to be a list')
    break
  }
  if(!is.null(A)) mydim <- dim(A)
  zeroBool <- lapply(mar, function(x){x == 0})
  for(i in 1:length(zeroBool)){
    if(zeroBool[[i]][1]) {
      mar[[i]] <- 1:mydim[i]
    }
  }
  return(mar)
}
