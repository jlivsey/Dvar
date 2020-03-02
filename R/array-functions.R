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
  A[mar[[1]], mar[[2]], mar[[3]], mar[[4]], mar[[5]], mar[[6]]]
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
    stop('mar needs to be a list')
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



# -----------------------------------------------------------------------------
#' Recode or condense a marginal of histogram
#'
#' @param A array to be recoded
#' @param margin margin that will be recoded
#' @param newCode list of vectors that specify the recode
#'
#' @return recoded array
#' @export
#'
recode <- function(A, margin, newCode){
  d <- dim(A)
  dl <- length(dim(A))
  # dimension of recoded array
  newd <- d
  newd[margin] <- length(newCode)

  # initialize empty mar object
  mar <- vector(mode = 'list', length = dl)
  for(i in 1:dl) mar[[i]] <- 1:d[i] # will change mar[[margin]] later

  # loop over all newCodes
  Aout <- array(NA, dim = newd)
  for(i in 1:length(newCode)){
    # create margin object for i^th newCode
    mar[[margin]] <- newCode[[i]]

    # WILL NEED TO BE CHANGED WHEN GENERALIZING TO MULTIPLE DIMENSIONS
    # DON'T KNOW HOW TO HANDLE LHS OF THIS ASSIGNEMENT IN GENERAL.
    Aout[,,,,,i] <- apply(arrayEval(A, mar), (1:dl)[-margin], sum)
  }
  return(Aout)
}
















