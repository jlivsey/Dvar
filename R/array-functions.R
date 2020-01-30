
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



#' Convert Null to full dimension vector
#'
#' @param mar list of of margins
#' @param A full array used only for dimensionality alternatively pass mydim
#' @param mydim dimensionality of full array
#'
#' @return mar with NULL replaced by vectors
#' @export
#'
marginNull2vec <- function(mar, A = NULL, mydim = NULL){
  if(!is.null(A)) mydim <- dim(A)
  
  nullBool <- unlist(lapply(mar, is.null))
  for(i in 1:length(nullBool)){
    if(nullBool[i]) mar[[i]] <- 1:mydim[i]
  }
  return(mar)
}

#' Convert zero etries of mar to NULL
#'
#' @param mar list of of margins
#' @param A full array used only for dimensionality alternatively pass mydim
#' @param mydim dimensionality of full array
#'
#' @return mar with zero replaced by NULL
#' @export
#'
marginZero2null <- function(mar){
  # boolean vector of if zero
  zeroBool <- mar == 0
  # output mar list
  outmar <- list()
  for(i in 1:length(mar)) outmar[i] <- mar[i]
  
  for(i in 1:length(zeroBool)){
    if(zeroBool[i]) outmar[[i]] <- 'NULL'
  }
  return(outmar)
}

#' Convert vector ent of mar to NULL
#'
#' @param mar list of of margins
#' @param A full array used only for dimensionality alternatively pass mydim
#' @param mydim dimensionality of full array
#'
#' @return mar with zero replaced by NULL
#' @export
#'
marginZero2null <- function(mar, A = NULL, mydim = NULL){
  if(!is.null(A)) mydim <- dim(A)
  zeroBool <- unlist(lapply(mar, function(x){x == 0}))
  for(i in 1:length(zeroBool)){
    if(zeroBool[i]) mar[[i]] <- NULL
  }
  return(mar)
}
