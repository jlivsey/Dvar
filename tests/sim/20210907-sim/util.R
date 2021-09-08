printf = function(msg, ...)
{
  cat(sprintf(msg, ...))
}

logger = function(msg, ...)
{
  sys.time = as.character(Sys.time())
  cat(sys.time, "-", sprintf(msg, ...))
}

print_vector = function(x)
{
  sprintf("c(%s)", paste(x, collapse = ","))
}




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
  Z[idx[[1]], idx[[2]], idx[[3]], idx[[4]], idx[[5]], idx[[6]]] <- 1
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


# function to calculate all subtouples
all_subtouples <- function(v){
  A <- list()
  idx <- 1
  for(n in 1:(length(v)-1)){
    C <- combn(v, n)
    for(j in 1:ncol(C)){
      A[[idx]] <- C[, j]
      idx <- idx + 1
    }
  }
  return(A)
}

# function to check J that every touple has all sub-touples included
# NULL string = valid
# any other string is the error message
check_valid <- function(J){
  flag <- TRUE
  
  # CHECK HERE FOR J INCREASING VECTOR SIZES
  
  for(i in 1:length(J)){
    current_J <- J[[i]]
    # Check that i(th) element is vector
    if(!is.numeric(current_J)){
      flag <- paste0(i, "th element of J is not of class numeric")
      return(flag)
    }
    # DONE if length is one
    current_n <- length(current_J)
    if(current_n == 1){
      next
    }
    # check that i(th) element is sorted in increasing order
    if(!all.equal(current_J, sort(current_J))){
      flag <- paste0(i, "th element of J is not a increasing index order")
      return(flag)
    }
    # All prior elements of J contain all sub-touples
    A <- all_subtouples(current_J)
    for(k in 1:length(A)){
      if(!any(unlist(lapply(J[1:(i-1)], function(x) identical(x, A[[k]]))))){
        flag <- paste0("subtouple does not exisit in list")
        return(flag)
      }
    }
  }
  return(flag)
}

#' Check Geography associated with given margin
#'
#' @param mar list of length 6 with the 6th being Geo margin
#'
#' @return Geo level in ('state', 'county', 'tract')
#' @export
#'
geo_of_mar <- function(mar){
  if(length(mar[[6]]) > 6){
    return("state")
  } else if(length(mar[[6]]) > 1 ){
    return("county")
  } else if(length(mar[[6]] == 1)){
    return("tract")
  } else{
    stop("Geo of margin is not an expected length")
  }
}

#' Convert geo character to epsilon modifier
#'
#' @param geoMod vector of modifier levels from largest to smallest in
#'               geographic size
#' @param geo character valued geography
#'
#' @return entry of geoMod associated with geo
#' @export
#'
geoChar2geoMod <- function(geoMod, geo){
  if(geo == "state"){
    return(geoMod[1])
  }else if(geo == "county"){
    return(geoMod[2])
  }else if(geo == "tract"){
    return(geoMod[3])
  }else{
    stop("problem with geoChar2geoMod function - didn't enter a geo level")
  }
}


#' Check query associated with a given margin
#'
#' @param mar list of length 6 with first 1-5 elements being margins
#'            associated with hhgq, sex, cenrace, age, hisp in that order
#'
#' @return query from ("detail", "hhgq", "votingAge_hist_cenrace", "age_sex")
#' @export
#'
query_of_mar <- function(mar){
  zerosMar <- lapply(mar, function(x){x[1]==0})
  zerosMar <- unlist(zerosMar)
  if(zerosMar[2] == 1 &&
     zerosMar[3] == 1 &&
     zerosMar[4] == 1 &&
     zerosMar[5] == 1 ){
    return("hhgq")
  }else if(zerosMar[1] == 1 && zerosMar[2] == 1){
    return("votingAge_hisp_cenrace")
  }else if(zerosMar[1] == 1 && zerosMar[3] == 1 && zerosMar[5] == 1){
    return("age_sex")
  }else{
    return("detail")
  }
}

#' Convert query character to epsilon modifier
#'
#' @param queryMod vector of modifier levels of query
#' @param query character valued geography
#'
#' @return entry of geoMod associated with geo
#' @export
#'
queryChar2queryMod <- function(queryMod, query){
  if(query == "detailed"){
    return(queryMod[1])
  }else if(query == "hhgq"){
    return(queryMod[2])
  }else if(query == "votingAge_hisp_cenrace"){
    return(queryMod[3])
  }else if(query == "age_sex"){
    return(queryMod[4])
  }else{
    stop("problem with geoChar2geoMod function - didn't enter a geo level")
  }
}
