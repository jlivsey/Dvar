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
