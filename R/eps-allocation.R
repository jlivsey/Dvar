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


#' Check query associated with a given margin
#'
#' @param mar list of length 6 with first 1-5 elements being margins
#'            associated with hhgq, sex, cenrace, age, hisp in that order
#'
#' @return
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
