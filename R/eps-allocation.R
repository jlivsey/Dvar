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
  if(zerosMar[1] == 1 &&
     zerosMar[2] == 1 &&
     zerosMar[3] == 1 &&
     zerosMar[4] == 1 &&
     zerosMar[5] == 1 &&
     zerosMar[6] != 1){
    return("total")
  }else if(zerosMar[1] != 1 &&
           zerosMar[2] == 1  &&
           zerosMar[3] == 1 &&
           zerosMar[4] == 1 &&
           zerosMar[5] == 1 ){
    return("hhgq")
  }else if(zerosMar[1] == 1 &&
           zerosMar[2] == 1 &&
           zerosMar[3] != 1 &&
           zerosMar[4] != 1 &&
           zerosMar[5] != 1 ){
    return("votingAge_hisp_cenrace")
  }else if(zerosMar[1] == 1 &&
           zerosMar[2] != 1 &&
           zerosMar[3] == 1 &&
           zerosMar[4] != 1 &&
           zerosMar[5] == 1){
    return("age_sex")
  }else if(zerosMar[1] != 1 &&
           zerosMar[2] != 1 &&
           zerosMar[3] != 1 &&
           zerosMar[4] != 1 &&
           zerosMar[5] != 1){
    return("detail")
  }else{
    stop("problem with query_of_mar function - didn't enter a known mar level")
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
        if(query %in% c("detailed", "total")){
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
