setup_margins <- function(trct_dtld = FALSE,
                          trct_hhgq = FALSE,
                          trct_vhcr = FALSE,
                          trct_agsx = FALSE,
                          cnty_dtld = FALSE,
                          cnty_hhgq = FALSE,
                          cnty_vhcr = FALSE,
                          cnty_agsx = FALSE,
                          stat_dtld = FALSE,
                          stat_hhgq = FALSE,
                          stat_vhcr = FALSE,
                          stat_agsx = FALSE){

  # setup marPack
  marPack = list()

  # setup geoid to track marPack
  geoidPack = tibble::tibble(.rows = 1881,
    REGION_ID = -99,
    REGION_TYPE = "FILLME",

  )

  # Storage for workload indexing
  WLlist <- list()
  WLlist[["trct_dtld"]] <- 1:7224

  idx <- 1

  # ---- Detailed -------------------------------------------------------------
  # Note - detailed x tract is not needed since it is included in every run
  # if(trct_dtld){
  #   marPack[[idx]] <- list(0, 0, 0, 0, 0, 0)
  #   idx <- idx + 1
  #   marPack[[idx]] <- list(0, 0, 0, 0, 0, 1:10) # just a random second margin
  # }

  if(cnty_dtld){
  startIdx <- idx
    # Add detailed margins at county level
      for(j in 1:7){  # county
        marPack[[idx]] <- list(0, 0, 0, 0, 0, countyRecode[j, 1]:countyRecode[j, 2])
        geoidPack$REGION_ID[idx]   = j
        geoidPack$REGION_TYPE[idx] = "COUNTY"
        idx <- idx + 1
      }
  endIdx <- idx - 1
  WLlist[["cnty_dtld"]] <- 7224 + startIdx:endIdx
  }

  if(stat_dtld){
  startIdx <- idx
    # Add detailed margins at state level
      for(j in 1:2){ # state
        marPack[[idx]] <- list(0, 0, 0, 0, 0, stateRecode[j, 1]:stateRecode[j, 2])
        geoidPack$REGION_ID[idx]   = j
        geoidPack$REGION_TYPE[idx] = "STATE"
        idx <- idx + 1
      }
  endIdx <- idx - 1
  WLlist[["stat_dtld"]] <- 7224 + startIdx:endIdx
  }


  # ---- HH/GQ --------------------------------------------------------------------
    if(trct_hhgq){
    startIdx <- idx
      # Add HH/GQ margins at tract level
      for(i in 1:2){     # hh/gq
        for(j in 1:43){  # tract
          marPack[[idx]] <- list(i, 0, 0, 0, 0, j)
          geoidPack$REGION_ID[idx]   = stringr::str_pad(j, 2, pad = "0")
          geoidPack$REGION_TYPE[idx] = "TRACT"
          idx <- idx + 1
        }
      }
  endIdx <- idx - 1
  WLlist[["trct_hhgq"]] <- 7224 + startIdx:endIdx
    }

    if(cnty_hhgq){
  startIdx <- idx
      # Add HH/GQ margins at county level
      for(i in 1:2){    # hh/gq
        for(j in 1:7){  # county
          marPack[[idx]] <- list(i, 0, 0, 0, 0, countyRecode[j, 1]:countyRecode[j, 2])
          geoidPack$REGION_ID[idx]   = j
          geoidPack$REGION_TYPE[idx] = "COUNTY"
          idx <- idx + 1
        }
      }
  endIdx <- idx - 1
  WLlist[["cnty_hhgq"]] <- 7224 + startIdx:endIdx
    }

    if(stat_hhgq){
  startIdx <- idx
      # Add HH/GQ margins at state level
      for(i in 1:2){   # hhgq
        for(j in 1:2){ # state
          marPack[[idx]] <- list(i, 0, 0, 0, 0, stateRecode[j, 1]:stateRecode[j, 2])
          geoidPack$REGION_ID[idx]   = j
          geoidPack$REGION_TYPE[idx] = "STATE"
          idx <- idx + 1
        }
      }
  endIdx <- idx - 1
  WLlist[["stat_hhgq"]] <- 7224 + startIdx:endIdx
    }

  # ---- Age x Sex ---------------------------------------------------------------

    if(trct_agsx){
  startIdx <- idx
      # Add margins at tract level
      for(i in 1:43){ # tract levels
        for(j in 1:3){ # age levels
          for(k in 1:2){ # sex levels
            marPack[[idx]] <- list(0, k, 0, j, 0, i)
            geoidPack$REGION_ID[idx]   = stringr::str_pad(i, 2, pad = "0")
            geoidPack$REGION_TYPE[idx] = "TRACT"
            idx <- idx + 1
          }
        }
      }
  endIdx <- idx - 1
  WLlist[["trct_agsx"]] <- 7224 + startIdx:endIdx
    }

    if(cnty_agsx){
  startIdx <- idx
      # Add margins at county level
      for(i in 1:7){ # county levels
        for(j in 1:3){ # age levels
          for(k in 1:2){ # sex levels
            marPack[[idx]] <- list(0, k, 0, j, 0,
                                   countyRecode[i,1]:countyRecode[i, 2])
            geoidPack$REGION_ID[idx]   = i
            geoidPack$REGION_TYPE[idx] = "COUNTY"
            idx <- idx + 1
          }
        }
      }
  endIdx <- idx - 1
  WLlist[["cnty_agsx"]] <- 7224 + startIdx:endIdx
    }

    if(stat_agsx){
  startIdx <- idx
      # Add margins at state level
      for(i in 1:2){ # state levels
        for(j in 1:3){ # age levels
          for(k in 1:2){ # sex levels
            marPack[[idx]] <- list(0, k, 0, j, 0,
                                   stateRecode[i,1]:stateRecode[i, 2])
            geoidPack$REGION_ID[idx]   = i
            geoidPack$REGION_TYPE[idx] = "STATE"
            idx <- idx + 1
          }
        }
      }
  endIdx <- idx - 1
  WLlist[["stat_agsx"]] <- 7224 + startIdx:endIdx
    }

  # ---- votingAge x hisp x cenRace ---------------------------------------------


    votingAgeRecode <- rbind(c(1, 1),
                             c(2, 3))

    if(trct_vhcr){
  startIdx <- idx
      # Add margins at tract level
      for(i in 1:43){ # tract levels
        for(j in 1:2){ # voting age levels
          for(k in 1:2){ # hisp levels
            for(el in 1:7){ # cenRace levels
              marPack[[idx]] <- list(0, 0, el,
                                     votingAgeRecode[j, 1]:votingAgeRecode[j, 2],
                                     k, i)
              geoidPack$REGION_ID[idx]   = stringr::str_pad(i, 2, pad = "0")
              geoidPack$REGION_TYPE[idx] = "TRACT"
              idx <- idx + 1
            }
          }
        }
      }
  endIdx <- idx - 1
  WLlist[["trct_vhcr"]] <- 7224 + startIdx:endIdx
    }

    if(cnty_vhcr){
  startIdx <- idx
      # Add margins at county level
      for(i in 1:7){ # county levels
        for(j in 1:2){ # voting age levels
          for(k in 1:2){ # hisp levels
            for(el in 1:7){ # cenRace levels
              marPack[[idx]] <- list(0, 0, el,
                                     votingAgeRecode[j, 1]:votingAgeRecode[j, 2],
                                     k,
                                     countyRecode[i, 1]:countyRecode[i, 2])
              geoidPack$REGION_ID[idx]   = i
              geoidPack$REGION_TYPE[idx] = "COUNTY"
              idx <- idx + 1
            }
          }
        }
      }
  endIdx <- idx - 1
  WLlist[["cnty_vhcr"]] <- 7224 + startIdx:endIdx
    }

    if(stat_vhcr){
  startIdx <- idx
      # Add margins at state level
      for(i in 1:2){ # state levels
        for(j in 1:2){ # voting age levels
          for(k in 1:2){ # hisp levels
            for(el in 1:7){ # cenRace levels
              marPack[[idx]] <- list(0, 0, el,
                                     votingAgeRecode[j, 1]:votingAgeRecode[j, 2],
                                     k,
                                     stateRecode[i, 1]:stateRecode[i, 2])
              geoidPack$REGION_ID[idx]   = i
              geoidPack$REGION_TYPE[idx] = "STATE"
              idx <- idx + 1
            }
          }
        }
      }
  endIdx <- idx - 1
  WLlist[["stat_vhcr"]] <- 7224 + startIdx:endIdx
    }

  return(list(marPack = marPack, WLlist = WLlist, geoidPack = geoidPack))
}
