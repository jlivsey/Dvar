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
  # ---- County recodes ----
  # Load VA tract level data.frame
  # source('~/github/Dvar/tests/survey/20210410-raceHisp-tarWts.R')

  # Count the number of tracts in each county
  # x_recode <- temp_x %>%
  # count(County_name)
  # save(x_recode, file = "tests/sim/20210429-sim/x_recode.Rdata")
  x_recode <-
    structure(list(
      County_name = c(
        "Culpeper County",
        "Halifax County",
        "Harrisonburg city",
        "Hopewell city",
        "Madison County",
        "Northumberland County",
        "Rockbridge County"
      ),
      n = c(8L, 9L, 11L, 6L, 2L, 3L, 4L)
    ),
    row.names = c(NA,
                  7L),
    class = "data.frame")

  countyRecode <- matrix(-99, 7, 2)
  countyRecode[1, ] <- c(1, x_recode$n[1])
  idx = x_recode$n[1] + 1
  for(i in 2:7){
    countyRecode[i, ] <- c(idx, idx + x_recode$n[i] - 1)
    idx <- idx + x_recode$n[i]
  }

  # ---- State recodes ----
  stateRecode <- matrix(-99, 2, 2)
  stateRecode[1, ] <- c(1, 28)
  stateRecode[2, ] <- c(29, 43)

  # setup marPack
  marPack = list()

  # Storage for workload indexing
  WLlist <- list()
  WLlist[["trct_dtld"]] <- 1:7224

  idx <- 1

  # ---- Detailed --------------------------------------------------------------------
  # Note - detailed x tract is not needed since it is included in every run

  if(trct_dtld){
    marPack[[idx]] <- list(0, 0, 0, 0, 0, 0)
    idx <- idx + 1
    marPack[[idx]] <- list(0, 0, 0, 0, 0, 1:10) # just a random second margin
  }

  if(cnty_dtld){
  startIdx <- idx
    # Add detailed margins at county level
      for(j in 1:7){  # county
        marPack[[idx]] <- list(0, 0, 0, 0, 0, countyRecode[j, 1]:countyRecode[j, 2])
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
              idx <- idx + 1
            }
          }
        }
      }
  endIdx <- idx - 1
  WLlist[["cnty_vhcr"]] <- 7224 + startIdx:endIdx
    }

    if(cnty_vhcr){
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
              idx <- idx + 1
            }
          }
        }
      }
  endIdx <- idx - 1
  WLlist[["stat_vhcr"]] <- 7224 + startIdx:endIdx
    }

  return(list(marPack, WLlist))
}
