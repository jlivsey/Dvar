setup_margins <- function(trct = FALSE,
                          cnty = FALSE,
                          stat = FALSE,
                          dtld = FALSE,
                          hhgq = FALSE,
                          vhcr = FALSE,
                          agsx = FALSE){
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

  idx <- 1

  # ---- HH/GQ --------------------------------------------------------------------

  if(hhgq){

    if(trct){
      # Add HH/GQ margins at tract level
      for(i in 1:2){     # hh/gq
        for(j in 1:43){  # tract
          marPack[[idx]] <- list(i, 0, 0, 0, 0, j)
          idx <- idx + 1
        }
      }
    }
    if(cnty){
      # Add HH/GQ margins at county level
      for(i in 1:2){    # hh/gq
        for(j in 1:7){  # county
          marPack[[idx]] <- list(i, 0, 0, 0, 0, countyRecode[j, 1]:countyRecode[j, 2])
          idx <- idx + 1
        }
      }
    }
    if(stat){
      # Add HH/GQ margins at state level
      for(i in 1:2){   # hhgq
        for(j in 1:2){ # state
          marPack[[idx]] <- list(i, 0, 0, 0, 0, stateRecode[j, 1]:stateRecode[j, 2])
          idx <- idx + 1
        }
      }
    }

  }

  # ---- Age x Sex ---------------------------------------------------------------

  if(agsx){

    if(trct){
      # Add margins at tract level
      for(i in 1:43){ # tract levels
        for(j in 1:3){ # age levels
          for(k in 1:2){ # sex levels
            marPack[[idx]] <- list(0, k, 0, j, 0, i)
            idx <- idx + 1
          }
        }
      }
    }
    if(cnty){
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
    }
    if(stat){
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
    }
  }
  # ---- votingAge x hisp x cenRace ---------------------------------------------

  if(vhcr){

    votingAgeRecode <- rbind(c(1, 1),
                             c(2, 3))

    if(trct){
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
    }
    if(cnty){
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
    }
    if(stat){
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
      }
  }

  return(marPack)
}
