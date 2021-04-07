# County recodes
countyRecode <- matrix(-99, 7, 2)
countyRecode[1, ] <- c(1, 3)
countyRecode[2, ] <- c(4, 6)
countyRecode[3, ] <- c(7, 10)
countyRecode[4, ] <- c(11, 12)
countyRecode[5, ] <- c(13, 15)
countyRecode[6, ] <- c(16, 18)
countyRecode[7, ] <- c(19, 20)

# State rollups
stateRecode <- matrix(-99, 2, 2)
stateRecode[1, ] <- c(1, 10)
stateRecode[2, ] <- c(11, 20)


# setup marPack
marPack = vector("list",36) # 36 total margins only for tract 1

idx <- 1

# ---- HHGQ --------------------------------------------------------------------

# Add HHGQ margins at tract level
for(i in 1:2){
  for(j in 1:1){ # TRACT 1 ONLY
    marPack[[idx]] <- list(i, 0, 0, 0, 0, j)
    idx <- idx + 1
  }
}
# Add HHGQ margins at county level
# for(i in 1:2){
#   for(j in 1:7){
#     marPack[[idx]] <- list(i, 0, 0, 0, 0, countyRecode[j, 1]:countyRecode[j, 2])
#     idx <- idx + 1
#   }
# }
# Add HHGQ margins at state level
# for(i in 1:2){
#   for(j in 1:2){
#     marPack[[idx]] <- list(i, 0, 0, 0, 0, stateRecode[j, 1]:stateRecode[j, 2])
#     idx <- idx + 1
#   }
# }

# ---- Age x Sex ---------------------------------------------------------------

# Add margins at tract level
for(i in 1:1){ # tract levels # TRACT 1 ONLY
  for(j in 1:3){ # age levels
    for(k in 1:2){ # sex levels
      marPack[[idx]] <- list(0, k, 0, j, 0, i)
      idx <- idx + 1
    }
  }
}
# Add margins at county level
# for(i in 1:7){ # county levels
#   for(j in 1:3){ # age levels
#     for(k in 1:2){ # sex levels
#       marPack[[idx]] <- list(0, k, 0, j, 0,
#                              countyRecode[i,1]:countyRecode[i, 2])
#       idx <- idx + 1
#     }
#   }
# }
# Add margins at state level
# for(i in 1:2){ # state levels
#   for(j in 1:3){ # age levels
#     for(k in 1:2){ # sex levels
#       marPack[[idx]] <- list(0, k, 0, j, 0,
#                              stateRecode[i,1]:stateRecode[i, 2])
#       idx <- idx + 1
#     }
#   }
# }
# ---- votingAge x hisp x cenRace ---------------------------------------------

votingAgeRecode <- rbind(c(1, 1),
                         c(2, 3))

# Add margins at tract level
for(i in 1:1){ # tract levels TRACT 1 ONLY
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
# Add margins at county level
# for(i in 1:7){ # county levels
#   for(j in 1:2){ # voting age levels
#     for(k in 1:2){ # hisp levels
#       for(el in 1:7){ # cenRace levels
#         marPack[[idx]] <- list(0, 0, el,
#                                votingAgeRecode[j, 1]:votingAgeRecode[j, 2],
#                                k,
#                                countyRecode[i, 1]:countyRecode[i, 2])
#         idx <- idx + 1
#       }
#     }
#   }
# }
# Add margins at state level
# for(i in 1:2){ # state levels
#   for(j in 1:2){ # voting age levels
#     for(k in 1:2){ # hisp levels
#       for(el in 1:7){ # cenRace levels
#         marPack[[idx]] <- list(0, 0, el,
#                                votingAgeRecode[j, 1]:votingAgeRecode[j, 2],
#                                k,
#                                stateRecode[i, 1]:stateRecode[i, 2])
#         idx <- idx + 1
#       }
#     }
#   }
# }
