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
marPack = vector("list",58)

idx <- 1
# Add HHGQ margins at tract level
for(i in 1:2){
  for(j in 1:20){
    marPack[[idx]] <- list(i, 0, 0, 0, 0, j)
    idx <- idx + 1
  }
}
# Add HHGQ margins at county level
for(i in 1:2){
  for(j in 1:7){
    marPack[[idx]] <- list(i, 0, 0, 0, 0, countyRecode[j, ])
    idx <- idx + 1
  }
}
# Add HHGQ margins at state level
for(i in 1:2){
  for(j in 1:2){
    marPack[[idx]] <- list(i, 0, 0, 0, 0, stateRecode[j, ])
    idx <- idx + 1
  }
}
