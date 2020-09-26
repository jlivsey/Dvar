# Purpose - understand how a list-of-list's is unlisted. This will be
#         important when defining our loglinear model inputs

idxList <- vector("list", length = 3)
idxList[[1]] <- vector("list", length = 5)
idxList[[2]] <- vector("list", length = 5)
idxList[[3]] <- vector("list", length = 5)

counter <- 1
for(i in 1:3){
  for(j in 1:5){
    idxList[[i]][[j]] <- counter
    counter <- counter + 1
  }
}

idxList
unlist(idxList)

# How would initializing idxList for r=2 in our simulation look?

# 2-way interactions to include: sex x age
#                                cenRace x geo
#                                sex x geo
inter2 <- rbind(c(2, 4), c(3, 6), c(2, 6))
inter2_levels <- rbind(c(2, 3), c(7, 20), c(2, 20))

idxList <- list(list())


counter <- 1
for(i in 1:nrow(inter2)){
    if(i==1){
        idxList[[i]] <- inter2[i, ]
      }else{
        idxList <- append
      }
  for(j in 1:prod(inter2_levels[i, ])){
    idxList <- append(idxList[[i]], )
    counter <- counter + 1
  }
}
