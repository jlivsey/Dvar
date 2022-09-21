# Launch script that is basis for creating each individual launch script

load("X.RData")
load("WLlist.RData")
load("noise.RData")
load("true-table.RData")

# set object WLidx with vector from
if(FALSE){
  WLidx <- WLlist[[1]]
  saveFileName <- "results01.RData"
}
#REPLACEME
#REPLACEME

# Load things that won't change over replicates
X_obs     <- X[WLidx, ]
Y_true    <- X_obs %*% c(A)

# Initialize storage
numReplicates <- 10
repTime <- rep(NA, numReplicates)
coefEsts <- matrix(NA, nrow = length(c(A)), ncol = numReplicates)

# Main loop over number of replicates
for(repIdx in 1:numReplicates){

  noise_obs <- noise[WLidx, repIdx]
  Y_obs <- Y_true + noise_obs

  st <- Sys.time()
  fit <- l1fit(X_obs, Y_obs)
  et <- Sys.time()

  coefEsts[, repIdx] <- fit$coef
  repTime[repIdx] <- difftime(et, st, units = 'mins')

}

save.image(file = saveFileName)
