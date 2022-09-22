# Launch script that is basis for creating each individual launch script

#library(L1pack)
library(L1pack, lib = "~/R/x86_64-redhat-linux-gnu-library/3.3/")

load("X.RData")
load("WLlist_fullLen.RData")
load("noise.RData")
load("true-table.RData")
load("epsMod.RData")

# set object WLidx with vector from
if(FALSE){
  WLidx <- WLlist_fullLen[[2]]
  saveFileName <- "results02.RData"
}
WLidx <- WLlist_fullLen[[3]]
saveFileName <- "results03.RData"

# Load things that won't change over replicates
X_obs     <- X[WLidx, ]
Y_true    <- X_obs %*% c(A)

eps <- 12
epsMod_obs <- epsMod[WLidx]
bpar <- (1/epsMod_obs) * (1/eps)
W = diag(1/bpar)

# Initialize storage
numReplicates <- 10
repTime <- rep(NA, numReplicates)
coefEsts <- matrix(NA, nrow = length(c(A)), ncol = numReplicates)

# Main loop over number of replicates
for(repIdx in 1:numReplicates){

  # Get observed values for noise of this replicate
  noise_obs <- noise[WLidx, repIdx]
  Y_obs <- Y_true + noise_obs

  # Main L1 fit with timing
  st <- Sys.time()
  print(paste0("replication ", repIdx, " starting at: ", st))
  fit <- l1fit(W %*% X_obs, W %*% Y_obs, int = FALSE)
  et <- Sys.time()

  # Save coef ests and timing results
  coefEsts[, repIdx] <- fit$coef
  repTime[repIdx] <- difftime(et, st, units = 'mins')

}

save.image(file = saveFileName)
