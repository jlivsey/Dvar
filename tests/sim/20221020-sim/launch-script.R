# Launch script that is basis for creating each individual launch script

#library(L1pack)
library(L1pack, lib = "~/R/x86_64-redhat-linux-gnu-library/3.3/")

# Source lad2() function that allows initial conditions
source("lad2.r")

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
#REPLACEME
#REPLACEME

# Load things that won't change over replicates
X_obs     <- X[WLidx, ]
Y_true    <- c(X_obs %*% c(A))

eps <- 10
epsMod_obs <- epsMod[WLidx]
bpar <- (1/epsMod_obs) * (1/eps)
W = diag(1/bpar)

WX_obs <- W %*% X_obs

# Initialize storage
numReplicates <- 10
repTime <- rep(NA, numReplicates)
coefEsts <- matrix(NA, nrow = length(c(A)), ncol = numReplicates)

# Main loop over number of replicates
for(repIdx in 1:numReplicates){

  # Get observed values for noise of this replicate
  noise_obs <- noise[WLidx, repIdx]
  Y_obs <- Y_true + noise_obs

  # Setup initial coef estimates
  if(repIdx == 2){
    olsfit = lsfit(WX_obs, WY_obs, intercept = TRUE)
    # Define initial estimate values
    init.estim <- list(
      fit = olsfit,
      # res = rnorm(ncoef + nxrow),
      res = rep(0, dim(X_obs)[1]),
      cf  = coefEsts[, 1],
      # R   = diag(ncoef+1)
      R   = qr.R(olsfit$qr)
    )
  }

  # Main L1 fit with timing
  WY_obs <- W %*% Y_obs
  if(repIdx == 1){
    print(paste0("replication NO init conditions", repIdx, " starting at: ", Sys.time()))
    fit <- lad2(WY_obs ~ WX_obs)
  } else{
    print(paste0("replication YES init conditions", repIdx, " starting at: ", Sys.time()))
    fit <- lad2(WY_obs ~ WX_obs, init.estim = init.estim)
  }

  # Save coef ests and timing results
  coefEsts[, repIdx] <- fit$coef
  repTime[repIdx] <- fit$speed[3]

}

save.image(file = saveFileName)
