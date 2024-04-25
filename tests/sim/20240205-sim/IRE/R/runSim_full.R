# Basic sim script for full WL (12) only

runSim_full <- function(eps, geoMod, queryMod, seed, Nrep = 50){
require(tidyverse)
require(L1pack)
options(warn = -1)

X_info_upd <- add_X_info_modifiers(eps = eps,
                                   geoMod = geoMod,
                                   queryMod = queryMod)

# Setup noise matrix based on seed and input params

noise <- matrix(NA, 9105, Nrep)
for(i in 1:ncol(noise)){
  set.seed(seed + i)
  noise[, i] <- rand_lap(9105, X_info_upd$bpar)
}

# Storage for output
R <- array(NA, dim = c(7224, 1, Nrep))
dimnames(R)[[2]] <- 12

    Y_true    <- c(X %*% c(A))

    bpar_obs <- X_info_upd$bpar
    W <- diag(1 / bpar_obs)

    WX_obs <- diag(W) * X

    # Main loop over number of replicates
    for (repIdx in 1:Nrep) {
      # Get observed values for noise of this replicate
      noise_obs <- noise[, repIdx]
      Y_obs <- Y_true + noise_obs

      # Main L1 fit with timing
      st <- Sys.time()
      print(paste0("replication ", repIdx, " starting at: ", st))
      fit <- try(l1fit(WX_obs, W %*% Y_obs, intercept = FALSE), silent = TRUE)
      if(class(fit) == "try-error") next
      et <- Sys.time()

      # Save coef ests and timing results
      R[, 1, repIdx] <- fit$coef

      repFileName <- paste0("iter", repIdx, "coefEsts.rds")
      saveRDS(fit$coef, file = repFileName)

    }

return(
  list(
    R = R,
    X_info_upd = X_info_upd
  )
)

}
