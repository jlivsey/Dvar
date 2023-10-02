# Thinking about how to pass around WLseq with new X_info setup

runSim <- function(WLseq, WLseqOrdering, eps, geoMod, queryMod, seed, Nrep = 500){

  require(tidyverse)

# eps = 10
# geoMod = c(.4, .3, .3)
# queryMod = c(.2, .25, .25, .3)
# seed = 123
# Nrep = 1

# Launch script that is basis for creating each individual launch script

require(L1pack)
# Change l1pack to not print warning of non-unique solution
options(warn = -1)
devtools::load_all()

# load("X.RData")
# load("true-table.RData")

X_info_upd <- add_X_info_modifiers(eps = eps,
                                   geoMod = geoMod,
                                   queryMod = queryMod)

# Setup noise matrix based on seed and input params

# Need epsMod vector of length 9105 from setup_sim6way run
# Note: our bpar = their_scale * sqrt(2)
#  our_eps = their_eps / sqrt(2)

noise <- matrix(NA, 9105, Nrep)
for(i in 1:ncol(noise)){
  set.seed(seed + i)
  noise[, i] <- rand_lap(9105, X_info_upd$bpar)
}

tract_nums_pad <-  stringr::str_pad(1:43, 2, pad = "0")

# Storage for output
R <- array(NA, dim = c(7224, length(WLseq), Nrep))
# dimnames(R) <- list(coefEsts = 1:7224, WLseq = c(1, 2, 4, 3), Nrep = 1:25)
dimnames(R)[[2]] <- WLseqOrdering

for (w in 1:length(WLseq)){

  this_region_type <- WLseq[[w]]$regionTYPE
  this_query       <- WLseq[[w]]$queryTYPE  # FIX THIS TO INDEX CORRECTLY AFTER CALL

if("COUNTY" %in% this_region_type){

  for (j in 1:7) {

    regionID = tract_nums_pad[countyRecode[j, 1]:countyRecode[j, 2]]

    print(paste0("---------- Working on County ", j, " starting at: ", Sys.time()))

    rowIDX <- X_info_upd %>%
      filter(region_id %in% c(regionID, j)) %>%
      filter(region_type %in% c("TRACT", "COUNTY")) %>%
      filter(query %in% this_query) %>%
      pull(row_idx)
    colIDX = which(rep(1:43, each = 168) %in% as.numeric(regionID))

    # Load things that won't change over replicates
    X_obs     <- X[rowIDX, colIDX]
    Y_true    <- c(X_obs %*% c(A)[colIDX])

    bpar_obs <- X_info_upd$bpar[rowIDX]
    W <- diag(1 / bpar_obs)

    WX_obs <- W %*% X_obs

    # Main loop over number of replicates
    for (repIdx in 1:Nrep) {
      # Get observed values for noise of this replicate
      noise_obs <- noise[rowIDX, repIdx]
      Y_obs <- Y_true + noise_obs

      # Main L1 fit with timing
      st <- Sys.time()
      print(paste0("replication ", repIdx, " starting at: ", st))
      fit <- try(l1fit(WX_obs, W %*% Y_obs, intercept = FALSE), silent = TRUE)
      if(class(fit) == "try-error") next
      et <- Sys.time()

      # Save coef ests and timing results
      R[colIDX, w, repIdx] <- fit$coef

    }

  } # end j loop for COUNTY estimation

} else{

for (j in seq_along(tract_nums_pad)) {

  regionID = tract_nums_pad[j]

  print(paste0("Working on tract ", regionID,
              " for WLseq ", w,
              " starting at: ", Sys.time()))

  rowIDX <- X_info_upd %>%
    filter(region_id == regionID) %>%
    filter(region_type == this_region_type) %>%
    filter(query %in% this_query) %>%
    pull(row_idx)
  colIDX = which(rep(1:43, each = 168) == as.numeric(regionID))

  # Load things that won't change over replicates
  X_obs     <- X[rowIDX, colIDX]
  Y_true    <- c(X_obs %*% c(A)[colIDX])

  bpar_obs <- X_info_upd$bpar[rowIDX]
  W <- diag(1 / bpar_obs)

  WX_obs <- W %*% X_obs

  # Initialize storage
  coefEsts <- matrix(NA, nrow = ncol(X_obs), ncol = Nrep)

  # Main loop over number of replicates
  for (repIdx in 1:Nrep) {
    # Get observed values for noise of this replicate
    noise_obs <- noise[rowIDX, repIdx]
    Y_obs <- Y_true + noise_obs

    # Handle case when only detailed-TRACT is wanted
    # Might want to use boolean if you don't have it as first WLseq always
    if(w == 1){
        R[colIDX, 1, repIdx] <- Y_obs
        next
    }

    # browser()

    # Main L1 fit with timing
    st <- Sys.time()
    # print(paste0("replication ", repIdx, " starting at: ", st))
    fit <- try(l1fit(WX_obs, W %*% Y_obs, intercept = FALSE), silent = TRUE)
    if(class(fit) == "try-error") next
    et <- Sys.time()

    # Save coef ests and timing results
    R[colIDX, w, repIdx] <- fit$coef

  }

} # end j loop for TRACT decoupled runs

} # end else loop that checks if "COUNTY" in query

} # end w loop

return(
  list(
    R = R,
    X_info_upd = X_info_upd
  )
)

}

