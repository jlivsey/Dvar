# Thinking about how to pass around WLseq with new X_info setup

#Example
# WLseq <- c(1, 3, 4, 2)
# eps = 10
# geoMod = c(.4, .3, .3)
# queryMod = c(.2, .25, .25, .3)
# seed = 123
# Nrep = 20

local_runSim_tract <-
  function(WLseq,
           eps,
           geoMod,
           queryMod,
           seed,
           Nrep = 500) {
    require(tidyverse)

    # Launch script that is basis for creating each individual launch script

    require(L1pack)
    # Change l1pack to not print warning of non-unique solution
    options(warn = -1)
    #devtools::load_all()

    # load("X.RData")
    # load("true-table.RData")

    X_info_upd <- add_X_info_modifiers(eps = eps,
                                       geoMod = geoMod,
                                       queryMod = queryMod)

    # Setup noise matrix based on seed and input params

    # Need epsMod vector of length 9105 from setup_sim6way run
    # Note: our bpar = their_scale * sqrt(2)
    #  our_eps = their_eps / sqrt(2)

    # Independently set the bpar for the detailed-tract cells to be large
    large_bpar = 5000
    X_info_upd$bpar[1:7224] <- large_bpar

    noise <- matrix(NA, 9105, Nrep)
    for (i in 1:ncol(noise)) {
      set.seed(seed + i)
      noise[, i] <- rand_lap(9105, X_info_upd$bpar)
    }

    # Storage for output
    R <- array(NA, dim = c(7224, length(WLseq), Nrep))
    dimnames(R) <- list(coefEsts = 1:7224,
                        WLseq = WLseq,
                        repNum = 1:Nrep)

    for (w in 1:4) {

      print(paste0("Workload ", w,
                   " starting at: ", Sys.time(),
                   "---------------------------------"))

        # Decouple for tracts
        for (j in 1:43) {

          rowIDX = NULL
          for(k in 1:w){
              rowIDX <- c(rowIDX, L[[WLseq[k]]][[j]])
          }
          colIDX = which(rep(1:43, each = 168) == j)

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

            # Handle detailed-TRACT independently without l1fit()
            if(w == 1){
              R[colIDX, 1, repIdx] <- Y_obs
              next
            }

            # Main L1 fit with timing
            st <- Sys.time()
            # print(paste0("replication ", repIdx, " starting at: ", st))
            # print(paste0("replication ", repIdx, " starting at: ", st))
            fit <-
              try(l1fit(WX_obs, W %*% Y_obs, intercept = FALSE), silent = TRUE)
            if (class(fit) == "try-error")
              next
            et <- Sys.time()

            # Save coef ests and timing results
            R[colIDX, w, repIdx] <- fit$coef

          } # repIdx

        } # j

    } # w

    return(list(R = R,
                X_info_upd = X_info_upd))

} # function
