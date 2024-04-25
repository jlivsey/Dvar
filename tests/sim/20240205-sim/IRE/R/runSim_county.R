# Thinking about how to pass around WLseq with new X_info setup

#Example
# WLseq <- c(1, 2, 5, 6, 7, 8, 3, 4)
# eps = 10
# geoMod = c(.4, .3, .3)
# queryMod = c(.2, .25, .25, .3)
# seed = 123
# Nrep = 20

runSim_county <-
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

    for (w in 1:length(WLseq)) {

      print(paste0("Workload ", w,
                   " starting at: ", Sys.time(),
                   "---------------------------------"))

        # Decouple for counties
        for (j in 1:7) {

          print(paste0("County ", j,
                       " starting at: ", Sys.time(),
                       "---------------------------------"))

          tract_nums_pad <-  stringr::str_pad(1:43, 2, pad = "0")
          regionID = tract_nums_pad[countyRecode[j, 1]:countyRecode[j, 2]]

          rowIDX = NULL
          for(k in 1:w){
            # If tract WL then add all tracts in county j
            if(WLseq[k] < 5){
              for(i in as.numeric(regionID)){
                rowIDX <- c(rowIDX, L[[WLseq[k]]][[i]])
              }
            }
            if(WLseq[k] >= 5){
              rowIDX <- c(rowIDX, L[[WLseq[k]]][[j]])
            }
          }
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

            # Handle detailed-TRACT independently without l1fit()
            if(w == 1){
              R[colIDX, 1, repIdx] <- Y_obs
              next
            }

            # Main L1 fit with timing
            st <- Sys.time()
            # print(paste0("replication ", repIdx, " starting at: ", st))
            print(paste0("replication ", repIdx, " starting at: ", st))
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
