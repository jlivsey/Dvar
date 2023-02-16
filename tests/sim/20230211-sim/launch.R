# Launch script that is basis for creating each individual launch script

library(L1pack)

# Set working directory
setwd("~/github/Dvar/tests/sim/20230211-sim/")

load(file.path("RData", "X.RData"))
load(file.path("RData", "noise.RData"))
load(file.path("RData", "true-table.RData"))
load(file.path("RData", "epsMod.RData"))

# Things that change with each different subset
tract_nums_pad <-  stringr::str_pad(1:43, 2, pad = "0")

# Define list with all regionID's including county
regionID_list = vector(mode = 'list', length = 50)
regionID_list[1:43] <- tract_nums_pad
for(c in 1:7) regionID_list[[43 + c]] <- c(tract_nums_pad[countyRecode[c, 1]:countyRecode[c, 2]], as.character(c))

WLquery_list = vector(mode = 'list', length = 3)
WLquery_list[[1]] <- c("detailed", "hhgq")
WLquery_list[[2]] <- c("detailed", "hhgq", "votingAge_hisp_cenrace")
WLquery_list[[3]] <- c("detailed", "hhgq", "votingAge_hisp_cenrace", "age_sex")

for (j in 1:50) {
for(w in 1:3){
  regionID = regionID_list[[j]]
  WLquery  = WLquery_list[[w]]

  all_names <- capture.output(cat(regionID, WLquery[-1], sep = "_"))
  saveFileName <- sprintf("results%s.RData", all_names)
  print(saveFileName)

  # Setup row index by filtering X_info to this workload
  X_info_WL <- X_info %>%
    filter(region_type != "STATE") %>% # remove state first so no issue with county j = state j region_id
    filter(region_id %in% regionID) %>%
    filter(query %in% WLquery)

  rowIDX = X_info_WL$row_idx

  if(j %in% 44:50) regionID = regionID[-length(regionID)] # pop off last element if county-level
  colIDX = which(rep(1:43, each = 168) %in% as.numeric(regionID))

  # Load things that won't change over replicates
  X_obs     <- X[rowIDX, colIDX]
  Y_true    <- c(X_obs %*% c(A)[colIDX])

  eps <- 10
  epsMod_obs <- epsMod[rowIDX]
  bpar <- (1 / epsMod_obs) * (1 / eps)
  W = diag(1 / bpar)

  WX_obs <- W %*% X_obs

  # Initialize storage
  if(j %in% 1:43){
    numReplicates <- 2000
  }else{
    numReplicates <- 50
  }
  repTime <- rep(NA, numReplicates)
  # coefEsts <- matrix(NA, nrow = length(c(A)), ncol = numReplicates)
  coefEsts <- matrix(NA, nrow = ncol(X_obs), ncol = numReplicates)

  # Main loop over number of replicates
  for (repIdx in 1:numReplicates) {
    # Get observed values for noise of this replicate
    noise_obs <- noise[rowIDX, repIdx]
    Y_obs <- Y_true + noise_obs

    # Main L1 fit with timing
    st <- Sys.time()
    if(j %in% 1:43){
      if(repIdx %% 250 == 0) print(paste0("replication ", repIdx, " starting at: ", st))
    } else {
      print(paste0("replication ", repIdx, " starting at: ", st))
    }
    fit <- l1fit(WX_obs, W %*% Y_obs, intercept = FALSE)
    et <- Sys.time()

    # Save coef ests and timing results
    coefEsts[, repIdx] <- fit$coef
    repTime[repIdx] <- difftime(et, st, units = 'mins')

  }

  save.image(file = file.path("results", saveFileName))

}}
