library(Dvar)
library(L1pack)
library(parallel)

# load marPack object
# source('~/GitHub/Dvar/tests/sim/setup-marPack.R')
# Need at least list of length 2 for code to run. This is minimum possible
#    marginal information current sim6way can run with.
# marPack <- marPack[1:2]

# marPack only including margins for outcome
# [hhgq = hh, sex = M, cenRace = Wh, Age = 0-17, Hisp = hisp, Geo = 1]
marPack = vector("list",3)
marPack[[1]] <- list(1,0,0,0,0,1)
marPack[[2]] <- list(0,0,1,1,1,1)
marPack[[3]] <- list(0,1,0,1,0,1)

# privacy budget parameters
bpar <- 4
geoMod <- c(.2, .5, .3)
queryMod <- c(.1, .2, .5, .2)

#' True table
load("~/github/Dvar/tests/sim/true-table.RData")

# make function that takes dummy arguement to pass to mclapply( )
run_sim6way <- function(trial){
  Sim6Way(Nrep = 1,
          intab = A,
          bpar = bpar,
          marPack = marPack,
          geoMod = geoMod,
          queryMod = queryMod)
}

# Number of cores
numCores <- detectCores()

# Number of replicates
trials <- 1:100

# Run in parallel
library(tictoc)
tic()
coefEsts <- mclapply(trials, run_sim6way, mc.cores = numCores)
toc()

# convert list object to matrix
a <- matrix(unlist(coefEsts), ncol = 3360, byrow = TRUE)
coefEsts <- a

# save to directory
save(coefEsts, file = "~/GitHub/Dvar/tests/sim/sim-results-2020-03-24-specificCell.RData")
