library(Dvar)
library(L1pack)
library(parallel)

# load marPack object
 source('~/GitHub/Dvar/tests/sim/setup-marPack-county1-tract1.R')

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
coefEsts_tract1_county1 <- mclapply(trials, run_sim6way, mc.cores = numCores)
toc()

# convert list object to matrix
a <- matrix(unlist(coefEsts_tract1_county1), ncol = 3360, byrow = TRUE)
coefEsts_tract1_county1 <- a

# save to directory
save(coefEsts_tract1_county1, file = "~/GitHub/Dvar/tests/sim/sim-results-2020-06-04-tract1-county1.RData")
