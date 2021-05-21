library(Dvar)
library(L1pack)
library(tictoc)
library(Dvar)

#' True table
load("~/github/Dvar/tests/sim/20210429-sim/true-table.RData")

# load marPack object
source('~/GitHub/Dvar/tests/sim/20210429-sim/setup-marPack.R')

# privacy budget parameters
bpar <- 4
geoMod <- c(.2, .5, .3)
queryMod <- c(.1, .2, .5, .2)

# Run simulation
OUT <-
  Sim6Way(Nrep    = 10,
          intab   = A,
          bpar    = bpar,
          marPack = marPack,
          geoMod  = geoMod,
          queryMod = queryMod)

# Save output
save(OUT, file = "tests/sim/20210429-sim/20210505-results-noMargins.RData")
