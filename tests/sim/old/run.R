library(Dvar)
library(L1pack)

# load marPack object
source('~/GitHub/Dvar/tests/sim/setup-marPack.R')

# privacy budget parameters
bpar <- 4
geoMod <- c(.2, .5, .3)
queryMod <- c(.1, .2, .5, .2)

#' True table
# mydim = c(2, 2, 7, 3, 2, 20)
# set.seed(123)
# A <- array(sample(0:10, size = prod(mydim), replace = TRUE), mydim)
load("~/github/Dvar/tests/sim/true-table.RData")

OUT <-
  Sim6Way(Nrep = 300,
          intab = A,
          bpar = bpar,
          marPack = marPack,
          geoMod = geoMod,
          queryMod = queryMod)

# save(OUT, file = "~/GitHub/Dvar/tests/sim/sim-results-2020-03-16.RData")
