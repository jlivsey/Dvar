library(Dvar)
library(L1pack)
library(tictoc)

# load marPack object
source('~/GitHub/Dvar/tests/sim/20210429-sim/setup-marPack.R')

# setup marPack to be just total
marPack = marPack[1:2]

# privacy budget parameters
bpar <- 4
geoMod <- c(.2, .5, .3)
queryMod <- c(.1, .2, .5, .2)

#' True table
# mydim = c(2, 2, 7, 3, 2, 20)
# set.seed(123)
# A <- array(sample(0:10, size = prod(mydim), replace = TRUE), mydim)
load("~/github/Dvar/tests/sim/20210429-sim/true-table.RData")


tic()
OUT <-
  Sim6Way(Nrep = 1,
          intab = A,
          bpar = bpar,
          marPack = marPack,
          geoMod = geoMod,
          queryMod = queryMod)
toc()

# save(OUT, file = "tests/sim/20210429-sim/20210505-results-noMargins.RData")
