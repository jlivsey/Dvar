library(Dvar)
library(L1pack)

# load marPack object
source('~/GitHub/Dvar/tests/sim/setup-marPack.R')

# privacy budget parameters
bpar <- 4
geoMod <- c(.2, .5, .3)
queryMod <- c(.1, .2, .5, .2)

#' True table
mydim = c(2, 2, 7, 3, 2, 20)
A <- array(sample(1:10, size = prod(mydim), replace = TRUE), mydim)


OUT <-
  Sim6Way(Nrep = 3,
          intab = A,
          bpar = bpar,
          marPack = marPack,
          geoMod = geoMod,
          queryMod = queryMod)

save(OUT, file = "sim-results-2020-03-13.RData")
