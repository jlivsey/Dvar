library(Dvar)
library(L1pack)

#' True table
load("true-table.RData")

# load marPack object
load('marPack-tract.RData')

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
save(OUT, file = "20210517-results-tract.RData")
