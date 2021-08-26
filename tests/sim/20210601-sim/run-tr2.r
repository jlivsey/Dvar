install.packages("Dvar_0.0.0.9000.tar.gz")

library(Dvar)
library(L1pack)

#' True table
load("true-table.Rdata")

# load marPack object
load('marPack-tr2.RData')

# privacy budget parameters
bpar <- 12
geoMod <- c(1/3, 1/3, 1/3)
queryMod <- c(1/4, 1/4, 1/4, 1/4)

# Run simulation
OUT <-
  Sim6Way(Nrep    = 100,
          intab   = A,
          bpar    = bpar,
          marPack = marPack,
          geoMod  = geoMod,
          queryMod = queryMod)

# Save output
save(OUT, file = "20210601-results-tr2.RData")
