install.packages("Dvar_0.0.0.9000.tar.gz")

library(Dvar)
library(L1pack)

#' True table
load("true-table.Rdata")

# load marPack object
load('marPack-cnt.RData')

# privacy budget parameters
bpar <- 12
geoMod <- c(0, 1/2, 1/2)
queryMod <- c(1/4, 1/4, 1/4, 1/4)

# Run simulation
OUT <-
  Sim6Way(Nrep    = 50,
          intab   = A,
          bpar    = bpar,
          marPack = marPack,
          geoMod  = geoMod,
          queryMod = queryMod)

# Save output
save(OUT, file = "20210629-results-cnt.RData")
