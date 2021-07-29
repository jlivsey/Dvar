install.packages("Dvar_0.0.0.9000.tar.gz")

library(Dvar)
library(L1pack)

# source different sim6way to normalize epsMod
source("sim6way.r")

#' True table
load("true-table.Rdata")

# load marPack object
load('marPack-cnt.RData')

# privacy budget parameters
bpar <- 12
geoMod <- c(1/3, 1/3, 1/3)
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
save(OUT, file = "20210623-results-cnt.RData")
