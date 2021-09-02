install.packages("Dvar_0.0.2.tar.gz", repos = NULL, type="source")

library(Dvar)
library(L1pack)

#' True table
load("true-table.Rdata")

# load marPack object
load('marPack-tr3.RData')

# privacy budget parameters
eps      <- 5
geoMod   <- c(0, 0, 1)
queryMod <- c(1/3, 1/3, 1/3, 0)

# Run simulation
OUT <-
  Sim6Way(Nrep    = 1,
          intab   = A,
          eps     = eps,
          marPack = marPack,
          geoMod  = geoMod,
          queryMod = queryMod)

# Save output
save(OUT, file = "20210826-results-tr3.RData")
