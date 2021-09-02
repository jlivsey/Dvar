install.packages("Dvar_0.0.2.tar.gz", repos = NULL, type="source")

library(Dvar)
library(L1pack)

#' True table
load("true-table.Rdata")

# load marPack object
load('marPack-all.RData')

# privacy budget parameters
eps      <- 5
geoMod   <- c(1/3, 1/3, 1/3)
queryMod <- c(1/4, 1/4, 1/4, 1/4)

# Run simulation
OUT <-
  Sim6Way(Nrep     = 1,
          intab    = A,
          eps      = eps,
          marPack  = marPack,
          geoMod   = geoMod,
          queryMod = queryMod)

# Save output
save(OUT, file = "20210826-results-all.RData")
