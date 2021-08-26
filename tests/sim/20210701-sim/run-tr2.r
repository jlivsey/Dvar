install.packages("Dvar_0.0.1.tar.gz", repos = NULL, type="source")

library(Dvar)
library(L1pack)

#' True table
load("true-table.Rdata")

# load marPack object
load('marPack-tr2.RData')

# privacy budget parameters
eps      <- 5
geoMod   <- c(0, 0, 1)
queryMod <- c(1/3, 1/3, 1/3, 0)

# Run simulation
OUT <-
  Sim6Way(Nrep    = 50,
          intab   = A,
          eps     = eps,
          marPack = marPack,
          geoMod  = geoMod,
          queryMod = queryMod)

# Save output
save(OUT, file = "20210701-results-tr2.RData")
