library(tidyverse)
devtools::load_all()

source("~/Documents/GitHub/Dvar/tests/sim/20240120-sim/setup-rowIdx-list.R")
source("~/Documents/GitHub/Dvar/tests/sim/20240120-sim/local_runSim_tract.R")


# First we run the case where detailed-tract gets a small allocation in the
# eps schedule

eps = 1
geoMod = c(1/3, 1/3, 1/3)
queryMod = c(1/4, 1/4, 1/4, 1/4)
seed = 5555
Nrep = 50

# Small, unif, tract
WLseq <- 1:4
out1 <-
  runSim_tract(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out1, file = "out_small_unif_tract.rds")

geoMod = c(1/20, 1/20, 9/10)
queryMod = c(1/20, 1/20, 1/20, 17/20)
# Small, extreme, tract
WLseq <- 1:4
out4 <-
  runSim_tract(WLseq = WLseq,
                eps = eps,
                geoMod = geoMod,
                queryMod = queryMod,
                seed = seed,
                Nrep = Nrep)
saveRDS(out4, file = "out_small_extreme_tract.rds")

eps <- 500
geoMod = c(1/3, 1/3, 1/3)
queryMod = c(1/4, 1/4, 1/4, 1/4)
# large, unif, tract
WLseq <- 1:4
out5 <-
  runSim_tract(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out5, file = "out_large_unif_tract.rds")


geoMod = c(1/20, 1/20, 9/10)
queryMod = c(1/20, 1/20, 1/20, 17/20)
# large, extreme, tract
WLseq <- 1:4
out8 <-
  runSim_tract(WLseq = WLseq,
                eps = eps,
                geoMod = geoMod,
                queryMod = queryMod,
                seed = seed,
                Nrep = Nrep)
saveRDS(out8, file = "out_large_extreme_tract.rds")
