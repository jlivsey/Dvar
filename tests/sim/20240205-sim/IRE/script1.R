devtools::load_all()

source("setup-rowIdx-list.R")

# eps schedule

eps = 10
geoMod = c(1/3, 1/3, 1/3)
queryMod = c(1/4, 1/4, 1/4, 1/4)
seed = 6666
Nrep = 33

# Small, unif, tract
WLseq <- c(1, 4, 8)
out1 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out1, file = "out_eps10_unif_tract1.rds")
