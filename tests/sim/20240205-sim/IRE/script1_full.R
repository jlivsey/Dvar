devtools::load_all()

source("setup-rowIdx-list.R")

# eps schedule

eps = 10
geoMod = c(1/3, 1/3, 1/3)
queryMod = c(1/4, 1/4, 1/4, 1/4)
seed = 6666
Nrep = 33

# Run with runSim_full() that only uses WL=12
out1_full <-
  runSim_full(eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out1_full, file = "out_eps10_unif_full.rds")
