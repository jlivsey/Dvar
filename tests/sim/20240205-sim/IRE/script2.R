devtools::load_all()

source("setup-rowIdx-list.R")

# eps schedule

eps = 10
geoMod = c(1/3, 1/3, 1/3)
queryMod = c(1/4, 1/4, 1/4, 1/4)
seed = 6666
Nrep = 33


WLseq <- c(1, 8, 4)
out2 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out2, file = "out_eps10__unif_county1.rds")
