devtools::load_all()

source("setup-rowIdx-list.R")

# First we run the case where detailed-tract gets a small allocation in the
# eps schedule

eps = 1
geoMod = c(1/3, 1/3, 1/3)
queryMod = c(1/4, 1/4, 1/4, 1/4)
seed = 5555
Nrep = 50


# Try to run full WL
# Small, unif, tract
out1_full <-
  runSim_full(
    eps = eps,
    geoMod = geoMod,
    queryMod = queryMod,
    seed = seed,
    Nrep = Nrep
  )
saveRDS(out1_full, file = "out_small_unif_full.rds")


# Small, unif, tract
WLseq <- 1:8
out1 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out1, file = "out_small_unif_tract.rds")

# Small, unif, county
WLseq <- c(1, 8:2)
out2 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out2, file = "out_small_unif_county.rds")

geoMod = c(1/20, 1/20, 9/10)
queryMod = c(1/20, 1/20, 1/20, 17/20)
# Small, extreme, county
WLseq <- c(1, 8:2)
out3 <-
  runSim_county(WLseq = WLseq,
                eps = eps,
                geoMod = geoMod,
                queryMod = queryMod,
                seed = seed,
                Nrep = Nrep)
saveRDS(out3, file = "out_small_extreme_county.rds")

geoMod = c(1/20, 1/20, 9/10)
queryMod = c(1/20, 1/20, 1/20, 17/20)
# Small, extreme, tract
WLseq <- 1:8
out4 <-
  runSim_county(WLseq = WLseq,
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
WLseq <- 1:8
out5 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out5, file = "out_large_unif_tract.rds")

# large, unif, county
WLseq <- c(1, 8:2)
out6 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out2, file = "out_large_unif_county.rds")

geoMod = c(1/20, 1/20, 9/10)
queryMod = c(1/20, 1/20, 1/20, 17/20)
# large, extreme, county
WLseq <- c(1, 8:2)
out7 <-
  runSim_county(WLseq = WLseq,
                eps = eps,
                geoMod = geoMod,
                queryMod = queryMod,
                seed = seed,
                Nrep = Nrep)
saveRDS(out7, file = "out_large_extreme_county.rds")

geoMod = c(1/20, 1/20, 9/10)
queryMod = c(1/20, 1/20, 1/20, 17/20)
# large, extreme, tract
WLseq <- 1:8
out8 <-
  runSim_county(WLseq = WLseq,
                eps = eps,
                geoMod = geoMod,
                queryMod = queryMod,
                seed = seed,
                Nrep = Nrep)
saveRDS(out8, file = "out_large_extreme_tract.rds")
