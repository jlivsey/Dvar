library(tidyverse)
devtools::load_all()


eps = 10
geoMod = c(.4, .3, .3)
queryMod = c(.2, .25, .25, .3)
seed = 5555
Nrep = 50

# ---------------------------------

WLseq <- 1:8
out1 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out1, file = "out1.rds")

# County first
WLseq <- c(1, 5, 6, 7, 8, 2, 3, 4)
out2 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out2, file = 'out2.rds')

# County level VA x hist x cenRace first then all tract
WLseq <- c(1, 6, 2, 3, 4, 5, 7, 8)
out3 <-
  runSim_county(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out3, file = 'out3.rds')

# All tract first but reordering of tract and county
WLseq <- c(1, 4, 3, 2, 7, 6, 5, 8)
out4 <-
  runSim_county(WLseq = WLseq,
               eps = eps,
               geoMod = geoMod,
               queryMod = queryMod,
               seed = seed,
               Nrep = Nrep)
saveRDS(out4, file = 'out4.rds')

