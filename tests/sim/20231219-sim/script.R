library(tidyverse)
devtools::load_all()

source("~/Documents/GitHub/Dvar/tests/sim/20231219-sim/local_runSim_tract.R")


# First we run the case where detailed-tract gets a small allocation in the
# eps schedule

eps = 10
geoMod = c(0.495, 0.495, 0.01)
queryMod = c(.001, 0.333, 0.333, 0.333)
seed = 5555
Nrep = 100



WLseq <- 1:4
out1 <-
  runSim_tract(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out1, file = "out1_smallSched.rds")

# different WLseq
WLseq <- c(1, 4, 3, 2)
out2 <-
  runSim_tract(WLseq = WLseq,
          eps = eps,
          geoMod = geoMod,
          queryMod = queryMod,
          seed = seed,
          Nrep = Nrep)
saveRDS(out2, file = 'out2_smallSched.rds')

# -----------------------------------------------------------------------------

# Second we run the case where detailed-tract gets an independent large noise
# drawn that is independent of the eps budget and schedule

eps = 10
geoMod = c(1/3, 1/3, 1/3)
queryMod = c(1/4, 1/4, 1/4, 1/4)
seed = 5555
Nrep = 100


WLseq <- 1:4
out1 <-
  local_runSim_tract(WLseq = WLseq,
               eps = eps,
               geoMod = geoMod,
               queryMod = queryMod,
               seed = seed,
               Nrep = Nrep)
saveRDS(out1, file = "out1_indep.rds")

# different WLseq
WLseq <- c(1, 4, 3, 2)
out2 <-
  local_runSim_tract(WLseq = WLseq,
               eps = eps,
               geoMod = geoMod,
               queryMod = queryMod,
               seed = seed,
               Nrep = Nrep)
saveRDS(out2, file = 'out2_indep.rds')
