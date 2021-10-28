library(L1pack)
simResultsName = "results4.RData"
load("marPack4.RData")

load("true-table.Rdata")

Nrep = 1
eps = 1
geoMod = c(0.1,0.3,0.6)
queryMod = c(0.6,0.1,0.2,0.1)

source("sim.R")
