library(L1pack)
load("marPacks-TD.RData")
simResultsName = "results10.RData"
marPack = marPack10
load("true-table.Rdata")

Nrep = 1
eps = 5
geoMod = c(0.4,0.3,0.3)
queryMod = c(0.2,0.25,0.25,0.3)

source("sim.R")
