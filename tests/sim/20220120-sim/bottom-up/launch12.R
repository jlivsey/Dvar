load("marPacks-BU.RData")
simResultsName = "results12.RData"
marPack = marPack12
load("true-table.Rdata")

Nrep = 20
eps = 5
geoMod = c(0.4,0.3,0.3)
queryMod = c(0.2,0.25,0.25,0.3)

source("sim.R")
