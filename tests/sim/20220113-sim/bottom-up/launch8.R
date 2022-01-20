library(L1pack)
load("marPacks-BU.RData")
simResultsName = "results8.RData"
marPack = marPack8
load("true-table.Rdata")

Nrep = 20
eps = 5
geoMod = c(0.4,0.3,0.3)
queryMod = c(0.2,0.25,0.25,0.3)

source("sim.R")
