source("util.R")

# ---- Begin config -----
# Levels of simulation will be defined here
sim_levels <- 1:12 # This should match marPack##.RData files
N_sim <- 20
eps   <- 5
geoMod = c(.4, .3, .3)
queryMod = c(.2, .25, .25, .3)
# ---- End config -----

for(idx_sim in seq_along(sim_levels)){

  simNum <- sim_levels[idx_sim]

  # Create a name for this level of the simulation
  simResultsName = sprintf("results%d.RData", simNum)

  # Generate launch script and save
  #Sim6Way = function(Nrep, intab, eps, marPack, geoMod, queryMod, W = NULL) {
  ff = sprintf("launch%d.R", simNum)
  script = paste(sep = "\n",
                 #"library(L1pack)",
                 "load(\"marPacks-BU.RData\")",
                 sprintf("simResultsName = \"%s\"", simResultsName),
                 sprintf("marPack = marPack%d", simNum),
                 "load(\"true-table.Rdata\")\n",
                 sprintf("Nrep = %d", N_sim),
                 sprintf("eps = %g", eps),
                 sprintf("geoMod = %s", print_vector(geoMod)),
                 sprintf("queryMod = %s", print_vector(queryMod)),
                 "",
                 "source(\"sim.R\")\n"
  )
  cat(script, file = ff)
}
