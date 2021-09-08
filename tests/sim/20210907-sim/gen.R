source("util.R")

# ---- Begin config -----
# Levels of simulation will be defined here
sim_levels <- 0:10 # This should match marPack##.RData files
N_sim <- 1
eps   <- 12
geoMod   <- c(1/3, 1/3, 1/3)
queryMod <- c(1/4, 1/4, 1/4, 1/4)
# ---- End config -----

for(idx_sim in seq_along(sim_levels)){

  simNum <- sim_levels[idx_sim]

  # Create a folder for this level of the simulation
  # If it already exists, skip it
  dd = sprintf("sim%d", simNum)
  if (dir.exists(dd)) { next }
  dir.create(dd)

  # Generate launch script and save to file launch.R in run folder
  # The script sets variables particular to this level of the simulation,
  # then calls sim.R to run the simulation.
  #Sim6Way = function(Nrep, intab, eps, marPack, geoMod, queryMod, W = NULL) {
  ff = sprintf("%s/launch.R", dd)
  script = paste(sep = "\n",
                 sprintf("load(\"../marPack%d.RData\")\n", simNum),
                 "load(\"../true-table.Rdata\")\n",
                 sprintf("Nrep = %d", N_sim),
                 sprintf("eps = %g", eps),
                 sprintf("geoMod = %s", print_vector(geoMod)),
                 sprintf("queryMod = %s", print_vector(queryMod)),
                 "",
                 "source(\"../sim.R\")\n"
  )
  cat(script, file = ff)
}
