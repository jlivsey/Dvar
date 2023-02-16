# Path to save noise output (relative to DVar path)
simDir = "~/github/Dvar/tests/sim/20230211-sim/"

# Set parameters
Nrep = 2000
eps = 10

# Need epsMod vector of length 9105 from setup_sim6way run
bpar = 1 / (eps * epsMod)
# Note: our bpar = their_scale * sqrt(2)
#  our_eps = their_eps / sqrt(2)

noise <- matrix(NA, 9105, Nrep)

for(i in 1:ncol(noise)){
  set.seed(5555 + i)
  if(i %% 100 == 0) print(i)
  noise[, i] <- rand_lap(9105, bpar)
}

save(noise, file = file.path(simDir, "RData", "noise.RData"))
