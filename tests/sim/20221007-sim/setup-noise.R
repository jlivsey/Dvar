# Set parameters for sim run

Nrep = 30
eps = 10

bpar = 1 / (eps * epsMod)
# Note: our bpar = their_scale * sqrt(2)
#  our_eps = their_eps / sqrt(2)

noise <- matrix(NA, 9105, Nrep)

for(i in 1:ncol(noise)){
  set.seed(5555 + i)
  print(i)
  noise[, i] <- rand_lap(9105, bpar)
}

save(noise, file = "noise.RData")
