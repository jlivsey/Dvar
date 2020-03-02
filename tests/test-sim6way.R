library(L1pack)
library(Dvar)

options(warn = -1)

# Setup dimension and data
mydim <- c(2,2,7,3,2,20)
possible_observation_values <- 0:10
Nrep <- 1
eps <- 4
# functions of input
d <- prod(mydim)
T <- array(sample(possible_observation_values,
                  size = prod(mydim),
                  replace = TRUE),
           mydim)
C <- recode(T, 6, list(1:3, 4:6, 7:10, 11:12, 13:15, 16:18, 19:20))
S <- recode(C, 6, list(1:4, 4:7))

#' Run simulation with only total. I use only the total because the simulation
#' function is not setup to take no margins.
#' NOTE - still sim3way right now to check for changes needed to become
#'        sim6way
res <-
  Sim3Way(
            Nrep = Nrep,
            intab = A,
            bpar = bpar,
            marPack = list(list(0,0,0))
          )
