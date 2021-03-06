library(L1pack)
library(Dvar)

options(warn = -1)

#' Lets look at simulation for same dimension as Age x sex x blockGroup
#' First set the dimensionality and simulate histogram
mydim = c(6, 2, 10)
A <- array(sample(1:10, size = prod(mydim), replace = TRUE), mydim)
Nrep <- 2000
bpar <- 1/3

#' Run simulation with only total. I use only the total because the simulation
#' function is not setup to take no margins.
res <-
  Sim3Way(
            Nrep = Nrep,
            intab = A,
            bpar = bpar,
            marPack = list(list(0,0,0))
          )

#' Look at the first 10 estimates, the true values and the rmse
x <-
cbind(
  c(A)[1:10],
  round(res$param[1:10], 4),
  round(res$rmse[1:10], 4)
)
colnames(x) <- c("true", "estimate", "rmse")
print(x)

#' Construct marPack with all possible margins and total. This is ugly at the
#' moment because the simulation function expects a list of lists.
eg <- expand.grid(0:mydim[1], 0:mydim[2], 0:mydim[3])
# which rows have all none zero entries (not valid margins)
allNotZero <- (eg[,1] != 0) & (eg[, 2] != 0) & (eg[, 3] != 0)
marPackMatrix <- eg[!allNotZero, ]
marPack = list()
for(i in 1:dim(marPackMatrix)[1]){
  marPack[[i]] <- list()
  marPack[[i]][[1]] <- marPackMatrix[i, 1]
  marPack[[i]][[2]] <- marPackMatrix[i, 2]
  marPack[[i]][[3]] <- marPackMatrix[i, 3]
}

#' Run simulation with full margins and total
resFull <-
  Sim3Way(
    Nrep = Nrep,
    intab = A,
    bpar = bpar,
    marPack = marPack
)

#' Add full margins sim results to output table
x <-
  cbind(
    c(A)[1:10],
    round(res$param[1:10], 4),
    round(res$rmse[1:10], 4),
    round(resFull$param[1:10], 4),
    round(resFull$rmse[1:10], 4)
  )
colnames(x) <- c("true",
                 "estimate-none",
                 "rmse-none",
                 "estimate-full",
                 "rmse-full")
print(x)

#' Interestingly some of the estimates are not better with the full margins.
#' Let's look at the average rmse of full vs no margins
mean(res$rmse)
mean(resFull$rmse)

#' Much Better!
hist(res$rmse, breaks = 30, main = "rmse with no margins")
hist(resFull$rmse, breaks = 30, main = "rmse with full margins")

#' Test to see if weight vector is working
weightVec <- rep(1, prod(mydim) + length(marPack))























