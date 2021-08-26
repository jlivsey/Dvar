# Pick a results file to load
load("sim-results-ALL.RData")                       # Full workload
load("sim-results-2020-03-23-noMar.RData")          # empty workload
load("sim-results-2020-03-24-specificCell.RData")   # all for specific cell
load("sim-results-2020-06-04-tract1-county1.RData") # spec cell, tract1, count1
load("sim-results-2020-06-04-tract1.RData")         # specific cell, tract 1

# Name the results coefEsts
# coefEsts <- coefEsts_tract1
# coefEsts <- coefEsts_tract1_county1

# Take mean of all param estimates (will be output)
mu = apply(coefEsts, 2, mean)

# mean square error (will be output)
rmse.cell = sqrt(apply(coefEsts^2,2,mean)-mu^2)
r <- rmse.cell

# look at output
hist(r, breaks = 30, main = "RMSE with NO margins (empty workload)")
tail(sort(r), 10)

# convert RMSE to array
R <- array(r, c(2, 2, 7, 3, 2, 20))

# Find exact cell
myIdx <- which.max(r)
myIdx <- which(abs(r - tail(sort(r), 10)[8]) < .0001)
Rbool <- array(FALSE, c(2,2,7,3,2,20))
Rbool[myIdx] <- TRUE
which(Rbool, arr.ind = TRUE)



# histogram of of RMSE's
my_rmse <- rmse[rmse < 2]
hist(my_rmse, breaks = 30)


my_hist <- function(x, thresh = 10){
  xx <- x[x<thresh]
  hist(xx, breaks = 30, xlim = c(0, 2))
}

my_hist(rmse.all, 2)
my_hist(rmse.none, 2)
my_hist(rmse.cell, 2)


# > which(Rbool, arr.ind = TRUE)
# dim1 dim2 dim3 dim4 dim5 dim6
# [1,]    1    2    6    2    2    2


Rbool[1, 2, 6, 2, 2, 2] <- TRUE
which(Rbool)


coefEsts[, 303]







# Cell analysis

cellEst.all <- coefEsts[1:100, 1]
cellEst.none <- coefEsts[, 1]
cellEst.cell <- coefEsts[, 1]

hist(cellEst.all, xlim = c(-2, 6), breaks = 30)
hist(cellEst.none, xlim = c(-2, 6), breaks = 30)
hist(cellEst.cell, xlim = c(-2, 6), breaks = 30)
