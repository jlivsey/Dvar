coefEsts <- coefEsts_tract1
coefEsts <- coefEsts_tract1_county1

# Take mean of all param estimates (will be output)
mu = apply(coefEsts, 2, mean)

# mean square error (will be output)
rmse.cell = sqrt(apply(coefEsts^2,2,mean)-mu^2)
r <- rmse.cell

hist(r)
tail(sort(r), 10)

R <- array(r, c(2, 2, 7, 3, 2, 20))
R1 <- R[, , , , , 1]

max(R1)


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



# Cell analysis

cellEst.all <- coefEsts[1:100, 1]
cellEst.none <- coefEsts[, 1]
cellEst.cell <- coefEsts[, 1]

hist(cellEst.all, xlim = c(-2, 6), breaks = 30)
hist(cellEst.none, xlim = c(-2, 6), breaks = 30)
hist(cellEst.cell, xlim = c(-2, 6), breaks = 30)
