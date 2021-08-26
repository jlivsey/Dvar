library(Dvar)
library(L1pack)

#' True table
load("true-table.Rdata")

# load marPack object
load('marPack-all.RData')

# privacy budget parameters
bpar <- 12
geoMod <- c(1/3, 1/3, 1/3)
queryMod <- c(1/4, 1/4, 1/4, 1/4)

# Run Sim6Way to get X and y out
Xny <-
  return_X_Y_Sim6Way(
          Nrep    = 1,
          intab   = A,
          bpar    = bpar,
          marPack = marPack,
          geoMod  = geoMod,
          queryMod = queryMod)

X = Xny[[1]]
y = Xny[[2]]
infNoise = Xny[[3]]

# load coef estimates
load("20210527-results-all.RData")
coefEsts = OUT[1, ]

e = y - X %*% coefEsts
sum(e == 0 )
sum(e != 0)
which(e != 0)
idx0 = which(e == 0)
e[-idx0]

e2 = round(e, 10)
sum(e2 == 0)
sum(e2 != 0)
idx_nonZero <- which(e2 != 0)
idx_nonZero

# How many changes were in detailed cells vs margins
sum(idx_nonZero <= 7224)
sum(idx_nonZero > 7224)

# Convert to array indexing
e2Arr = array(e2, dim = dim(A))
idxArr_nonZero <- which(e2Arr != 0, arr.ind = TRUE)
B <-
  cbind(idxArr_nonZero,     # Array index of non-zero
        A[idxArr_nonZero],  # True cell count
        y[e2 != 0],         # True + noise
        coefEsts[e2 != 0])  # estimated value
head(B, 20)

# Look at number of margins changed per tract
table(idxArr_nonZero[, 5])

# plot error as function of True + noise cell value
nonZeroE2 = e2[idx_nonZero]
plot(nonZeroE2[1:1324]~B[,8])
abline(h = 0 )

# Look at all detailed cells in tract 20
idxArr_nonZero[idxArr_nonZero[,6] == 20, ]


# compare noise from nonZero cells to Laplace density of noise infused
estNoise = B[, 8] - B[, 7]
infNoise2 = infNoise[1:7224][-idx_nonZero]

summary(abs(estNoise))
summary(abs(infNoise2))

boxplot(estNoise, infNoise2)





