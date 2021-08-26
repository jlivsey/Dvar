# Purpose: look at slack Constraints of L1 regression problem.
#          Specifically, the values of absoluate value constraints which are
#          exactly satisfied after estimation broken down by detailed cells and
#          marginal cells.

library(Dvar)
library(L1pack)

#' True table
load("true-table.Rdata")

# load marPack object
load('marPack-all.RData')

# privacy budget parameters
# (make sure consistent with coefEst results)
bpar <- 12
geoMod <- c(1/3, 1/3, 1/3)
queryMod <- c(1/4, 1/4, 1/4, 1/4)

# load coef estimates
load("20210527-results-all.RData")
coefEsts = OUT[1, ]

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

# Construct marinal totals using true counts
nMar = length(marPack)
Xmar = X[(nrow(X)-nMar+1):nrow(X), ] #  select last nMar rows of X

S = data.frame(marNoise = infNoise[(nrow(X) - nMar + 1):nrow(X), ])
M <- S %>%
  mutate(truMar = Xmar %*% c(A)) %>%
  mutate(sumInteriorDP = Xmar %*% y[1:7224]) %>%
  mutate(estMar = Xmar %*% coefEsts) %>%
  mutate(marDP = truMar + marNoise) %>%
  mutate(startingErr = abs(sumInteriorDP - marDP)) %>%
  mutate(slackVal = round(estMar - marDP, 10))

boxplot(cbind(M$startingErr[M$slackVal == 0],
              M$startingErr[M$slackVal != 0]))

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
B = data.frame(B)
colnames(B) <- c("ownRent", "sex", "race", "age", "hisp", "tract", "true", "DPobs", "estim")
head(B, 20)


# Sum of all estimated changes in Owner/Tract 1 cells
own_tract1 = B %>%
  filter(tract == 1) %>%
  filter(ownRent == 1) %>%
  mutate(estimErr = estim - DPobs)

sum(own_tract1$estimErr) # This ALWAYS turns out to be 6.33


# Sum of all estimated changes in Owner/Tract 4 cells
own_tract4 = B %>%
  filter(tract == 4) %>%
  filter(ownRent == 1) %>%
  mutate(estimErr = estim - DPobs)

sum(own_tract4$estimErr) # This ALWAYS turns out to be 6.33

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

boxplot(estNoise,
        infNoise2,
        M$startingErr[M$slackVal != 0],
        M$startingErr[M$slackVal == 0],
        names = c("interior-notZero", "interior-zero",
                  "margin-notZero", "margin-zero"))
