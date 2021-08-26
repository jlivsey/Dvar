library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

simDir <- "tests/sim/20210623-sim/"
nsim <- 50

# ---- Load Results ----
# no margins
load(file.path(simDir, "20210623-results-non.RData"))
non = t(OUT)
colnames(non) = paste0("non", 1:nsim)
# tr1 margins (see README)
load(file.path(simDir, "20210623-results-tr1.RData"))
tr1 = t(OUT)
colnames(tr1) = paste0("tr1", 1:nsim)
# tr2 margins (see README)
load(file.path(simDir, "20210623-results-tr2.RData"))
tr2 = t(OUT)
colnames(tr2) = paste0("tr2", 1:nsim)
# tr3 margins (see README)
load(file.path(simDir, "20210623-results-tr3.RData"))
tr3 = t(OUT)
colnames(tr3) = paste0("tr3", 1:nsim)
# cnt margins (see README)
load(file.path(simDir, "20210623-results-cnt.RData"))
cnt = t(OUT)
colnames(cnt) = paste0("cnt", 1:nsim)
# all margins
load(file.path(simDir, "20210623-results-all.RData"))
all = t(OUT)
colnames(all) = paste0("all", 1:nsim)

# ---- True Data ----
load(file.path(simDir, "true-table.Rdata")) # Loads array A
Av <- c(A)

# --- Absolue Error ----
nonErr <- abs(non - Av)
tr1Err <- abs(tr1 - Av)
tr2Err <- abs(tr2 - Av)
tr3Err <- abs(tr3 - Av)
cntErr <- abs(cnt - Av)
allErr <- abs(all - Av)

summary(apply(nonErr, 1, mean))
summary(apply(tr1Err, 1, mean))
summary(apply(tr2Err, 1, mean))
summary(apply(tr3Err, 1, mean))
summary(apply(cntErr, 1, mean))
summary(apply(allErr, 1, mean))

nonMean <- apply(nonErr, 1, mean)
tr1Mean <- apply(tr1Err, 1, mean)
tr2Mean <- apply(tr2Err, 1, mean)
tr3Mean <- apply(tr3Err, 1, mean)
cntMean <- apply(cntErr, 1, mean)
allMean <- apply(allErr, 1, mean)

boxplot(cbind(nonMean, tr1Mean, tr2Mean, tr3Mean, cntMean, allMean),
        ylim = c(0, 0.015))

# ---- Variance ----
nonVar = apply(non, 1, var)
tr1Var = apply(tr1, 1, var)
tr2Var = apply(tr2, 1, var)
tr3Var = apply(tr3, 1, var)
cntVar = apply(cnt, 1, var)
allVar = apply(all, 1, var)

boxplot(cbind(nonVar, tr1Var, tr2Var, tr3Var, cntVar, allVar), ylim = c(0, 100))

# ---- RMSE ----
# Take mean of all param estimates
nonMu = apply(non, 1, mean)
tr1Mu = apply(tr1, 1, mean)
tr2Mu = apply(tr2, 1, mean)
tr3Mu = apply(tr3, 1, mean)
cntMu = apply(cnt, 1, mean)
allMu = apply(all, 1, mean)

# root mean square error
nonRMSE = sqrt(apply(non^2, 1, mean) - nonMu^2)
tr1RMSE = sqrt(apply(tr1^2, 1, mean) - tr1Mu^2)
tr2RMSE = sqrt(apply(tr2^2, 1, mean) - tr2Mu^2)
tr3RMSE = sqrt(apply(tr3^2, 1, mean) - tr3Mu^2)
cntRMSE = sqrt(apply(cnt^2, 1, mean) - cntMu^2)
allRMSE = sqrt(apply(all^2, 1, mean) - allMu^2)

summary(nonRMSE)
summary(tr1RMSE)
summary(tr2RMSE)
summary(tr3RMSE)
summary(cntRMSE)
summary(allRMSE)

# ---- What cell/index has the largest RMSE for each iteration ----

View(nonErr)

table(apply(nonErr, 2, which.max))
table(apply(tr1Err, 2, which.max))
table(apply(tr2Err, 2, which.max))
table(apply(tr3Err, 2, which.max))
table(apply(cntErr, 2, which.max))
table(apply(allErr, 2, which.max))

# ---- Lets look at spec index 7220 ----
idx = 7220
Av[idx]
nonErr[idx, ]
Z = array(0, dim = dim(A)); Z[idx] = 1
which(Z == 1, arr.ind = TRUE)
# dim1 = 2 --> Rent
# dim2 = 2 --> Female
# dim3 = 6 --> Other race (only variable not at 'highest' value)
# dim4 = 3 --> age 63+
# dim5 = 2 --> Non-hisp
# dim6 = 43 -> tract 43



