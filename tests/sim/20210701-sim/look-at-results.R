library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

simDir <- "tests/sim/20210701-sim/"
nsim <- 50

# ---- Load Results ----
# no margins
load(file.path(simDir, "20210701-results-non.RData"))
non = t(OUT)
colnames(non) = paste0("non", 1:nsim)
# tr1 margins (see README)
load(file.path(simDir, "20210701-results-tr1.RData"))
tr1 = t(OUT)
colnames(tr1) = paste0("tr1", 1:nsim)
# tr2 margins (see README)
load(file.path(simDir, "20210701-results-tr2.RData"))
tr2 = t(OUT)
colnames(tr2) = paste0("tr2", 1:nsim)
# tr3 margins (see README)
load(file.path(simDir, "20210701-results-tr3.RData"))
tr3 = t(OUT)
colnames(tr3) = paste0("tr3", 1:nsim)
# tr4 margins (see README)
load(file.path(simDir, "20210701-results-tr4.RData"))
tr4 = t(OUT)
colnames(tr4) = paste0("tr4", 1:nsim)
# tr5 margins (see README)
load(file.path(simDir, "20210701-results-tr5.RData"))
tr5 = t(OUT)
colnames(tr5) = paste0("tr5", 1:nsim)
# tr6 margins (see README)
load(file.path(simDir, "20210701-results-tr6.RData"))
tr6 = t(OUT)
colnames(tr6) = paste0("tr6", 1:nsim)
# tr7 margins (see README)
load(file.path(simDir, "20210701-results-tr7.RData"))
tr7 = t(OUT)
colnames(tr7) = paste0("tr7", 1:nsim)
# cnt margins (see README)
load(file.path(simDir, "20210701-results-cnt.RData"))
cnt = t(OUT)
colnames(cnt) = paste0("cnt", 1:nsim)
# all margins
load(file.path(simDir, "20210701-results-all.RData"))
all = t(OUT)
colnames(all) = paste0("all", 1:nsim)

# ---- True Data ----
load(file.path(simDir, "true-table.Rdata")) # Loads array A
Av <- c(A)

# ---- Mean -----
nonMu <- apply(non, 1, mean)
tr1Mu <- apply(tr1, 1, mean)
tr2Mu <- apply(tr2, 1, mean)
tr3Mu <- apply(tr3, 1, mean)
tr4Mu <- apply(tr4, 1, mean)
tr5Mu <- apply(tr5, 1, mean)
tr6Mu <- apply(tr6, 1, mean)
tr7Mu <- apply(tr7, 1, mean)
cntMu <- apply(cnt, 1, mean)
allMu <- apply(all, 1, mean)

# ---- Absolue Error ----
nonErr <- abs(non - Av)
tr1Err <- abs(tr1 - Av)
tr2Err <- abs(tr2 - Av)
tr3Err <- abs(tr3 - Av)
tr4Err <- abs(tr4 - Av)
tr5Err <- abs(tr5 - Av)
tr6Err <- abs(tr6 - Av)
tr7Err <- abs(tr7 - Av)
cntErr <- abs(cnt - Av)
allErr <- abs(all - Av)

boxplot(cbind(
  nonErr[, 2],
  tr1Err[, 2],
  tr2Err[, 2],
  tr3Err[, 2],
  tr4Err[, 2],
  tr5Err[, 2],
  tr6Err[, 2],
  tr7Err[, 2],
  cntErr[, 2],
  allErr[, 2]
), ylim = c(0, 1))


# ---- Which cell has largest Absolute error over all replicates -----
table(apply(nonErr, 2, which.max))
table(apply(tr1Err, 2, which.max))
table(apply(tr2Err, 2, which.max))
table(apply(tr3Err, 2, which.max))
table(apply(tr4Err, 2, which.max))
table(apply(tr5Err, 2, which.max))
table(apply(tr6Err, 2, which.max))
table(apply(tr7Err, 2, which.max))
table(apply(cntErr, 2, which.max))
table(apply(allErr, 2, which.max))

# ---- True count for cell with largest Absolute error over all replicates -----
table(Av[apply(nonErr, 2, which.max)])
table(Av[apply(tr1Err, 2, which.max)])
table(Av[apply(tr2Err, 2, which.max)])
table(Av[apply(tr3Err, 2, which.max)])
table(Av[apply(tr4Err, 2, which.max)])
table(Av[apply(tr5Err, 2, which.max)])
table(Av[apply(tr6Err, 2, which.max)])
table(Av[apply(tr7Err, 2, which.max)])
table(Av[apply(cntErr, 2, which.max)])
table(Av[apply(allErr, 2, which.max)])

# ---- root mean square error ----
nonRMSE = sqrt(apply(non^2, 1, mean) - nonMu^2)
tr1RMSE = sqrt(apply(tr1^2, 1, mean) - tr1Mu^2)
tr2RMSE = sqrt(apply(tr2^2, 1, mean) - tr2Mu^2)
tr3RMSE = sqrt(apply(tr3^2, 1, mean) - tr3Mu^2)
tr4RMSE = sqrt(apply(tr4^2, 1, mean) - tr4Mu^2)
tr5RMSE = sqrt(apply(tr5^2, 1, mean) - tr5Mu^2)
tr6RMSE = sqrt(apply(tr6^2, 1, mean) - tr6Mu^2)
tr7RMSE = sqrt(apply(tr7^2, 1, mean) - tr7Mu^2)
cntRMSE = sqrt(apply(cnt^2, 1, mean) - cntMu^2)
allRMSE = sqrt(apply(all^2, 1, mean) - allMu^2)

summary(nonRMSE)
summary(tr1RMSE)
summary(tr2RMSE)
summary(tr3RMSE)
summary(tr4RMSE)
summary(tr5RMSE)
summary(tr6RMSE)
summary(tr7RMSE)
summary(cntRMSE)
summary(allRMSE)

# ---- Boxplot of RMSE ----
boxplot(cbind(# nonRMSE,
              tr1RMSE,
              tr2RMSE,
              tr3RMSE,
              tr4RMSE,
              tr5RMSE,
              tr6RMSE,
              tr7RMSE,
              cntRMSE,
              allRMSE))



# ---- Variance ----
nonVar = apply(non, 1, var)
tr1Var = apply(tr1, 1, var)
tr2Var = apply(tr2, 1, var)
tr3Var = apply(tr3, 1, var)
tr4Var = apply(tr4, 1, var)
tr5Var = apply(tr5, 1, var)
tr6Var = apply(tr6, 1, var)
tr7Var = apply(tr7, 1, var)
cntVar = apply(cnt, 1, var)
allVar = apply(all, 1, var)

# Don't include nonVar for y-scale
boxplot(cbind(tr1Var,
              tr2Var,
              tr3Var,
              tr4Var,
              tr5Var,
              tr6Var,
              tr7Var,
              cntVar,
              allVar ))

# ---- What variance is huge? ----
Av[which(nonVar > 50)]



