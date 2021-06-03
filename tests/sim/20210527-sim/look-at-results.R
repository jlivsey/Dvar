library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

simDir <- "tests/sim/20210527-sim/"
nsim <- 10

# ---- Load Results ----
# no margins
load(file.path(simDir, "20210527-results-non.RData"))
non = t(OUT)
colnames(non) = paste0("non", 1:nsim)
# tr1 margins (see README)
load(file.path(simDir, "20210527-results-tr1.RData"))
tr1 = t(OUT)
colnames(tr1) = paste0("tr1", 1:nsim)
# tr2 margins (see README)
load(file.path(simDir, "20210527-results-tr2.RData"))
tr2 = t(OUT)
colnames(tr2) = paste0("tr2", 1:nsim)
# tr3 margins (see README)
load(file.path(simDir, "20210527-results-tr3.RData"))
tr3 = t(OUT)
colnames(tr3) = paste0("tr3", 1:nsim)
# cnt margins (see README)
load(file.path(simDir, "20210527-results-cnt.RData"))
cnt = t(OUT)
colnames(cnt) = paste0("cnt", 1:nsim)
# all margins
load(file.path(simDir, "20210527-results-all.RData"))
all = t(OUT)
colnames(all) = paste0("all", 1:nsim)

# ---- True Data ----
load(file.path(simDir, "true-table.Rdata")) # Loads array A
Av <- c(A)

# --- Absolue Error ----
nonErr <- non - Av
tr1Err <- tr1 - Av
tr2Err <- tr2 - Av
tr3Err <- tr3 - Av
cntErr <- cnt - Av
allErr <- all - Av


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



