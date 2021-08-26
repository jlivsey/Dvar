library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

simDir <- "tests/sim/20210629-sim/"
nsim <- 50

# ---- Load Results ----
# no margins
load(file.path(simDir, "20210629-results-non.RData"))
non = t(OUT)
colnames(non) = paste0("non", 1:nsim)
# tr1 margins (see README)
load(file.path(simDir, "20210629-results-tr1.RData"))
tr1 = t(OUT)
colnames(tr1) = paste0("tr1", 1:nsim)
# tr2 margins (see README)
load(file.path(simDir, "20210629-results-tr2.RData"))
tr2 = t(OUT)
colnames(tr2) = paste0("tr2", 1:nsim)
# tr3 margins (see README)
load(file.path(simDir, "20210629-results-tr3.RData"))
tr3 = t(OUT)
colnames(tr3) = paste0("tr3", 1:nsim)
# tr4 margins (see README)
load(file.path(simDir, "20210629-results-tr4.RData"))
tr4 = t(OUT)
colnames(tr4) = paste0("tr4", 1:nsim)
# tr5 margins (see README)
load(file.path(simDir, "20210629-results-tr5.RData"))
tr5 = t(OUT)
colnames(tr5) = paste0("tr5", 1:nsim)
# tr6 margins (see README)
load(file.path(simDir, "20210629-results-tr6.RData"))
tr6 = t(OUT)
colnames(tr6) = paste0("tr6", 1:nsim)
# tr7 margins (see README)
load(file.path(simDir, "20210629-results-tr7.RData"))
tr7 = t(OUT)
colnames(tr7) = paste0("tr7", 1:nsim)
# tr8 margins (see README)
load(file.path(simDir, "20210629-results-tr8.RData"))
tr8 = t(OUT)
colnames(tr8) = paste0("tr8", 1:nsim)
# cnt margins (see README)
load(file.path(simDir, "20210629-results-cnt.RData"))
cnt = t(OUT)
colnames(cnt) = paste0("cnt", 1:nsim)
# all margins
load(file.path(simDir, "20210629-results-all.RData"))
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
tr4Err <- abs(tr4 - Av)
tr5Err <- abs(tr5 - Av)
tr6Err <- abs(tr6 - Av)
tr7Err <- abs(tr7 - Av)
tr8Err <- abs(tr8 - Av)
cntErr <- abs(cnt - Av)
allErr <- abs(all - Av)

# ---- Boxplot of absolute error for 1 replicate ----
boxplot(cbind(nonErr[, 1],
              tr1Err[, 1],
              tr2Err[, 1],
              tr3Err[, 1],
              tr4Err[, 1],
              tr5Err[, 1],
              tr6Err[, 1],
              tr7Err[, 1],
              tr8Err[, 1],
              cntErr[, 1],
              allErr[, 1]))

# ---- Boxplot of mean absolute error ----
boxplot(cbind(apply(nonErr, 1, mean),
              apply(tr1Err, 1, mean),
              apply(tr2Err, 1, mean),
              apply(tr3Err, 1, mean),
              apply(tr4Err, 1, mean),
              apply(tr5Err, 1, mean),
              apply(tr6Err, 1, mean),
              apply(tr7Err, 1, mean),
              apply(tr8Err, 1, mean),
              apply(cntErr, 1, mean),
              apply(allErr, 1, mean)))

# ---- Variance ----
nonVar = apply(non, 1, var)
tr1Var = apply(tr1, 1, var)
tr2Var = apply(tr2, 1, var)
tr3Var = apply(tr3, 1, var)
tr4Var = apply(tr4, 1, var)
tr5Var = apply(tr5, 1, var)
tr6Var = apply(tr6, 1, var)
tr7Var = apply(tr7, 1, var)
tr8Var = apply(tr8, 1, var)
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
              tr8Var,
              cntVar,
              allVar), )

# ---- What variance is huge? ----
Av[which(nonVar > 50)]



