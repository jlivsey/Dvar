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

# ---- Boxplot of absolute error ----
boxplot(cbind(nonErr[, 1:2],
              allErr[, 1:2],
              tr1Err[, 1:2],
              tr2Err[, 1:2],
              tr3Err[, 1:2],
              cntErr[, 1:2]))

# ---- Variance ----
nonVar = apply(non, 1, var)
tr1Var = apply(tr1, 1, var)
tr2Var = apply(tr2, 1, var)
tr3Var = apply(tr3, 1, var)
cntVar = apply(cnt, 1, var)
allVar = apply(all, 1, var)

boxplot(cbind(nonVar, tr1Var, tr2Var, tr3Var, cntVar, allVar), ylim = c(0, 100))

# ---- What variance is huge? ----
Av[which(nonVar > 50)]



