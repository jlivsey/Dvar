library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

simDir <- "~/Github/Dvar/tests/sim/20210909-sim"
nsim <- 1

# ---- Load Results ----
simLevels <- c(0:4, 6:10)

R <- matrix(NA, 7224, length(simLevels))
colnames(R) <- letters[1:length(simLevels)]

for(jj in seq_along(simLevels)){
  simNum <- simLevels[jj]
  print(simNum)
  fileName <- sprintf("results%d.RData", simNum)
  print(fileName)
  load(file.path(simDir, fileName))
  R[, jj] <- coefEsts
  colnames(R)[jj] <- paste0("sim", simNum)
}

head(R)


# ---- True Data ----
load(file.path(simDir, "true-table.Rdata")) # Loads array A
Av <- c(A)

# ---- Absolue Error ----
absErr <- abs(R - Av)
par(mfrow = c(1, 2), mar = c(3, 3, 3, .1))
boxplot(absErr, main = "absolute error")
boxplot(absErr, ylim = c(0, 1), main = "zoom in to (0, 1)")

Rsort <- apply(absErr, 2, sort)

hist(R[, "sim0"], breaks = 40, ylim = c(0, 100))
hist(R[, "sim1"], breaks = 40, ylim = c(0, 100))


