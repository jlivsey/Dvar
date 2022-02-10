library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
dim0 <- c(2, 2, 7, 3, 2, 43)

simDir <- "~/Github/Dvar/tests/sim/20211210-sim"
nsim <- 1

# ---- Load Results ----
simLevels <- c(0:4, 6:10)

R <- matrix(NA, 7224, length(simLevels))
colnames(R) <- letters[1:length(simLevels)]

for(jj in seq_along(simLevels)){
  simNum_here <- simLevels[jj]
  print(simNum_here)
  fileName <- sprintf("results%d.RData", simNum_here)
  print(fileName)
  load(file.path(simDir, fileName))
  R[, jj] <- coefEsts
  colnames(R)[jj] <- paste0("sim", simNum_here)
}

head(R)


# ---- True Data ----
load(file.path(simDir, "true-table.Rdata")) # Loads array A
Av <- c(A)

# ---- Absolue Error ----
absErr <- abs(R - Av)
par(mfrow = c(1, 2), mar = c(3, 3, 3, .1))
boxplot(absErr, main = "absolute error")
boxplot(absErr, ylim = c(0, 9), main = "zoom in to (0, 1)")

# ---- summary ----
View(apply(R, 2, summary))


# ---- Absolute Error of margins in workload ----

M <- matrix(NA, length(marPack), 7224)
for(i in 1:length(marPack)) {
  # extract i-th element from marPack and formulate as full vectors not 0.
  mar <- marginZero2vec(marPack[[i]], mydim = dim0)
  # Find next row for design matrix
  Zv <- c(arraySetOnes(dim0, mar))
  M[i, ] <- Zv
}
# M is the margin extraction matrix. M %*% datavector will return margin

marTru <- M %*% Av
marEst <- M %*% R2

marAbsErr <- abs(c(marTru) - marEst)
boxplot(marAbsErr)


# ---- Comparison to laplace variance ----
geoMod   <- .3
queryMod <- .3
b        <- sqrt(2) / 5 / geoMod / queryMod

2 * b^2
apply(c(marTru) - marEst, 2, var)












