library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
dim0 <- c(2, 2, 7, 3, 2, 43)

simDir <- "~/Github/Dvar/tests/sim/20220203-sim/top-down/"
nsim <- 20

# ---- Load Results ----
simLevels <- 1:12

R <- matrix(NA, 7224, length(simLevels))
R <- array(NA, dim = c(7224, length(simLevels), nsim))
colnames(R) <- letters[1:length(simLevels)]

for(jj in seq_along(simLevels)){
  simNum_here <- simLevels[jj]
  print(simNum_here)
  fileName <- sprintf("results%d.RData", simNum_here)
  print(fileName)
  load(file.path(simDir, fileName))
  R[, jj, ] <- t(coefEsts)
  colnames(R)[jj] <- paste0("sim", simNum_here)
}

head(R)


# ---- True Data ----
load(file.path(simDir, "true-table.Rdata")) # Loads array A
Av <- c(A)

# ---- Absolue Error from truth ----
absErr <- abs(R - Av)
par(mfrow = c(1, 2), mar = c(3, 3, 3, .1))
absErrMat <- aperm(absErr, c(1, 3, 2)) %>% matrix(ncol = 10)
boxplot(absErrMat, main = "absolute error")
boxplot(absErrMat, ylim = c(0, 9), main = "zoom in")


# ---- Histograms ----
hist(absErrMat[, 1], breaks = 50)
hist(absErrMat[, 10], breaks = 50)


# ---- summary ----
View(apply(R, 2, summary))

qprob <- function(x){sum(x[x < quantile(x, prob=.95)])}

apply(absErr, 2:3, sum) %>% t() %>% boxplot()


# ---- Absolute Error of margins in workload ----
marPack <-
  setup_margins(trct_dtld = F,
                trct_hhgq = F,
                trct_vhcr = F,
                trct_agsx = F,
                cnty_dtld = F,
                cnty_hhgq = F,
                cnty_vhcr = TRUE,
                cnty_agsx = F,
                stat_dtld = F,
                stat_hhgq = F,
                stat_vhcr = F,
                stat_agsx = F)

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
marEst <- M %*% R[, ,1]

marAbsErr <- abs(c(marTru) - marEst[, -1])
boxplot(marAbsErr)
boxplot(marAbsErr, ylim = c(0, 20), main = "zoom in")

# ---- Comparison to laplace variance ----
geoMod   <- .3
queryMod <- .3
b        <- 5 / geoMod / queryMod

2 * b^2
apply(c(marTru) - marEst, 2, var)

# save(R, A, file = "topDown-forEric.RData")










