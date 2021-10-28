library(L1pack)
# Change l1pack to not print warning of non-unique solution
options(warn = -1)

# Number of replicates
N <- 10^3

# Laplace noise parameters
bpar <- 1
bpar_mar <- 5
bpar_vec <- c(bpar, bpar, bpar_mar)

# Storage
slackWgt <- matrix(NA, 3, N)
slackNon <- matrix(NA, 3, N)
noise <- matrix(NA, 3, N)
rownames(slackWgt) <- c("X1", "X2", "X+")
rownames(slackNon) <- c("X1", "X2", "X+")
rownames(noise) <- c("X1", "X2", "X+")

# Main loop
for(i in 1:N){

  if(i %% 100 == 0 ) print(i)

  # Draw x and noise values
  x <- sample(1:10, 2)
  B <- (2 * rbinom(3, 1, 1/2)-1) # random -1 or 1 coinflip
  noise[, i] <-  B * rexp(n = 3, rate = 1/c(bpar, bpar, bpar_mar))

  # Set DP observations as true + noise
  xDP <- x + noise[1:2, i]
  marDP <- sum(x) + noise[3, i]

  # Design matrix and observation vector for L1fit
  D <- rbind(diag(2), c(1, 1))
  y <- c(xDP, marDP)

  df <- data.frame(cbind(y, D))

  # L1fit NO-weights
  # coefEstsNon <- l1fit(D, y, int=F)$coef
  coefEstsNon <- lad(y~.-1, data = df, method = "EM")$coef


  # Set weights to be inverse of laplace scale parameter
  W <- diag(1/bpar_vec)
  # coefEstsWgt <- l1fit(W %*% D, W %*% y, int=F)$coef
  df2 <- data.frame(y = W %*% y, W %*% D)
  coefEstsWgt <- lad(y~.-1, data = df2, method = "EM")$coef

  # Store slack variables
  slackWgt[, i] <- (df$y != c(coefEstsWgt, sum(coefEstsWgt)))
  slackNon[, i] <- (df2$y != c(coefEstsNon, sum(coefEstsNon)))

}

apply(noise, 1, summary)

rowMeans(slackNon)
rowMeans(slackWgt)




