
#' Want to explore what type of speed gains we can get if we provide initial
#' parameter estimates to the `lad()` function.
#'
#' First we modify the `lad()` function to accept user provided initial
#' parameter estimates.

# Settings
ncoef <- 750
nxrow <- 400
nreps <- 5

speedMat <- matrix(NA, nreps, 3)

for(i in 1:nreps){

 print(i)

X_rows <- matrix(sample(0:1, ncoef*nxrow, TRUE), nxrow, ncoef)
X <- rbind(diag(ncoef), X_rows)

y <- floor(abs(rcauchy(ncoef + nxrow, scale = 5)))

noise <- rand_lap(ncoef + nxrow, bpar = 5)
y_obs <- y + noise

# Fit with original lad() function
fit <- lad2(y_obs ~ X)
st <-
fit4 <- l1fit(X, y_obs)

olsfit = lsfit(X, y_obs, intercept = TRUE)
# Define initial estimate values
init.estim <- list(
  fit = olsfit,
  # res = rnorm(ncoef + nxrow),
  res = rep(0, ncoef + nxrow),
  cf  = y_obs[1:(ncoef+1)],
  # R   = diag(ncoef+1)
  R   = qr.R(olsfit$qr)
)

# Fit with new lad() w/ inital estimates
fit2 <- lad2(y_obs ~ X, init.estim = init.estim)

# --- Fit 3 with coefs from default run ----
# Define initial estimate values
init.estim <- list(
  fit = olsfit,
  # res = rnorm(ncoef + nxrow),
  res = rep(0, ncoef + nxrow),
  cf  = fit$coefficients,
  # R   = diag(ncoef+1)
  R   = qr.R(olsfit$qr)
)

# Fit with new lad() w/ inital estimates
fit3 <- lad2(y_obs ~ X, init.estim = init.estim)

# --- Store output ----
speedMat[i, 1] <- fit$speed[3]
speedMat[i, 2] <- fit2$speed[3]
speedMat[i, 3] <- fit3$speed[3]
speedMat[i, 4] <- fit4

}

boxplot(speedMat)
summary(speedMat)

# Define init estim to be exactly the same as in lad original function
fit <- lsfit(X, y_obs, intercept = TRUE)
res <- fit$residuals
cf <- fit$coefficients
R <- qr.R(fit$qr)

init.estim <- list(
  fit = fit,
  res = res,
  cf  = cf,
  R   = R
)

fit2 <- lad2(y_obs ~ X, init.estim = init.estim)





