# Purpose - fit a single iteration of L1fit to be used in the sim6way
#           simulation function.

library(L1pack)
library(Dvar)

options(warn = -1)

#' Lets look at simulation for same dimension as Age x sex x blockGroup
#' First set the dimensionality and simulate histogram
mydim = c(2, 2, 7, 3, 2, 20)
A <- array(sample(1:10, size = prod(mydim), replace = TRUE), mydim)
Nrep <- 2000
bpar <- 1/3

# Define variables to match Sim6way function input
Nrep <- 1
intab <- A
bpar <- 1/3 # NOT NEEDED ANYMORE


Sim6Way = function(Nrep, intab, bpar, marPack, W = NULL) {
  # dimension of 'middle' of table. No margins
  dim0 = dim(intab)
  d <- prod(dim0) # total number of unknowns

  # vectorize true values at indices that go into model (will add noise)
  y_true = c(intab)

  # Initalize Design matrix for model at Identity = will be using all scalar
  #   elements of table for model.
  #   Will rbind margins within function
  X <- diag(d)

  # If no weight matrix is passed initialize to all ones
  if(is.null(W)) W <- rep(1, d + length(marPack))

  # Loop over marpack and add corresponding row to X and value to y
  for(i in 1:length(marPack)){
    # extract i-th element from marPack and formulate as full vectors not 0.
    mar <- marginZero2vec(marPack[[i]], mydim = dim0)
    # Find next row for design matrix
    Zv <- c(arraySetOnes(mydim, mar))
    # Add to design matrix
    X <- rbind(X, Zv)
    # Find next row for y
    newY <- sum(arrayEval(intab, mar))
    # Append newY to y_true vector
    y_true <- c(y_true, newY)
    # Sanity check - remove for large problems after working
    if( (intab %*% Zv) != newY ) warning("new y value not equal to table x X")
  }

  # number of total covariate that go in model (including specified margins)
  ndat = length(marPack) + prod(dim0)
  # SANITY CHECK
  if( ndat != length(y_true) ) warning("ndat != length(y_true)")

  # Noise for every replication and every covariate (including margins)
  noise = array(rlaplace(Nrep*ndat, scale=bpar*sqrt(2)), c(Nrep,ndat))

  # initalize storage for cell estimates
  # each row is vectorized table estimates
  coefEsts = array(0, c(Nrep, d))

  # Use l1fit( ) to get parameter estimates
  for(i in 1:Nrep){
    # setup dependent variable
    y <- y_true + noise[i, ]
    # Fit model and return estimated coefs
    coefEsts[i,] = l1fit(W*X, W*y, int=F)$coef
  }

  # Take mean of all param estimates (will be output)
  mu = apply(coefEsts, 2, mean)

  # mean square error (will be output)
  rmse = sqrt(apply(coefEsts^2,2,mean)-mu^2)

  # Return the following list
  return(list(
    param = mu,
    rmse  = rmse
  ))
}
