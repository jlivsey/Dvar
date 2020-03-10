# Purpose - fit a single iteration of L1fit to be used in the sim6way
#           simulation function.

library(L1pack)
library(Dvar)
library(tictoc)

# load marPack object
source('~/GitHub/Dvar/tests/setup-marPack.R')

options(warn = -1)

#' Lets look at simulation for same dimension as Age x sex x blockGroup
#' First set the dimensionality and simulate histogram
mydim = c(2, 2, 7, 3, 2, 20)
A <- array(sample(1:10, size = prod(mydim), replace = TRUE), mydim)


# Sim6Way = function(Nrep, intab, bpar, marPack, W = NULL) {


  Nrep <- 1
  intab <- A
  bpar <- 4
  W <- NULL

  # New parameters
  geoMod <- c(.2, .5, .3)
  queryMod <- c(.1, .2, .5, .2)



  # dimension of 'middle' of table. No margins
  dim0 = dim(intab)
  d <- prod(dim0) # total number of unknowns
  # number of total covariate that go in model (including specified margins)
  ndat = length(marPack) + d

  # vectorize true values at indices that go into model (will add noise)
  y_true <- rep(-99, ndat)
  y_true[1:d] <- c(intab)

  # Initalize Design matrix for model at Identity = will be using all scalar
  #   elements of table for model.
  #   Will rbind margins within function
  X <- matrix(-99, ndat, d)
  X[1:d, 1:d] <- diag(d)

  # Initialize epsilon modifier
  epsMod_init <- geoMod[3] * # tract
                 queryMod[1] # detailed
  epsMod <- rep(-99, ndat)
  epsMod[1:d] <- epsMod_init

  # If no weight matrix is passed initialize to all ones
  if(is.null(W)) W <- rep(1, d + length(marPack))

tic()
  # Loop over marpack and add corresponding row to X and value to y
  for(i in 1:length(marPack)){
    if(i < 10) print(i)
    if(i%%100==0) print(round(i/length(marPack)*100, 1))
    # Check what geo and query are associated with i^th marginal
    # Need to do this now because expects 0 for totals
    g <- geo_of_mar(marPack[[i]])
    q <- query_of_mar(marPack[[i]])
    # extract i-th element from marPack and formulate as full vectors not 0.
    mar <- marginZero2vec(marPack[[i]], mydim = dim0)
    # Find next row for design matrix
    Zv <- c(arraySetOnes(dim0, mar))
    # Add to design matrix
    X[d+i, ] <- Zv
    # Find next row for y
    newY <- sum(arrayEval(intab, mar))
    # Append newY to y_true vector
    y_true[d+i] <- newY
    # Append epsilon modifier to match with y_true and specified margin
    epsMod_new <- geoChar2geoMod(geoMod, g) *
                  queryChar2queryMod(queryMod, q)
    epsMod[d+i] <- epsMod_new
  }
toc() # Time to run 287 seconds = 4.78 mins

  # SANITY CHECK
  if( ndat != length(y_true) ) warning("ndat != length(y_true)")

  # initalize storage for cell estimates
  # each row is vectorized table estimates
  coefEsts = matrix(0, c(Nrep, d))

  # Use l1fit( ) to get parameter estimates
  # for(i in 1:Nrep){
    # Noise for ith replication
    noise = rlaplace(ndat, epsMod * bpar*sqrt(2))
    # setup dependent variable
    y <- y_true + noise
    # Fit model and return estimated coefs
tic()
        coefEsts = l1fit(W*X, W*y, int=F)$coef
toc() # timer: 1070 seconds = 17.8 mins
  # }

  # Take mean of all param estimates (will be output)
  mu = apply(coefEsts, 2, mean)

  # mean square error (will be output)
  rmse = sqrt(apply(coefEsts^2,2,mean)-mu^2)

  # Return the following list
  return(list(
    param = mu,
    rmse  = rmse
  ))
#}
