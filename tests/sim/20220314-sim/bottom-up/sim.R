#' Replicate Laplace Noise for 6-way table under the exact schema
#' (hhgq, sex, Cenrace, age, hisp, geo).
#'
#' @param Nrep Number of replication
#' @param intab True values - array with 6 dimensions (last is geo)
#' @param eps Laplace noise parameter. Privacy Budget
#' @param marPack list of lists of vectors with margins to put in model.
#'                Workload of queries
#' @param geoMod vector of weights for each geography in marPack
#' @param queryMod vector of weights for each query in marPack
#' @param W (optional) weight vector of same length as
#'          d + length(marPack) = total number of predictors.
#'

#Sim6Way = function(Nrep, intab, eps, marPack, geoMod, queryMod, W = NULL) {


library(L1pack, lib = "~/R/x86_64-redhat-linux-gnu-library/3.3/")

source("util.R")

#' True table
intab <- A

# Don't use weights in L1fit
W = NULL

# Change l1pack to not print warning of non-unique solution
options(warn = -1)

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
# if(is.null(W)) W <- rep(1, d + length(marPack))

# Check if marPack is empty. If so, return laplace noise for all replicates
if(is.null(marPack)){
  bpar <- (1/epsMod) * (1/eps)
  noise <- matrix(nrow = ndat, ncol = Nrep)
  for(i in 1:Nrep){
    noiseSeed <- i
    set.seed(noiseSeed)
    new_noise <- rlaplace(n = ndat, scale = bpar)
    noise[, i] <- new_noise
  }
  return(y_true + noise)
} else {

# Loop over marpack and add corresponding row to X and value to y
for(i in 1:length(marPack)){
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

# SANITY CHECK
# if( ndat != length(y_true) ) warning("ndat != length(y_true)")

# initalize storage for cell estimates
# each row is vectorized table estimates
# coefEsts = matrix(0, Nrep, d)



bpar <- (1/epsMod) * (1/eps)
noise <- matrix(nrow = ndat, ncol = Nrep)
# Set weights to be inverse of laplace scale parameter
W = diag(1/bpar)
# init storage for time of L1-fit
timeRun <- numeric(Nrep)
# Use l1fit( ) to get parameter estimates
for(i in 1:Nrep){
  # Print time and replication number
  start_time <- Sys.time()
  print(paste0("replication ", i, " starting at: ", start_time))
  # Generate noise for ith replication.
  # Set seed based on index of sim (consistant across marpack levels)
  noiseSeed <- i
  set.seed(noiseSeed)
  # vector of laplace scale param for each row of L1regression
  new_noise <- rlaplace(n = ndat, scale = bpar)
  noise[, i] <- new_noise
  # setup dependent variable
  y <- y_true + noise[, i]
  # Fit model and return estimated coefs
  if(i == 1){
    coefEsts <- l1fit(W %*% X, W %*% y, int=F)$coef
  }else{
    new_coefEsts <- l1fit(W %*% X, W %*% y, int=F)$coef
    coefEsts     <- rbind(coefEsts, new_coefEsts)
  }
  end_time <- Sys.time()
  timeRun[i] <- end_time - start_time
}

} # END if-else checking if marpack = NULL

save.image(simResultsName)
