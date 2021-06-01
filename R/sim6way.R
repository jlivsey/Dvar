#' Replicate Laplace Noise for 6-way table under the exact schema
#' (hhgq, sex, Cenrace, age, hisp, geo).
#'
#' @param Nrep Number of replication
#' @param intab True values - array with 6 dimensions (last is geo)
#' @param bpar Laplace noise parameter. Privacy Budget
#' @param marPack list of lists of vectors with margins to put in model.
#'                Workload of queries
#' @param geoMod vector of weights for each geography in marPack
#' @param queryMod vector of weights for each query in marPack
#' @param W (optional) weight vector of same length as
#'          d + length(marPack) = total number of predictors.
#'
#' @return A list with the following:
#' \itemize{
#'   \item param - mean parameter estimates of Nrep replications
#'   \item rmse - Root mean square error of param estimates
#'   \item coefEsts - each row is coefficient estimates of a replicate
#'   \item noise - each column is the noise applied to the true table values
#' }
#' @export
#'

Sim6Way = function(Nrep, intab, bpar, marPack, geoMod, queryMod, W = NULL) {

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
  if(is.null(W)) W <- rep(1, d + length(marPack))

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

  # Generate noise for each run as col of matrix
  set.seed(123)
  noise <- matrix(data = rlaplace(ndat*Nrep, scale = epsMod * bpar * sqrt(2)),
                  nrow = ndat,
                  ncol = Nrep)

  # Use l1fit( ) to get parameter estimates
  for(i in 1:Nrep){
    # Print time and replication number
    current_time <- Sys.time()
    print(paste0("replication ", i, " starting at: ", current_time))
    # setup dependent variable
    y <- y_true + noise[, i]
    # Fit model and return estimated coefs
    if(i == 1){
        coefEsts <- l1fit(W*X, W*y, int=F)$coef
    }else{
        new_coefEsts <- l1fit(W*X, W*y, int=F)$coef
        coefEsts     <- rbind(coefEsts, new_coefEsts)
    }

    # save(coefEsts,
    #      file = "~/GitHub/Dvar/tests/sim/sim-results-2020-03-23_Mac_totalOnly.RData")
  }

  # Take mean of all param estimates (will be output)
  # mu = apply(coefEsts, 2, mean)

  # mean square error (will be output)
  # rmse = sqrt(apply(coefEsts^2,2,mean)-mu^2)

  # Return the following list
  # return(list(
  #   param = mu,
  #   rmse  = rmse,
  #   coefEsts = coefEsts,
  #   noise = noise
  # ))
  return(coefEsts)
}
