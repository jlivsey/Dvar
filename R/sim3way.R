#' Replicate Laplace Noise for 3-way table
#'
#' @param Nrep Number of replication
#' @param intab True cell values
#' @param bpar Laplace noise parameter
#' @param marPack list of lists of vectors with margins to put in model
#'
#' @return A list with the following:
#' \itemize{
#'   \item param - mean parameter estimates of Nrep replications
#'   \item rmse - Root mean square error of param estimates
#' }
#' @export
#'


Sim3Way = function(Nrep, intab, bpar, marPack) {
  # dimension of 'middle' of table. No margins
  dim0 = dim(intab)
  d <- prod(dim0) # total number of unknowns

  # vectorize true values at indices that go into model (will add noise)
  y_true = c(intab)

  # Initalize Design matrix for model at Identity = will be using all scalar
  #   elements of table for model.
  #   Will rbind margins within function
  X <- diag(d)

  # Loop over marpack and add corresponding row to X and value to y
  for(i in 1:length(marPack)){
    # extract i-th element from marPack and formulate as full vectors not NULL.
    mar <- marginNull2vec(marPack[[1]], mydim = dim0)
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
      coefEsts[i,] = l1fit(X, y, int=F)$coef
  }

  # Take mean of all param estimates (will be output)
  mu = apply(coefEsts, 2, mean)

  # mean square error (will be output)
  mse = (c(intab) - mu)^2

  # Return the following list
  return(list(
    param = mu,
    mse   = mse
  ))
}
