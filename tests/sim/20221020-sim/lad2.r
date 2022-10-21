## ID: lad.R, last updated 2020-10-09, F.Osorio and T.Wolodzko

lad2 <-
function(formula, data, method = c("BR", "EM"), subset, na.action,
  control = NULL, model = TRUE, x = FALSE, y = FALSE, contrasts = NULL,
  init.estim = NULL){

  ret.x <- x
  ret.y <- y
  Call <- match.call()
  mf <- match.call()
  mf$method <- mf$control <- mf$model <- mf$x <- mf$y <- mf$contrasts <- mf$init.estim <- NULL
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  Terms <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  x <- model.matrix(Terms, mf, contrasts)
  ynames <- names(y)
  xnames <- dimnames(x)[[2]]
  dx <- dim(x)
  n  <- dx[1]

  ## set control values
  if (is.null(control))
    control <- l1pack.control()
  method <- match.arg(method)
  choice <- switch(method, "BR" = 0, "EM" = 1)
  ctrl  <- unlist(control)
  nctrl <- names(control)
  ctrl  <- c(ctrl, choice, 0)

  ## initial estimates
  if (is.null(init.estim)) {
    fit <- lsfit(x, y, intercept = FALSE)
    res <- fit$residuals
    cf <- fit$coefficients
    R <- qr.R(fit$qr)
  } else {
    fit <- init.estim[['fit']]
    res <- init.estim[['res']]
    cf <- init.estim[['cf']]
    R <- init.estim[['R']]
  }


  ## Call fitter
  now <- proc.time()
  fit <- .C("lad_fitter",
            y = as.double(y),
            x = as.double(x),
            dims = as.integer(dx),
            coefficients = as.double(cf),
            scale  = as.double(0),
            fitted = double(n),
            resid  = as.double(res),
            weights = as.double(rep(1, n)),
            control = as.double(ctrl),
            sad = as.double(0),
            logLik = as.double(0))
  speed <- proc.time() - now

  ## creating the output object
  out <- list(call = Call,
              dims = fit$dims,
              coefficients = fit$coefficients,
              scale = fit$scale,
              minimum = fit$sad,
              fitted.values = fit$fitted,
              residuals = fit$resid,
              R = R,
              numIter = fit$control[4],
              control = fit$control,
              weights = fit$weights,
              logLik = fit$logLik,
              speed = speed,
              converged = FALSE)
  if (out$numIter < control$maxIter)
    out$converged <- TRUE
  names(out$control) <- c(nctrl, "method", "numIter")
  names(out$coefficients) <- xnames
  names(out$residuals) <- ynames
  names(out$fitted) <- ynames
  names(out$weights) <- ynames
  dimnames(out$R) <- list(xnames, xnames)
  out$na.action <- attr(mf, "na.action")
  out$contrasts <- attr(x, "contrasts")
  out$xlevels <- .getXlevels(Terms, mf)
  out$terms <- Terms
  if (model)
    out$model <- mf
  if (ret.y)
    out$y <- fit$y #y
  if (ret.x)
    out$x <- fit$x #x
  class(out) <- "lad"
  out
}
