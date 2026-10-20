################################################################################
##
## File:    TSA-Predict-Student-Functions.R
## 
## Purpose: Prediction functions.
##
## Created: 2018.03.03
##
## Version: 2025.12.18
## 
################################################################################


################################################################################
## Utilities
################################################################################

.model <- 
function(object)
{    
  ##############################################################################
  ##
  ## Arguments
  ##  object:  
  ##
  ## Value
  ##
  ##############################################################################

  ## FUNCTION:
     
  #### Extract
  coef <- object$coef
  sigma2 <- object$sigma2
  xreg <- colnames(object$xreg)
  #### Misc
  ind.drift <- is.element("drift", names(coef))
  ind.intercept <- is.element("intercept", names(coef))
  xreg.nodrift <- xreg[ xreg != "drift" ]
  ind.xreg <- NROW(xreg.nodrift) > 0
  #### Orders
  order <- object$arma[c(1,6,2)]
  seasonal <- list(order = object$arma[c(3,7,4)], period = object$arma[5]) 
  
  #### Answer
  list( 
    ind.drift = ind.drift, ind.intercept = ind.intercept, ind.xreg = ind.xreg, 
    coef = coef, sigma2 = sigma2, 
    xreg.nodrift = xreg.nodrift, order = order, seasonal = seasonal)
}
# ------------------------------------------------------------------------------


################################################################################
## Refit Arima on new data
################################################################################

#### Function copied from package Arima()
# Refits the model to new data x
arima2 <- function(x, model, xreg, method) {
  use.drift <- is.element("drift", names(model$coef))
  use.intercept <- is.element("intercept", names(model$coef))
  use.xreg <- is.element("xreg", names(model$call))
  sigma2 <- model$sigma2
  if (use.drift) {
    driftmod <- lm(model$xreg[, "drift"] ~ I(time(as.ts(model$x))))
    newxreg <- driftmod$coefficients[1] + driftmod$coefficients[2] * time(as.ts(x))
    if (!is.null(xreg)) {
      origColNames <- colnames(xreg)
      xreg <- cbind(newxreg, xreg)
      colnames(xreg) <- c("drift", origColNames)
    } else {
      xreg <- as.matrix(data.frame(drift = newxreg))
    }
    use.xreg <- TRUE
  }

  if (!is.null(model$xreg)) {
    if (is.null(xreg)) {
      stop("No regressors provided")
    }
    if (ncol(xreg) != ncol(model$xreg)) {
      stop("Number of regressors does not match fitted model")
    }
  }

  if (model$arma[5] > 1 & sum(abs(model$arma[c(3, 4, 7)])) > 0) # Seasonal model
  {
    if (use.xreg) {
      refit <- Arima(
        x, order = model$arma[c(1, 6, 2)], seasonal = list(order = model$arma[c(3, 7, 4)], period = model$arma[5]),
        include.mean = use.intercept, xreg = xreg, method = method, fixed = model$coef
      )
    } else {
      refit <- Arima(
        x, order = model$arma[c(1, 6, 2)], seasonal = list(order = model$arma[c(3, 7, 4)], period = model$arma[5]),
        include.mean = use.intercept, method = method, fixed = model$coef
      )
    }
  }
  else if (length(model$coef) > 0) # Nonseasonal model with some parameters
  {
    if (use.xreg) {
      refit <- Arima(x, order = model$arma[c(1, 6, 2)], xreg = xreg, include.mean = use.intercept, method = method, fixed = model$coef)
    } else {
      refit <- Arima(x, order = model$arma[c(1, 6, 2)], include.mean = use.intercept, method = method, fixed = model$coef)
    }
  }
  else { # No parameters
    refit <- Arima(x, order = model$arma[c(1, 6, 2)], include.mean = FALSE, method = method)
  }

  refit$var.coef <- matrix(0, length(refit$coef), length(refit$coef))
  if (use.xreg) { # Why is this needed?
    refit$xreg <- xreg
  }
  refit$sigma2 <- sigma2

  return(refit)
}

#### My own function
.Arima.refit <- function(object, y, xreg, 
  method = c("CSS-ML", "ML", "CSS")) 
{
  #### Extract settings
  settings <- .model(object)
  use.drift <- settings$ind.drift
  use.intercept <- settings$ind.intercept
  use.xreg <- settings$ind.xreg
  seasonal <- settings$seasonal
  coef <- settings$coef
  sigma2 <- settings$sigma2

  #### In case of drift
  if (use.drift) 
  {
    driftmod <- lm(object$xreg[, "drift"] ~ I(time(as.ts(object$x))))
    newxreg <- driftmod$coefficients[1] + driftmod$coefficients[2] * time(as.ts(y))
    newxreg.g <<- newxreg
    if (!is.null(xreg)) 
    {
      origColNames <- colnames(xreg)
      xreg <- cbind(newxreg, xreg)
      colnames(xreg) <- c("drift", origColNames)
    } 
    else 
    {
      xreg <- as.matrix(data.frame(drift = newxreg))
    }
    use.xreg <- TRUE
  }

  #### Compare xreg with object$xreg
  if (!is.null(object$xreg)) 
  {
    if (is.null(xreg)) stop("No regressors provided")
    if (NCOL(xreg) != NCOL(object$xreg)) 
    {
      stop("Number of regressors does not match fitted object")
    }
  }

  #### Some parameters
  if (!use.xreg) xreg <- NULL
  refit <- Arima(y = y,
    order = settings$order, seasonal = settings$seasonal,
    include.mean = use.intercept, xreg = xreg, method = method, fixed = coef)

  ####
  refit$var.coef <- matrix(0, length(refit$coef), length(refit$coef))
  refit$sigma2 <- sigma2

  #### Answer 
  refit
}
# ------------------------------------------------------------------------------


.predict.t1 <- 
function(nobs, J, n.ahead)
{    
  ##############################################################################
  ## Purpose
  ##  Compute the argument "t" to get ex-post forecasts (obtained from 
  ##   .predict(..., fixed.n.ahead = TRUE)) as function of:
  ##   - nobs = number of observations in the time series
  ##   - J = how many ex-post forecasts to compute  
  ##   - n.ahead = horizon = how many steps ahead to go 
  ##  The function is implemented because 'hand' calculation of "t" is a bit 
  ##   tricky for students.
  ## Arguments
  ##  nobs:    (numeric[1]) number of observations.
  ##  J:       (numeric[1]) Number of ex-post forecasts to compute. 
  ##  n.ahead: (numeric[1]) Number of steps ahead (horizon).
  ## Value
  ##  The argument x extended some steps ahead.
  ##############################################################################

  ## FUNCTION:
     
  #### Answer
  nobs - J - n.ahead + 1
}
# ------------------------------------------------------------------------------


.predict.naive <- 
function(object, n.ahead, t, 
  y = NULL, fixed.n.ahead = TRUE)
{
  ##############################################################################
  ## Purpose
  ##  Extract ex-post 'naive' forecasts from the time series.
  ##  The function is implemented because 'hand' extraction of 'naive' forecasts
  ##   from data is a bit tricky for students.
  ## Arguments
  ##  object:  (Arima) A model estimated from Arima().
  ##  n.ahead: (numeric[1]) Number of steps ahead (horizon).
  ##  t:       (numeric[1]) Time from which to start forecasts (it must be <= 
  ##           length(y)).
  ##  y:       (ts) Data used to compute predictions. Use this argument if you 
  ##           aim at getting forecasts beyond the estimation period (in this 
  ##           case NROW(y) must be > NROW(object$x)).  
  ## Value
  ##  (numeric[J]) 'naive' forecasts.
  ##
  ## Remarks   Could be calculated using .Arima.refit().
  ##           To be transformed back to the original scale.
  ##############################################################################

  ## FUNCTION:
     
  #### Size settings
  if (NROW(y) == 0) { y <- object$x }
  nobs <- NROW(y)
  t <- round(t[1])
  J <- nobs - t - n.ahead + 1
  
  #### Naive settings depending on the estimated model
  mod <- .model(object = object)
  mod$order[c(1,3)] <- mod$seasonal$order[c(1,3)] <- 0
  include.constant <- mod$order[2] == 0 && mod$seasonal$order[2] == 0
  xreg <- NULL
  #### Fit naive
  fit <- Arima(y = y, 
    order = mod$order, seasonal = mod$seasonal, 
    xreg = NULL, include.mean = TRUE, include.drift = FALSE)
  
  #### Answer
  .predict(object = fit, n.ahead = n.ahead, t = t, 
    y = NULL, xreg = NULL, fixed.n.ahead = fixed.n.ahead)
}
# ------------------------------------------------------------------------------


.predict.changing.h <- 
function(object, n.ahead, t, y, xreg)
{  
  ##############################################################################
  ## Arguments:
  ##  object:        An object created by Arima().
  ##  n.ahead:       Number of steps ahead (horizon).
  ##  t:             Forecast origin. Forecasts start from t + 1. 
  ##                  It must be <= length(y).
  ##  y:             Data used to compute predictions. Use this argument if you 
  ##                  aim at getting forecasts beyond the estimation period (in 
  ##                  this case NROW(y) must be > NROW(object$x)).  
  ##  xreg:          External data used to compute predictions. Use this 
  ##                  argument if you aim at getting forecasts beyond the 
  ##                  estimation period (see argument 'y'). 
  ##############################################################################
  
  #### Settings
  nobs <- NROW(y)
  ind.xreg <- NROW(xreg) > 0
  ind.in  <- 1 : t
  ind.out <- (t + 1) : (t + n.ahead)

  #### Extract
  y1 <- y[ind.in]
  if ( ind.xreg )
  {
    xreg1 <- xreg[ ind.in, , drop = FALSE]
    xreg2 <- xreg[ind.out, , drop = FALSE]
  }
  else
  {
    xreg1 <- xreg2 <- NULL
  }

  #### Fit  
  ## Prepare to avoid issues with drift
  object1 <- object
  object1$x <- object1$x[ind.in]
  object1$xreg <- object1$xreg[ind.in, , drop = FALSE] 
  ## Refit
  fit1 <- .Arima.refit(object = object1, y = y1, xreg = xreg1)
    
  #### Predict
  x1 <- forecast::forecast(object = fit1, h = n.ahead, level = 95, xreg = xreg2)
  se <- (x1$upper - x1$mean) / qnorm(0.975)

  #### Store
  pred1 <- cbind(t = ind.out, mean = as.numeric(x1$mean), se = se)

  #### Answer
  as.data.frame(pred1)
}
# ------------------------------------------------------------------------------


.predict.fixed.h <- 
function(object, n.ahead, t, y, xreg)
{  
  #############################################################################
  ## Arguments:
  ##  object:        An object created by Arima()
  ##  n.ahead:       Number of steps ahead (horizon).
  ##  t:             First origin from which to start forecasts (it must be <= 
  ##                 length(y)).
  ##  y:             Data used to compute predictions. Use this argument if you 
  ##                 aim at getting forecasts beyond the estimation period (in 
  ##                 this case NROW(y) must be > NROW(object$x)).  
  ##  xreg:          External data used to compute predictions. Use this 
  ##                 argument if you aim at getting forecasts beyond the 
  ##                 estimation period (see argument 'y')).
  #############################################################################
  
  #### Settings
  nobs <- NROW(y)
  ind.xreg  <- NROW(xreg) > 0
  mod <- .model(object)
  ind.drift <- mod$ind.drift
  
  #### Full drift
  if (ind.drift) 
  {
    drift <- object$xreg[, "drift"]
    x.drift <- seq(from = drift[1], by = 1, length.out = nobs)
  }
  
  #### External regressors
  xreg.full <- if ( ind.xreg ) 
  {
    xreg[, mod$xreg, drop = FALSE]
  }  
  else
  {
    object$xreg[, mod$xreg, drop = FALSE]
  }  
  if ( ind.drift )
  {
    xreg.full <- if (NCOL(xreg.full) == 0)
    {
      cbind(drift = x.drift)
    }
    else
    {
      cbind(drift = x.drift, xreg.full)
    }
  }
  
  #### Initialize
  ##
  # n1 <- nobs - t + 1             ## Fixed 2019.10.13
  # ind <- t : nobs
  # n1 <- nobs - t                 ## Fixed 2020.06.05
  # ind <- t : (t + n1 - 1)
  n1 <- (nobs - n.ahead) - (t - 1)
  ind <- t : (nobs - n.ahead)
  if ( n1 <= 0 ) { return(NULL) }
  ## Pred
  pred1 <- matrix(NA, n1, 3)
  colnames(pred1) <- c("t", "mean", "se")

  #### Cycle
  pos <- 1
  for( t in ind )
  { 
    #### Extract
    ind.in  <- 1 : t
    ind.out <- (t + 1) : (t + n.ahead)
    y1 <- y[ind.in]
    if ( ind.xreg )
    {
      #### Old: fixed 2020.12.01
      # xreg1 <- xreg[ ind.in, , drop = FALSE]
      # xreg2 <- xreg[ind.out, , drop = FALSE]
      #### xreg1 and xreg2 must not include a drift column
      ind <- colnames(xreg.full) != "drift"
      xreg1 <- xreg.full[ ind.in, ind, drop = FALSE]
      xreg2 <- xreg.full[ind.out, ind, drop = FALSE]
    }
    else
    {
      xreg1 <- xreg2 <- NULL
    }

    #### Fit  
    ## Prepare to avoid issues with drift
    object1 <- object
    ## Cases
    object1$x <- object1$x[ind.in]
    ## object1$xreg <- object1$xreg[ind.in, , drop = FALSE]
    object1$xreg <- xreg.full[ind.in, , drop = FALSE]
    ## Refit
    fit1 <- .Arima.refit(object = object1, y = y1, xreg = xreg1)

    #### Predict
    x1 <- forecast::forecast(object = fit1, h = n.ahead, level = 95, xreg = xreg2)
    se <- (x1$upper - x1$mean) / qnorm(0.975)
    #### Store
    pred1[pos, ] <- c(t + n.ahead, x1$mean[n.ahead], se[n.ahead])
    pos <- pos + 1
  }

  #### Answer
  as.data.frame(pred1)
}
# ------------------------------------------------------------------------------


.predict <- 
function(object, n.ahead, t, 
  y = NULL, xreg = NULL, fixed.n.ahead = TRUE)
{  
  ##############################################################################
  ## Arguments:
  ##  object:        An object created by Arima().
  ##  n.ahead:       Number of steps ahead (horizon).
  ##  t:             Time from which to start forecasts (it must be <= 
  ##                  length(y))
  ##  y:             Data used to compute predictions. Use this argument if we 
  ##                  aim at getting forecasts beyond the estimation period (in 
  ##                  this case NROW(y) must be > NROW(object$x)).  
  ##  xreg:          External data used to compute predictions. Use this 
  ##                  argument if we aim at getting forecasts beyond the 
  ##                  estimation period (see argument 'y').
  ##  fixed.n.ahead: Whether the number of steps ahead is retained fixed
  ##############################################################################

  ## FUNCTION:

  #### Settings
  fixed.n.ahead <- as.logical(fixed.n.ahead[1])

  #### Data
  if ( NROW(y) == 0 )
  {
    y <- object$x
  }
  if ( NROW(xreg) == 0 )
  {
    xreg <- .model(object)$xreg
    if ( NROW(xreg) > 0 )
    {
      xreg <- object$xreg[, xreg, drop = FALSE]
    }
  }

  #### Settings
  nobs <- NROW(y)

  #### Check
  if ( n.ahead <= 0 )
  {
    stop("Argument 'n.ahead' must be a positive integer")
  } 
  n.ahead <- round(n.ahead[1])
  if ( t > nobs )
  {
    stop("Argument 't' must be <= NROW(y)")
  } 

  #### n.ahead not fixed
  pred1 <- if (!fixed.n.ahead)
  {
    .predict.changing.h(object = object, n.ahead = n.ahead, t = t, 
      y = y, xreg = xreg)
  }
  else
  {
    .predict.fixed.h(object = object, n.ahead = n.ahead, t = t, 
      y = y, xreg = xreg)
  }
  
  #### Answer
  list( t = t, n.ahead = n.ahead, fixed.n.ahead = fixed.n.ahead, 
    pred = as.data.frame(pred1) )
}
# ------------------------------------------------------------------------------


.oeff.4.predict <- 
function(object, n.ahead, 
  delta = 0.7, type = c(".", ""))
{
  ##############################################################################
  ## Arguments:
  ##  object:        An object created by tso()
  ##  n.ahead:       Number of steps ahead.
  ##  delta:         Delta setting used in the tso() call.
  ##  type:          "" for predict()
  ##                 "." for .predict()
  ##############################################################################

  #### Settings
  type <- type[1]
  n.ahead <- n.ahead[1]
  
  #### Immediate return if no outliers are included
  if (  NROW(object$outliers) == 0 | !(type %in% c("", ".")) )
  {
    NULL
  }
    
  #### Select
  nobs <- NROW(object$y)
  freq <- frequency(object$y)
  pars <- coefs2poly(object$fit)
  mo   <- object$outliers
  
  #### Effects
  xreg <- outliers.effects(mo = mo, n = nobs + n.ahead, weights = FALSE, 
    delta = delta, pars = pars, freq = freq)
  if ( type == "" )
  {
    xreg <- xreg[(nobs + 1) : (nobs + n.ahead), , drop = FALSE]
  }
  
  #### Answer
  xreg
}
# ------------------------------------------------------------------------------


################################################################################
## Functions for prediction checking
################################v################################################

.ErrorMeasures <- 
function(y, fit, naive)
{ 
  #### Errors
  nf <- NROW(fit)
  y1 <- y[ (NROW(y) - nf + 1) : NROW(y) ]
  u  <- y1 - fit
  v  <- y1 / fit
  
  #### Error measures
  ME   <- mean( u )
  MAE  <- mean( abs(u) )
  RMSE <- sqrt( mean( u^2 ) )
  #### Percentage error measures
  if ( all(y1 > 0) )
  {
    ur     <- u / y1
    MPE    <- mean( ur )
    MAPE   <- mean( abs( ur ) )
    RMSPE  <- sqrt( mean( ur^2 ) )
    MLLE   <- mean( v - 1 - log(v) )
  }
  else
  {
    MPE    <- NULL
    MAPE   <- NULL
    RMSPE  <- NULL
    MLLE   <- NULL
  }

  #### Scaled error measures
  u1 <- y1 - naive
  ScMAE  <- MAE / mean( abs(u1) )
  ScRMSE <- RMSE / sqrt( mean( u1^2 ) )
  # if ( all(y1 > 0) )
  # {
  #   v1 <- y1 / naive
  #   ScLLE <- LLE / mean( v1 - 1 - log(v1))
  # }
  # else
  # {
  #   ScLLE <- NULL
  # }
  
  ####
  c(ME = ME, MAE = MAE, RMSE = RMSE,  
    MPE = MPE, MAPE = MAPE, RMSPE = RMSPE, MLLE = MLLE,  
    ScMAE = ScMAE, ScRMSE = ScRMSE) # , ScLLE = ScLLE)
}
# ------------------------------------------------------------------------------


.MincerZarnowitz <- 
function(y, fit)
{
  #### Estimate
  e <- y - fit
  lm1 <- lm( e ~ fit ) 
  
  #### vcov
  vcov    <- vcovHC(x = lm1, type = "const")
  vcovHC  <- vcovHC(x = lm1)
  vcovHAC <- vcovHAC(x = lm1)
  #### Coef
  cnames <- c("estimate", "s.e.", "tstat", "pvalue")
  coef    <- coeftest(x = lm1, vcov. = vcov);    colnames(coef) <- cnames
  coefHC  <- coeftest(x = lm1, vcov. = vcovHC);  colnames(coefHC) <- cnames
  coefHAC <- coeftest(x = lm1, vcov. = vcovHAC); colnames(coefHAC) <- cnames
  coef <- data.frame(coef[, 1, drop = FALSE], coef[, 2:4], HC = coefHC[, 2:4], 
    HAC = coefHAC[, 2:4], check.names = FALSE)
  #### Test
  test <- linearHypothesis(model = lm1, 
    hypothesis.matrix = c("(Intercept) = 0", "fit = 0"), vcov. = vcov)
  testHC <- linearHypothesis(model = lm1, 
    hypothesis.matrix = c("(Intercept) = 0", "fit = 0"), vcov. = vcovHC)
  testHAC <- linearHypothesis(model = lm1, 
    hypothesis.matrix = c("(Intercept) = 0", "fit = 0"), vcov. = vcovHAC)
  #### Answer
  list(x = test, xHC = testHC, xHAC = testHAC, coef = coef) 
}
# ------------------------------------------------------------------------------


.DieboldMariano <- 
function(y, f1, f2, h, loss)
{
  #### Compute
  loss <- toupper(loss)
  if (loss == "LLE")
  {
    e1 <- y / f1
    e1 <- e1 - 1 - log(e1)
    e2 <- y / f2
    e2 <- e2 - 1 - log(e2)
  }
  else if (loss == "AE")
  {
    e1 <- abs(y - f1)
    e2 <- abs(y - f2)
  }
  else if (loss == "SE")
  {
    e1 <- (y - f1)^2
    e2 <- (y - f2)^2
  }
  else
  {
    stop("'loss' must be 'LLE' or 'AE' or 'SE'")
  }
  dm.test(e1 = e1, e2 = e2, h = h, power = 1)
}
# ------------------------------------------------------------------------------


################################################################################
## Functions to manage variable transformations
################################################################################

.loglik <- 
function(object, transf.type, transf.par)
{
  ##############################################################################
  ## DESCRIPTION
  ##  Return the log-likelihood and the AIC for the original scale (y) of the 
  ##   variable when the model is estimated on a different scale (w).
  ##
  ## ARGUMENTS
  ##  object: () a model fitted with stats::arima or forecast::Arima.
  ##
  ## VALUE
  ##  (list) with components
  ##   $g (character[1]) 
  ##   $loglik (numeric[1]) 
  ##   $aic (numeric[1]) 
  ##############################################################################

  ## FUNCTION:

  #### Settings
  gDef <- c("BC", "YJ")
  g <- transf.type[1]
  par <- transf.par[1]
  
  #### Extract
  loglik <- object$loglik
  aic <- object$aic
  bic <- object$bic
  
  ####
  if ( NROW(g) == 0 | !( g %in% gDef ) )
  {
    x1 <- paste( paste0("'", gDef, "'"), collapse = ", ")
    stop("Argument 'trans.type' have to be one of ", x1)
  }
  else
  {
    #### Rebuild the time series (in the modeling scale)
    w <- fitted(object) + residuals(object)
  
    #### Adjust
    f.der <- if (g == "BC") {.BoxCox.inv1.der} else {.YeoJohnson.inv1.der}
    correction <- -sum( log( f.der(x = w, lambda = par) ) )
    loglik1 <- loglik + correction
    aic1 <- aic - 2 * (loglik1 - loglik)
    bic1 <- bic - 2 * (loglik1 - loglik)
    #### Copy
    loglik <- loglik1 
    aic <- aic1 
    bic <- bic1 
  }    
  
  #### Answer
  list(transf.type = g, transf.par = par, 
    loglik = loglik, aic = aic, bic = bic)
}
# ------------------------------------------------------------------------------


.mean <- 
function(mean, var,  
  transf.type = "YJ", transf.par = 1, tol = NULL)
{
  ##############################################################################
  ## DESCRIPTION
  ##  Return the conditional mean of the original scale (y) of the variable.
  ##
  ## ARGUMENTS
  ##  mean: (numeric) mean of the variable in the modeling scale (w).
  ##  sd: (numeric[1]) standard deviation of the error component.
  ##
  ## VALUE
  ##  (list) with components
  ##############################################################################

  ## FUNCTION:

  ####
  ans <- if ( transf.type == "YJ" )
  {
    .YeoJohnson.inv(x = mean, lambda = transf.par, 
      var = var, tol = tol)
  }
  else if ( transf.type == "BC" )
  {
    .BoxCox.inv(x = mean, lambda = transf.par, 
      var = var, tol = tol)
  }
  else 
  {
    stop("Argument 'transf.type' must be 'YJ' or 'BC'")
  }

  #### Answer
  list(transf.type = transf.type, transf.par = transf.par, 
    y = ans)
}
# ------------------------------------------------------------------------------


.pred.bands <- 
function(pred, 
  alpha = 0.05, transf.type = "YJ", transf.par = 1)
{
  ##############################################################################
  ## DESCRIPTION
  ##  Return the mean, median and confidence bands of the original scale (y) 
  ##  of the variable.
  ##
  ## ARGUMENTS
  ##  pred: () and object produced by .predict().
  ##  alpha: (numeric[1]) sum of probabilities in the tails.
  ##  g: (character[1]) type of transformation. Only "id", "sqrt", "log", 
  ##   "log10" are currently implemented.
  ##
  ## VALUE
  ##  (list) with components
  ##   $g (character[1]) 
  ##   $mean (numeric)
  ##   $median (numeric)
  ##   $lower (numeric)
  ##   $upper (numeric)
  ##   $alpha (numeric)
  ##############################################################################

  ## FUNCTION:
  
  #### Settings
  transf.type   <- transf.type[1]
  transf.par <- transf.par[1]
  
  #### Mean/Median
  median <- .mean(mean = pred$pred$mean, var = NULL, 
    transf.type = transf.type, transf.par = transf.par)$y
  mean   <- .mean(mean = pred$pred$mean, var = pred$pred$se^2, 
    transf.type = transf.type, transf.par = transf.par)$y

  #### Bands
  z <- qnorm(1 - alpha/2)
  lower <- pred$pred$mean - z * pred$pred$se
  upper <- pred$pred$mean + z * pred$pred$se
  lower <- .mean(mean = lower, var = NULL, 
    transf.type = transf.type, transf.par = transf.par)$y
  upper <- .mean(mean = upper, var = NULL, 
    transf.type = transf.type, transf.par = transf.par)$y

  #### Answer
  list(transf.type = transf.type, transf.par = transf.par, 
    t = pred$pred$t, mean = mean, median = median, 
    lower = lower, upper = upper, se = pred$pred$se, alpha = alpha) 
}
# ------------------------------------------------------------------------------


################################################################################
## Box-Cox and Yeo-Johnson transformations
################################################################################

#### Auxiliary function to apply Guerrero (1993) method 
estimate_subseries <- function(x, R) 
{
  period <- round(max(R, frequency(x)))
  nobsf <- length(x)
  nyr <- floor(nobsf / period)
  nobst <- floor(nyr * period)
  x.mat <- matrix(x[(nobsf - nobst + 1):nobsf], period, nyr)
  x.mean <- apply(x.mat, 2, mean, na.rm = TRUE)
  x.sd <- apply(x.mat, 2, sd, na.rm = TRUE)
  data.frame(mean = x.mean, sd = x.sd)
}
# ------------------------------------------------------------------------------

#### Auxiliary function for sign in order to get sign(0) = 1
.sign <- function(x) 
{
  1 - 2 * (x < 0)
}
# ------------------------------------------------------------------------------

.default.tol <- function()
{
  sqrt(.Machine$double.eps) 
}  
# ------------------------------------------------------------------------------  


.set.tol <- function(tol)
{  
  tol <- tol[1]
  tol <- tol[tol > 0]
  if (NROW(tol) == 0)
  {
    tol <- .default.tol()
  }
  tol
}  
# ------------------------------------------------------------------------------  


##############
## Box-Cox
##############

.BoxCox <- function(x, lambda, 
  tol = NULL) 
{
  #### Settings
  tol <- .set.tol(tol = tol)
  #### Check
  if (any(x <= 0)) 
  {
    stop("The Box-Cox transformation is only for strictly positive variables")
  }
  ####
  out <- if (abs(lambda) < tol) 
  {
    log(x)
  } 
  else 
  {
    (x ^ lambda - 1) / lambda
  }
  #### Answer
  # attr(out, "lambda") <- lambda
  out
}
# ------------------------------------------------------------------------------  


.BoxCox.inv1 <- function(x, lambda, 
  tol = NULL) 
{
  #### Settings
  tol <- .set.tol(tol = tol)

  #### 
  if (lambda < -tol) 
  {
    x[x > -1 / lambda] <- NA
  }
  if ( abs(lambda) < tol) 
  {
    exp(x)
  } 
  else 
  {
    (x * lambda + 1) ^ (1 / lambda)
  }
}
# ------------------------------------------------------------------------------  


.BoxCox.inv1.der <- function(x, lambda, tol = NULL) 
{
  .BoxCox.inv1(x = x, lambda = lambda, tol = tol)^(1-lambda)
}
# ------------------------------------------------------------------------------  


.BoxCox.inv1.der2 <- function(x, lambda, tol = NULL) 
{
  (1-lambda) * .BoxCox.inv1(x = x, lambda = lambda, tol = tol)^(1-2*lambda)
}
# ------------------------------------------------------------------------------  


.BoxCox.inv <- function(x, lambda, 
  var = NULL, tol = NULL) 
{
  #### Settings
  tol <- .set.tol(tol = tol)
  ####
  y <- .BoxCox.inv1(x = x, lambda = lambda, tol = tol)
  if (NROW(var) == NROW(x))
  {
    y <- y + 0.5 * .BoxCox.inv1.der2(x = x, lambda = lambda, tol = tol) * var
  }

  #### Answer
  # attr(y, "lambda") <- lambda
  # attr(y, "biasadj") <- NROW(var) > 0
  y
}
# ------------------------------------------------------------------------------  


BC.guerrero.cv <- function(lambda, x, R) 
{
  x.dat <- estimate_subseries(x, R) 
  x.rat <- x.dat$sd * x.dat$mean ^ (lambda - 1)
  sd(x.rat) / mean(x.rat)
}
# ------------------------------------------------------------------------------


.BC.guerrero.fit <- function(x, lower = -1, upper = 2, R = 2) 
{
  if( any(x <= 0, na.rm = TRUE) ) 
  {
    warning("Guerrero's method for selecting a Box-Cox parameter (lambda) 
      is given for strictly positive data.")
  }
  optimize(f = BC.guerrero.cv, interval = c(lower, upper), 
    x = x, R = R)$minimum
}
# ------------------------------------------------------------------------------


.BC.regression.fit <- function(x, R = 2) 
{
  x.dat <- estimate_subseries(x = x, R = R)
  log.sd   <- log(x.dat$sd)
  log.mean <- log(x.dat$mean)
  fit <- lm(log.sd ~ log.mean)               
  1 - as.numeric(coefficients(fit)[2]) 
}
# ------------------------------------------------------------------------------


##############
## Yeo-Johnson
##############

.YeoJohnson <- function(x, lambda, 
  tol = NULL) 
{
  #### Settings
  tol <- .set.tol(tol = tol)
  ####
  if (abs(lambda) < tol ) 
  {
    out <- - 0.5 * x^2 + x
    ind <- x > 0
    out[ind] <- log(x[ind] + 1)
  } 
  else if (abs(lambda-2) < tol ) 
  {
    out <- 0.5 * x^2 + x
    ind <- x < 0
    out[ind] <- -log(-x[ind] + 1)
  } 
  else 
  {
    sx <- sign(x)
    exp <- 1 + sx * (lambda - 1)
    out <- sx * ((abs(x) + 1) ^ exp - 1) / exp
  }
  #### Answer
  # attr(out, "lambda") <- lambda
  out
}
# ------------------------------------------------------------------------------  


.YeoJohnson.inv1 <- function(x, lambda, 
  tol = NULL) 
{
  #### Settings
  tol <- .set.tol(tol = tol)
  ####
  if (abs(lambda) < tol ) 
  {
    out <- exp(x) - 1
    ind <- x < 0
    out[ind] <- 1 - sqrt(1 - 2 * x[ind])
  }
  else if (abs(lambda-2) < tol ) 
  {
    out <- -exp(-x) + 1
    ind <- x > 0
    out[ind] <- - 1 + sqrt(1 + 2 * x[ind])
  } 
  else 
  {
    sx <- sign(x)
    exp <- 1 + sx * (lambda - 1)
    out <- sx * ((abs(x) * exp + 1)^(1/exp) - 1)
  }
  #### Answer
  # attr(out, "lambda") <- lambda
  out
}
# ------------------------------------------------------------------------------  


.YeoJohnson.inv1.der <- function(x, lambda, tol = NULL) 
{
  y <- .YeoJohnson.inv1(x = x, lambda = lambda, tol = tol)
  exp <- 1 + sign(x) * (lambda - 1)
  (abs(y) + 1)^(1-exp)
}
# ------------------------------------------------------------------------------  


.YeoJohnson.inv1.der2 <- function(x, lambda, tol = NULL) 
{
  y <- .YeoJohnson.inv1(x = x, lambda = lambda, tol = tol)
  sx <- .sign(x)
  exp <- 1 + sx * (lambda - 1)
  sx * (1 - exp) * (abs(y) + 1)^(1-2*exp)
}
# ------------------------------------------------------------------------------  


.YeoJohnson.inv <- function(x, lambda, 
  var = NULL, tol = NULL) 
{
  #### Settings
  tol <- .set.tol(tol = tol)
  ####
  y <- .YeoJohnson.inv1(x = x, lambda = lambda, tol = tol)
  if (NROW(var) == NROW(x))
  {
    y <- y + 0.5 * .YeoJohnson.inv1.der2(x = x, lambda = lambda, tol = tol) * var
  }
  #### Answer
  # attr(y, "lambda") <- lambda
  # attr(y, "biasadj") <- NROW(var) > 0
  y
}
# ------------------------------------------------------------------------------  


YJ.guerrero.cv <- function(lambda, x, R) 
{
  x.dat <- estimate_subseries(x, R) 
  x.rat <- x.dat$sd * (abs(x.dat$mean) + 1) ^ ((lambda - 1)*sign(x.dat$mean))
  sd(x.rat) / mean(x.rat)
}
# ------------------------------------------------------------------------------


.YJ.guerrero.fit <- function(x, lower=-1, upper=2, R=2) 
{  
  optimize(
    f = YJ.guerrero.cv, interval = c(lower, upper), x = x, R = R)$minimum
}
# ------------------------------------------------------------------------------


.YJ.regression.fit <- function(x, R = 2) 
{
  x.dat <- estimate_subseries(x = x, R = R)
  log.sd   <- log(x.dat$sd)
  log.mean <- sign(x.dat$mean) * log(x.dat$mean + 1)
  fit <- lm(log.sd ~ log.mean)               
  1 - as.numeric(coefficients(fit)[2]) 
}
# ------------------------------------------------------------------------------


################################################################################
## Test area
################################################################################

# y <- seq(from = 0.01, to = 5, by = 0.1)
# lambda <- 0
# w <- .BoxCox(x = y, lambda = lambda)
# y1 <- .BoxCox.inv(x = w, lambda = lambda)
# print(sum(abs(y - y1)))
 
# nder <- numDeriv::grad(func = .BoxCox.inv, x = w, lambda = lambda)
# der <- .BoxCox.inv1.der(x = w, lambda = lambda)
# print(sum(abs(nder - der)))

# nder2 <- numDeriv::grad(func = .BoxCox.inv1.der, x = w, lambda = lambda)
# der2 <- .BoxCox.inv1.der2(x = w, lambda = lambda)
# print(sum(abs(nder2 - der2)))

# y <- seq(from = -5, to = 5, by = 0.1)
# lambda <- -2
# w <- .YeoJohnson(x = y, lambda = lambda)
# y1 <- .YeoJohnson.inv(x = w, lambda = lambda)
# plot(x = y, y = w, type = "l")
# print(sum(abs(y - y1)))

# nder <- numDeriv::grad(func = .YeoJohnson.inv, x = w, lambda = lambda)
# der <- .YeoJohnson.inv1.der(x = w, lambda = lambda)
# print(sum(abs(nder - der)))
 
# nder2 <- numDeriv::grad(func = .YeoJohnson.inv1.der, x = w, lambda = lambda)
# der2 <- .YeoJohnson.inv1.der2(x = w, lambda = lambda)
# print(sum(abs(nder2 - der2)))