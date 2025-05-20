#' Predict Target ASFRs
#'
#' @param forecast_definition A pfs object.
#' @param target_ratio .
#' @param weights A numeric vector of positive weights stating the
#' relative importance of the TFR, MAB, and ratios optimization
#' criteria.
#'
#' @returns A list of target ASFRs and further details regarding the
#' optimization.
#'
#' @export
#'
#' @importFrom stats optim
#'
#' @examples
#' forecast_definition <- DefineForecast(
#'     jumpoff_asfrs = c(0.0031, 0.0269, 0.0671, 0.0888, 0.0512, 0.0134, 0.0012),
#'     forecast_horizon = 30,
#'     ages = c(15, 20, 25, 30, 35, 40, 45),
#'     wlast = 5,
#'     target_tfr = 1.51,
#'     target_mab = 32.9,
#'     asfr_growth_rate = 0.3,
#'     timestep_of_steepest_growth = 15,
#'     randomness = 'finland1995-2024'
#'  )
#' PredictTargetASFRs(forecast_definition)
PredictTargetASFRs <- function (
    forecast_definition,
    # target ratio to initial ASFRs
    target_ratio = c(0.697, 0.842, 0.980, 1.499, 1.888, 1.489, 1.917),
    # weights for the 3 constraints
    weights = c(tfr=10,mab=10,ratios=1)
) {
  # constants
  n <- forecast_definition$n_ages
  w <- forecast_definition$widths
  m <- forecast_definition$midpoints
  y0 <- forecast_definition$jumpoff_asfrs
  target_trf <- forecast_definition$target_tfr
  target_mab <- forecast_definition$target_mab

  # the loss function of the optimization problem
  Loss <- function (delta, n, w) {
    prop_delta <- exp(delta)
    optim_asfrs <- y0*prop_delta
    mab <- t(m)%*%prop.table(optim_asfrs)
    tfr <- t(w)%*%c(optim_asfrs)
    loss <- weights[1]*(tfr-target_trf)^2 +
      weights[2]*(mab-target_mab)^2 +
      sum(delta^2) +
      weights[3]*sum((target_ratio-prop_delta)^2)
    return(c(loss))
  }

  # optimize
  fit <- optim(rep(1, n), Loss, gr = NULL, n, w,
               control = list(maxit = 1e4))

  # results
  ratio_optim <- exp(fit$par)
  asfr_optim <- y0*ratio_optim
  mab_optim <- c(t(m)%*%prop.table(asfr_optim))
  tfr_optim <- c(t(w)%*%c(asfr_optim))
  result <- list(
    optimized_target_asfrs = asfr_optim,
    optimized_target_tfr = tfr_optim,
    optimized_target_mab = mab_optim,
    optimized_target_jumpoff_asfr_ratio = ratio_optim,
    fit = fit
  )

  return(result)
}
