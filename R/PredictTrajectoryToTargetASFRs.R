#' Predict Trajectory to Target ASFRs
#'
#' @param forecast_definition a pfs object
#' @param target_asfrs a numerical vectors of desired ASFRs at target
#' @param fixed_rates should the rates be fixed at jumpoff (default=FALSE)
#'
#' @returns
#' a matrix of central ASFR predictions over the forecasting horizon.
#' columns denote ages, rows denote forecast time steps.
#'
#' @export
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
#' target_asfrs <-
#'   PredictTargetASFRs(forecast_definition)$optimized_target_asfrs
#' PredictTrajectoryToTargetASFRs(forecast_definition, target_asfrs)
PredictTrajectoryToTargetASFRs <- function (
    forecast_definition,
    target_asfrs,
    fixed_rates = FALSE
) {
  y0 <- forecast_definition$jumpoff_asfrs
  ages_names <- forecast_definition$ages_names
  n_ages <- forecast_definition$n_ages
  H <- forecast_definition$forecast_horizon
  h <- forecast_definition$h

  if (isTRUE(fixed_rates)) {
    asfrs_trajectory <- matrix(y0, H, n_ages, byrow = TRUE)
  } else {
    # TODO: restrict domain to forecasting horizon
    halfway <- forecast_definition$timestep_of_steepest_growth
    B <- forecast_definition$asfr_growth_rate

    asfrs_delta <- (y0 - target_asfrs)
    time <- matrix(h, H, n_ages) - halfway
    asfrs_growth <- 1/(1 + exp(B*time))
    asfrs_change_trajectory <- sweep(asfrs_growth, 2, asfrs_delta, "*")
    asfrs_trajectory <- sweep(asfrs_change_trajectory, 2, target_asfrs, "+")
  }
  dimnames(asfrs_trajectory) <- list(h = h, age = ages_names)
  asfrs_trajectory
}
