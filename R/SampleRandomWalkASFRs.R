#' Sample Random Walk Realizations of ASFRs
#'
#' @param forecast_definition A pfs object.
#' @param mean_asfr_trajectory_matrix A matrix of central ASFR
#' forecasts as output by PredictTrajectoryToTargetASFRs().
#' @param nsim An integer scalar for the number of simulated ASFR paths.
#'
#' @returns
#' A 3d array of sampled ASFR paths.
#' @export
#'
#' @importFrom MASS mvrnorm
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
#' trajectory <-
#'   PredictTrajectoryToTargetASFRs(forecast_definition, target_asfrs)
#' SampleRandomWalkASFRs(forecast_definition, trajectory, nsim = 100)
#'
#' # example with user-provided co-variance matrix
#' forecast_definition$randomness <- 'manual'
#' forecast_definition$covariance <- diag(7)*0.05
#' target_asfrs <-
#'   PredictTargetASFRs(forecast_definition)$optimized_target_asfrs
#' trajectory <-
#'   PredictTrajectoryToTargetASFRs(forecast_definition, target_asfrs)
#' SampleRandomWalkASFRs(forecast_definition, trajectory, nsim = 100)
SampleRandomWalkASFRs <- function (
    forecast_definition,
    mean_asfr_trajectory_matrix,
    nsim
) {
  ages <- forecast_definition$ages
  ages_names <- forecast_definition$ages_names
  n_ages <- forecast_definition$n_ages
  cov_mat <- forecast_definition$covariance_matrix
  H <- forecast_definition$forecast_horizon
  h <- forecast_definition$h

  asfr_paths_sim <- array(
    NA,
    c(H, n_ages, nsim),
    list(h = h, age = ages_names, sim = 1:nsim)
  )

  for (k in 1:nsim) {
    innovations <- mvrnorm(H, rep(0, n_ages), cov_mat)
    cumulative_innovations <- apply(innovations, MARGIN = 2, cumsum)
    # TODO: check if innovations have been modeled over log differences as well
    log_asfr_paths <-
      log(mean_asfr_trajectory_matrix) + cumulative_innovations
    asfr_paths_sim[,,k] <- exp(log_asfr_paths)
  }

  return(asfr_paths_sim)
}
