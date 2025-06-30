#' Make ASFR/TFR Forecasts
#'
#' @param forecast_definition A pfs object.
#' @param nsim An integer scalar for the number of simulated ASFR/TFR
#' paths.
#'
#' @returns
#' A list with 7 entries
#' @export
#'
#' @importFrom stats quantile
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
#'  MakeForecast(forecast_definition)
MakeForecast <- function (forecast_definition, nsim = 250) {

  if (isTRUE(forecast_definition[['fixed_rates']])) {
    central_asfr_trajectory_matrix <- PredictTrajectoryToTargetASFRs(
      forecast_definition,
      fixed_rates = TRUE
    )
  } else {
    target_asfrs <- PredictTargetASFRs(forecast_definition)
    central_asfr_trajectory_matrix <- PredictTrajectoryToTargetASFRs(
      forecast_definition,
      target_asfrs = target_asfrs$optimized_target_asfrs
    )
  }

  asfr_sim <- SampleRandomWalkASFRs(
    forecast_definition,
    mean_asfr_trajectory_matrix = central_asfr_trajectory_matrix,
    nsim = nsim
  )

  # derive TFRs
  tfr_sim <- apply(
    asfr_sim, c(1, 3),
    function (asfrs) sum(asfrs*forecast_definition$widths)
  )
  tfr_central <- apply(
    central_asfr_trajectory_matrix, 1,
    function (asfrs) sum(asfrs*forecast_definition$widths)
  )

  # get quantiles over simulated forecast paths
  asfr_quantiles <- apply(
    asfr_sim, c(1,2), quantile, probs = c(0.025, 0.05, 0.5, 0.95, 0.975)
  )
  tfr_quantiles <- apply(
    tfr_sim, 1, quantile, probs = c(0.025, 0.05, 0.5, 0.95, 0.975)
  )

  # convert to data frames
  asfr_central_df <- array2DF(central_asfr_trajectory_matrix)
  names(asfr_central_df) <- c('h', 'age', 'asfr')
  asfr_sim_df <- array2DF(asfr_sim)
  names(asfr_sim_df) <- c('h', 'age', 'sim', 'asfr')
  tfr_sim_df <- array2DF(tfr_sim)
  names(tfr_sim_df) <- c('h', 'sim', 'tfr')
  tfr_central_df <- data.frame(h = forecast_definition$h, tfr = tfr_central)
  asfr_quantiles_df <- array2DF(asfr_quantiles)
  names(asfr_quantiles_df) <- c('quantile', 'h', 'age', 'asfr')
  tfr_quantiles_df <- array2DF(tfr_quantiles)
  names(tfr_quantiles_df) <- c('quantile', 'h', 'tfr')

  list(
    asfr_central = asfr_central_df,
    asfr_sim = asfr_sim_df,
    asfr_quantiles = asfr_quantiles_df,
    tfr_central = tfr_central_df,
    tfr_sim = tfr_sim_df,
    tfr_quantiles = tfr_quantiles_df
  )
}
