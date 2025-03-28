#' Define Fertility Forecast
#'
#' @param jumpoff_asfrs numeric vector of ASFRs in jumpoff year
#' @param forecast_horizon integer scalar defining the number of
#' timesteps to forecast
#' @param ages numeric vector of starting ages for age groups
#' @param wlast width of the last age group
#' @param target_tfr Target TFR
#' @param target_mab Target mean age at birth
#' @param asfr_growth_rate Logistic growth rate of ASFRs over
#' forecasting horizon
#' @param timestep_of_steepest_growth Timesteps into forecast until
#' half of ASFR change has been realized
#' @param randomness Specification of randomness around future ASFRs.
#' One of 'finland1995-2024', 'empirical', 'manual'. See details.
#'
#' @returns
#' A PFS object.
#' @export
#'
#' @examples
#' DefineForecast(
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
DefineForecast <- function (
    jumpoff_asfrs,
    forecast_horizon,
    ages,
    wlast,
    target_tfr,
    target_mab,
    asfr_growth_rate,
    timestep_of_steepest_growth,
    randomness = c('finland1995-2024', 'empirical', 'manual')
) {
  # TODO: Add forecast horizon, and year of target TFR

  # Input checks

  # ages
  stopifnot("`ages` must be a numeric." = is.numeric(ages))
  stopifnot("`ages` must not contain NAs or NaNs." = !anyNA(ages))
  stopifnot("`ages` must be finite." = all(is.finite(ages)))
  stopifnot("`ages` must be a positive." = all(ages>=0))
  names(ages)
  # wlast
  stopifnot("`wlast` must be scalar." = length(wlast)==1L)
  stopifnot("`wlast` must be a numeric." = is.numeric(wlast))
  stopifnot("`wlast` must not contain NAs or NaNs." = !is.na(wlast))
  stopifnot("`wlast` must be finite." = is.finite(wlast))
  stopifnot("`wlast` must be a positive." = wlast>=0)
  # jumpoff_asfrs
  stopifnot("`jumpoff_asfrs` must be same length as `ages`." = length(jumpoff_asfrs)==length(ages))
  stopifnot("`jumpoff_asfrs` must be a numeric." = is.numeric(jumpoff_asfrs))
  stopifnot("`jumpoff_asfrs` must not contain NAs or NaNs." = !is.na(jumpoff_asfrs))
  stopifnot("`jumpoff_asfrs` must be finite." = is.finite(jumpoff_asfrs))
  stopifnot("`jumpoff_asfrs` must be strictly positive." = all(jumpoff_asfrs>0))
  # target_tfr
  stopifnot("`target_tfr` must be scalar." = length(target_tfr)==1L)
  stopifnot("`target_tfr` must be a numeric." = is.numeric(target_tfr))
  stopifnot("`target_tfr` must not contain NAs or NaNs." = !is.na(target_tfr))
  stopifnot("`target_tfr` must be finite." = is.finite(target_tfr))
  stopifnot("`target_tfr` must be strictly positive." = target_tfr>=0)
  # target_mab
  stopifnot("`target_mab` must be scalar." = length(target_mab)==1L)
  stopifnot("`target_mab` must be a numeric." = is.numeric(target_mab))
  stopifnot("`target_mab` must not contain NAs or NaNs." = !is.na(target_mab))
  stopifnot("`target_mab` must be finite." = is.finite(target_mab))
  stopifnot("`target_mab` must be strictly positive." = target_mab>=0)
  # asfr_growth_rate
  stopifnot("`asfr_growth_rate` must be scalar." = length(asfr_growth_rate)==1L)
  stopifnot("`asfr_growth_rate` must be a numeric." = is.numeric(asfr_growth_rate))
  stopifnot("`asfr_growth_rate` must not contain NAs or NaNs." = !is.na(asfr_growth_rate))
  stopifnot("`asfr_growth_rate` must be finite." = is.finite(asfr_growth_rate))
  stopifnot("`asfr_growth_rate` must be positive." = asfr_growth_rate>=0)
  # randomness
  match.arg(arg = randomness, choices = c('finland1995-2024', 'empirical', 'manual'))
  # TODO: add further input type checks

  # derive objects
  if (is.null(names(ages))) {
    names(ages) <- ages
  }
  widths = c(diff(ages), wlast)
  midpoints <- ages + widths/2
  cov_matrix <- switch (
    randomness,
    'finland1995-2024' = preestimated_covariance$finland1995to2024,
    'empirical' = stop("Not yet implemented."),
    'manual' = stop("Not yet implemented.")
  )

  # create analysis object
  structure(
    list(
      jumpoff_asfrs = jumpoff_asfrs,
      forecast_horizon = forecast_horizon,
      h = 1:forecast_horizon,
      ages = ages,
      ages_names = names(ages),
      widths = widths,
      midpoints = midpoints,
      n_ages = length(ages),
      wlast = wlast,
      target_tfr = target_tfr,
      target_mab = target_mab,
      asfr_growth_rate = asfr_growth_rate,
      timestep_of_steepest_growth = timestep_of_steepest_growth,
      covariance_matrix = cov_matrix
    ),
    class = "pfs"
  )
}
