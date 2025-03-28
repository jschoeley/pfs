PFS: Probabilistic Fertility Scenarios
================
Jonas Schöley, Ricarda Duerst, Julia Hellstrand, Mikko Myrskylä

> PFS allows for probabilistic forecasts of age specific fertility rates
> around a pre-specified scenario where a given target TFR and mean age
> at birth are reached at a given time.

Install via:

``` r
devtools::install_github('jschoeley/pfs')
```

Run shiny demo via:

``` r
library(pfs)
DemoPFS()
```

Basic usage:

``` r
library(pfs)

forecast_definition <- DefineForecast(
   jumpoff_asfrs = c(0.0031, 0.0269, 0.0671, 0.0888, 0.0512, 0.0134, 0.0012),
   forecast_horizon = 30,
   ages = c(15, 20, 25, 30, 35, 40, 45),
   wlast = 5,
   target_tfr = 1.51,
   target_mab = 32.9,
   asfr_growth_rate = 0.3,
   timestep_of_steepest_growth = 15,
   randomness = 'finland1995-2024'
)

asfr_tfr_forecast <- MakeForecast(forecast_definition)
head(asfr_tfr_forecast$tfr_quantiles, 10)
```

    ##    quantile h      tfr
    ## 1      2.5% 1 1.161286
    ## 2        5% 1 1.176831
    ## 3       50% 1 1.262489
    ## 4       95% 1 1.359548
    ## 5     97.5% 1 1.375701
    ## 6      2.5% 2 1.119840
    ## 7        5% 2 1.150736
    ## 8       50% 2 1.269195
    ## 9       95% 2 1.411289
    ## 10    97.5% 2 1.430869
