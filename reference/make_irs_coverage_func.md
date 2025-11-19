# Set up dynamic forcing

If dynamic forcing has not already been set up, then turn on dynamic
forcing and set all the

## Usage

``` r
make_irs_coverage_func(
  options = list(),
  mean = 1,
  F_season = F_flat,
  season_par = list(),
  F_trend = F_flat,
  trend_par = list()
)
```

## Arguments

- options:

  a list of options to override defaults

- mean:

  the mean irs_coverage

- F_season:

  the seasonal signal in irs coverage

- season_par:

  parameters to configure F_season

- F_trend:

  a temporal trend in irs coverage

- trend_par:

  parameters to configure F_trend

## Value

a **`ramp.xds`** model object
