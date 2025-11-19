# Set up dynamic forcing

Set up the model for the effect sizes of insecticide treated nets using
the model by Le Menach, *et al.*

## Usage

``` r
make_bednet_effect_sizes_lemenach(
  options = list(),
  tau0_frac = c(0.68/3, 2.32/3),
  rr = 0.56,
  ss = 0.03
)
```

## Arguments

- options:

  a list of options to override defaults

- tau0_frac:

  a [numeric](https://rdrr.io/r/base/numeric.html) vector giving the
  proportion of a feeding cycle spent host seeking/bloodfeeding *vs.*
  resting/oviposition

- rr:

  probability of mosquito being repelled upon contact with ITN

- ss:

  probability of mosquito successfully feeding upon contact with ITN

## Value

a bed net effect size model object
