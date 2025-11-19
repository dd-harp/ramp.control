# Make an bednet Effect Size Curve

Return the parameters to make sharkfin function for an bednet round.

## Usage

``` r
make_bednet_efsz_profile(d_50, d_shape, start_day, peak, length = 20, pw = 1)
```

## Arguments

- d_50:

  the day when efficacy reaches 50%

- d_shape:

  the decay shape

- start_day:

  the start day for the bednet round

- peak:

  the maximum value

- length:

  the number of days it took to complete spraying

- pw:

  a shape parameter

## Value

a `sharkbite` function object
