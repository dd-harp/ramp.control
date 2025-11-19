# Make an Bed Net Round

Return the parameters to make sharkfin function for a bed net round with
one of the effective coverage profiles in the `bednet_profiles` table.

The parameters specify a model for the "effective coverage" after a mass
bed net distribution

## Usage

``` r
make_bednet_round(d_50, d_shape, start_day, peak, length = 20, pw = 1)
```

## Arguments

- d_50:

  the half-saturation day

- d_shape:

  the shape

- start_day:

  the start day for the bednet round

- peak:

  the maximum value

- length:

  the number of days it took to complete spraying

- pw:

  a shape parameter

## Value

a `sharkfin` function object
