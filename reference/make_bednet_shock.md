# Make a Bed Net Round

Return the parameters to make sharkfin function for an bednet round with
one of the pre-defined profiles in the `bednet_profiles` table.

The parameters specify a model for bed net coverage after a mass
distribution

## Usage

``` r
make_bednet_shock(d_50, d_shape, start_day, peak, length = 20, pw = 1)
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

a `sharkbite` function object
