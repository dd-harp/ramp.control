# Make an IRS Round

Return the parameters to make sharkfin function for an irs round with
one of the pesticides in the `irs_profiles` table.

The parameters specify a model for the "killing potential" from the
start of the spray round through the end.

## Usage

``` r
make_irs_shock(irs_type, start_day, peak, elength = 20, pw = 1)
```

## Arguments

- irs_type:

  the name of the IRS type

- start_day:

  the start day for the IRS round

- peak:

  the maximum value

- elength:

  the number of days it took to complete spraying

- pw:

  a shape parameter

## Value

a `sharkbite` function object
