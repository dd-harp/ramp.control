# Make Multiple Rounds of msat

Using information about the events (see
[setup_mass_treat_events](https://dd-harp.github.io/ramp.control/reference/setup_mass_treat_events.md)),
including a parameter describing access, this makes a function that
computes treatage over time.

## Usage

``` r
make_mass_treat_multiround(xds_obj, screen)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- screen:

  a switch

## Value

a **`xds`** object
