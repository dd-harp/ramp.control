# Setup IRS Events

Setup IRS Events

## Usage

``` r
setup_irs_events(
  xds_obj,
  start_day,
  pesticides,
  frac_sprayed = 0.5,
  event_length = 20,
  contact = 0.5,
  shock = 0.5
)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- start_day:

  the Julian start dates of IRS events

- pesticides:

  the pesticide used

- frac_sprayed:

  the fraction of houses sprayed (if known)

- event_length:

  the number of days it took to spray the houses

- contact:

  maximum effective contact

- shock:

  maximum shock size

## Value

a **`ramp.xds`** model object
