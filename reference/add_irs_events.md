# Add IRS rounds

If dynamic forcing has not already been set up, then turn on dynamic
forcing and set all the

## Usage

``` r
add_irs_events(
  xds_obj,
  start_day,
  pesticides,
  frac_sprayed,
  event_length = 20,
  contact = 1,
  shock = 1
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

a **`xds`** object
