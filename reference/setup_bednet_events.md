# Setup bednet Events

Setup bednet Events

## Usage

``` r
setup_bednet_events(
  xds_obj,
  start_day,
  type = "pbo",
  event_length = 20,
  coverage = 1,
  contact = 1,
  shock = 1,
  d_50 = 365,
  d_shape = 1/365
)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- start_day:

  the Julian start dates of bednet events

- type:

  the type of net used

- event_length:

  the number of days it took to spray the houses

- coverage:

  maximum effective coverage

- contact:

  maximum effective contact

- shock:

  maximum shock size

- d_50:

  half-coverage parameter

- d_shape:

  coverage shape parameter

## Value

a **`ramp.xds`** model object
