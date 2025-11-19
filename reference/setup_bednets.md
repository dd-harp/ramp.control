# Set Up a Bed Net Model

This sets up a bed net model

## Usage

``` r
setup_bednets(
  xds_obj,
  access_name = "none",
  access_opts = list(),
  use_name = "none",
  use_opts = list(),
  effects_name = "none",
  effects_opts = list(),
  coverage_name = "none",
  coverage_opts = list(),
  contact_name = "linear",
  contact_opts = list(cp = 1),
  effect_sizes_name = "none",
  effect_sizes_opts = list()
)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- access_name:

  the name of a model for bed net access

- access_opts:

  options for the bed net access model

- use_name:

  the name of a model for bed net usage

- use_opts:

  options for the bed net usage model

- effects_name:

  the name of a model for bed net effects

- effects_opts:

  options for the bed net effects model

- coverage_name:

  the name of a model for bed net coverage

- coverage_opts:

  options for the bed net coverage model

- contact_name:

  the name of a model for bed net contact

- contact_opts:

  options for the bed net contact model

- effect_sizes_name:

  the name of a model for bed net effect sizes

- effect_sizes_opts:

  options for the bed net effect sizes model

## Value

a **`ramp.xds`** model object
