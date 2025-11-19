# Modify baseline values due to vector control

Implements
[Bed_Net_Effect_Sizes](https://dd-harp.github.io/ramp.control/reference/Bed_Net_Effect_Sizes.md)
for the Le Menach ITN model of vector control

## Usage

``` r
# S3 method for class 'lemenach'
Bed_Net_Effect_Sizes(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state variables vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html)

## See also

[`compute_bednet_effect_sizes_lemenach()`](https://dd-harp.github.io/ramp.control/reference/compute_bednet_effect_sizes_lemenach.md)
