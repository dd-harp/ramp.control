# Modify baseline values due to vector control

Implements
[IRS_Effect_Sizes](https://dd-harp.github.io/ramp.control/reference/IRS_Effect_Sizes.md)
for the Le Menach IRS model of vector control

## Usage

``` r
# S3 method for class 'simple'
IRS_Effect_Sizes(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state variables

- xds_obj:

  a **`ramp.xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html)

## See also

[`compute_irs_effect_sizes_simple()`](https://dd-harp.github.io/ramp.control/reference/compute_irs_effect_sizes_simple.md)
