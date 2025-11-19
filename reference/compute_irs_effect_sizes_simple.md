# Modify baseline values due to vector control

Implements a model published in 2007

## Usage

``` r
compute_irs_effect_sizes_simple(ix, ff, qq, gg, contact)
```

## Arguments

- ix:

  an index over nPatches

- ff:

  baseline blood feeding rate

- qq:

  baseline human fraction

- gg:

  baseline mosquito mortality rate

- contact:

  the probability of contact given coverage

## Value

a **`ramp.xds`** model object
