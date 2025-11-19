# Implement Vector Control

Implements various forms of vector control. Each mode for vector control
is set up and configured separately.

## Usage

``` r
# S3 method for class 'static'
VectorControl1(t, y, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

## Value

a named [list](https://rdrr.io/r/base/list.html)

## Note

This a junction to implement various modes of vector control. Vector
control modules require
[**`ramp.control`**](https://github.com/dd-harp/ramp.control).
