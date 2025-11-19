# Set up Vector Control

Any function that sets up vector control calls this function.

The cases are:

- `none` the object needs to be initialized

- `done` the object has been initialized

If any function sets `vector_control_obj` to `dynamic` then every other
trivial module for every mode of vector control is set up. Otherwise,
nothing happens.

## Usage

``` r
setup_vector_control(xds_obj)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

## Value

a **`ramp.xds`** model object
