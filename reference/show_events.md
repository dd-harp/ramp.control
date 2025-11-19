# Show Events

Show Events

## Usage

``` r
show_events(
  xds_obj,
  mn = 0,
  mx = 1,
  ypos = 0,
  bny = 0,
  irsy = 1,
  bclr = "#E4460AFF",
  iclr = "#4686FBFF",
  add = FALSE
)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- mn:

  the bottom of the segment

- mx:

  the top of the segment

- ypos:

  the position of the interpolation points

- bny:

  the position of bednet labels

- irsy:

  the position of irs labels

- bclr:

  a color for the bednet line segments

- iclr:

  a color for the irs line segments

- add:

  if TRUE, add to an existing plot

## Value

a **`ramp.xds`** model object
