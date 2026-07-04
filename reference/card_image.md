# Image card content: one or more photos arranged in a row, with optional labels

Reads image files with
[`magick::image_read()`](https://docs.ropensci.org/magick/reference/editing.html),
converts them to raster grobs, and lays them out in a horizontal strip –
e.g. a "dominant species" photo band, as in the sample poster.

## Usage

``` r
card_image(
  files,
  labels = NULL,
  theme = poster_theme(),
  height = NULL,
  width = NULL,
  space = 3,
  label_position = c("below", "inside")
)
```

## Arguments

- files:

  Character vector of image file paths.

- labels:

  Optional character vector of captions, same length as `files`.

- theme:

  A
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  object.

- height:

  Height of each photo, as a
  [grid::unit](https://rdrr.io/r/grid/unit.html) or millimetres. `NULL`
  (default, if `width` is also `NULL`) uses 60mm.

- width:

  Alternative to `height`: a target *total* row width (all photos plus
  the gaps between them). Each photo's height is solved so that, at its
  own aspect ratio, the row sums to this width – useful for making a
  photo band fill a specific fraction of a card's width regardless of
  how many photos it has. Takes precedence over `height` if both are
  given.

- space:

  Gap between photos, as a
  [grid::unit](https://rdrr.io/r/grid/unit.html) or millimetres.

- label_position:

  `"below"` (default) draws each caption in its own row under the photo.
  `"inside"` overlays it near the bottom of the photo itself (white text
  on a semi-transparent bar), so the row's height is just the photo
  height.

## Value

A grob suitable as the `body` argument of
[`poster_card()`](https://matutosi.github.io/ggposter/reference/poster_card.md).

## Examples

``` r
f <- system.file("extdata", "small.JPG", package = "ggposter")
g <- card_image(f, labels = "sample", theme = poster_theme())
```
