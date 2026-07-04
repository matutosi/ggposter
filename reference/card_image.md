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
  height = 60,
  space = 3
)
```

## Arguments

- files:

  Character vector of image file paths.

- labels:

  Optional character vector of captions, same length as `files`, drawn
  under each photo.

- theme:

  A
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  object.

- height:

  Height of each photo, as a
  [grid::unit](https://rdrr.io/r/grid/unit.html) or millimetres.

- space:

  Gap between photos, as a
  [grid::unit](https://rdrr.io/r/grid/unit.html) or millimetres.

## Value

A grob suitable as the `body` argument of
[`poster_card()`](https://matutosi.github.io/ggposter/reference/poster_card.md).

## Examples

``` r
f <- system.file("extdata", "small.JPG", package = "ggposter")
g <- card_image(f, labels = "sample", theme = poster_theme())
```
