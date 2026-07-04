# Figure card content: a ggplot styled for poster font size

Applies the poster's base font size/family to a ggplot's theme, then
converts it to a grob.

## Usage

``` r
card_figure(gg, theme = poster_theme(), width = NULL, height = NULL)
```

## Arguments

- gg:

  A ggplot object.

- theme:

  A
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  object.

- width, height:

  Target size as a [grid::unit](https://rdrr.io/r/grid/unit.html) or
  millimetres. `NULL` keeps the plot's natural size.

## Value

A grob suitable as the `body` argument of
[`poster_card()`](https://matutosi.github.io/ggposter/reference/poster_card.md).

## Examples

``` r
g <- card_figure(ggplot2::qplot(1, 1), poster_theme())
#> Warning: `qplot()` was deprecated in ggplot2 3.4.0.
```
