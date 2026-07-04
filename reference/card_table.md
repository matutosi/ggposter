# Table card content: a booktab-style table grob

Builds a table with a title/caption and simple top/bottom/header rules
(booktab style), based on
[`gridExtra::tableGrob()`](https://rdrr.io/pkg/gridExtra/man/tableGrob.html).

## Usage

``` r
card_table(
  x,
  theme = poster_theme(),
  title = NULL,
  caption = NULL,
  rows = NULL,
  width = NULL
)
```

## Arguments

- x:

  A data.frame or tibble.

- theme:

  A
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  object.

- title:

  Optional title text drawn above the table.

- caption:

  Optional caption text drawn below the table.

- rows:

  Passed to
  [`gridExtra::tableGrob()`](https://rdrr.io/pkg/gridExtra/man/tableGrob.html);
  `NULL` (default) hides row names.

- width:

  Target width, as a [grid::unit](https://rdrr.io/r/grid/unit.html) or
  millimetres. If the table is naturally wider, its font size is shrunk
  to fit (with a final clip as a safety net). `NULL` leaves the table at
  its natural size.

## Value

A grob suitable as the `body` argument of
[`poster_card()`](https://matutosi.github.io/ggposter/reference/poster_card.md).

## Examples

``` r
g <- card_table(head(iris), poster_theme(), title = "Iris")
```
