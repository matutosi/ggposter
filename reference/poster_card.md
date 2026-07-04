# Build a section card: a rounded box with a header tab and content

A *card* is the repeating unit of a poster. It draws a rounded
background box (border in the theme accent), an optional header "tab" in
the accent colour, and a body grob underneath. Every content type (text,
table, figure, image) is wrapped in a card so the poster keeps one
consistent look.

## Usage

``` r
poster_card(body, header = NULL, theme = poster_theme(), header_size = NULL)
```

## Arguments

- body:

  The content. Either a grob, or a ggplot object (converted with
  [`ggplot2::ggplotGrob()`](https://ggplot2.tidyverse.org/reference/ggplotGrob.html)).

- header:

  Header label for the tab. `NULL`, `NA`, or `""` draws no header.

- theme:

  A
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  object.

- header_size:

  Font size of the header, in points. Defaults to
  `theme$base_size * 1.15`.

## Value

A [gtable::gtable](https://gtable.r-lib.org/reference/gtable.html) grob
of class `poster_card`.

## Details

The returned object is a
[gtable::gtable](https://gtable.r-lib.org/reference/gtable.html) (a
grob), which can be placed by
[`patchwork::wrap_elements()`](https://patchwork.data-imaginist.com/reference/wrap_elements.html)
or drawn directly with
[`grid::grid.draw()`](https://rdrr.io/r/grid/grid.draw.html).

## Examples

``` r
body <- grid::textGrob("hello")
card <- poster_card(body, header = "OBJECTIVES", theme = poster_theme())
# grid::grid.newpage(); grid::grid.draw(card)
```
