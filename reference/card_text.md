# Text card content: a bullet list rendered as markdown

Wraps a character vector of markdown lines (or a single markdown string)
as a body grob using
[`gridtext::textbox_grob()`](https://wilkelab.org/gridtext/reference/textbox_grob.html),
so bold/italic/links work.

## Usage

``` r
card_text(md, theme = poster_theme(), width = NULL)
```

## Arguments

- md:

  Character vector. Each element becomes one paragraph/bullet; they are
  joined with newlines. Markdown syntax (e.g. `**bold**`) is honoured.

- theme:

  A
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  object.

- width:

  Wrap width, as a [grid::unit](https://rdrr.io/r/grid/unit.html) or
  millimetres. `NULL` lets the grob size to its natural width (no
  wrapping).

## Value

A grob suitable as the `body` argument of
[`poster_card()`](https://matutosi.github.io/ggposter/reference/poster_card.md).

## Examples

``` r
g <- card_text(c("- point one", "- point two"), poster_theme())
```
