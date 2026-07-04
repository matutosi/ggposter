# Theme (visual settings) for a ggposter poster

`poster_theme()` bundles colours, fonts, and spacing used across a
poster. Every card, title band, and content builder receives the same
theme object so that the whole poster shares one look. Individual values
can be overridden per call.

## Usage

``` r
poster_theme(
  accent = "#2E7D32",
  header_text = "#FFFFFF",
  box_fill = "#FFFFFF",
  box_border = NULL,
  body_text = "#1A1A1A",
  corner_radius = 5,
  border_width = 2,
  base_size = 26,
  base_family = NULL,
  cjk_family = NULL,
  pad = NULL,
  gap = 6,
  lineheight = 0.95,
  content_pad_factor = 1.2
)
```

## Arguments

- accent:

  Accent colour for section-header tabs and box borders.

- header_text:

  Text colour used on the accent-coloured header tab.

- box_fill:

  Fill colour of the section box (card background).

- box_border:

  Border colour of the section box. Defaults to `accent`.

- body_text:

  Colour of body text.

- corner_radius:

  Corner radius of rounded boxes, as a
  [grid::unit](https://rdrr.io/r/grid/unit.html) or a number interpreted
  as millimetres.

- border_width:

  Line width of box borders (in points).

- base_size:

  Base font size in points (for A1 output).

- base_family:

  Font family for Latin text. `NULL` picks a sensible default via
  [`poster_font()`](https://matutosi.github.io/ggposter/reference/poster_font.md).

- cjk_family:

  Font family for CJK (Japanese) text. `NULL` falls back to
  `base_family`.

- pad:

  Inner padding of a card, as a
  [grid::unit](https://rdrr.io/r/grid/unit.html) or millimetres. `NULL`
  (default) uses one line's height of `base_size`/`lineheight` – a
  physical-unit conversion of the font size, not a text measurement, so
  it scales with `base_size` without needing a real output device open.

- gap:

  Gap between stacked cards, as a
  [grid::unit](https://rdrr.io/r/grid/unit.html) or millimetres.

- lineheight:

  Line spacing multiplier for wrapped text (title band and
  [`card_text()`](https://matutosi.github.io/ggposter/reference/card_text.md)),
  passed to [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html). Values
  below 1 (e.g. 0.95) tuck wrapped lines closer together than most
  text-rendering defaults.

- content_pad_factor:

  Multiplier applied to a block of text/table content's own measured
  height to get its enclosing box's height, used wherever a poster
  element sizes itself to fit its content (the title band, and any card
  with `height = "auto"`; see
  [`poster()`](https://matutosi.github.io/ggposter/reference/poster.md)).
  `1.2` means 20% breathing room above/below the content.

## Value

An object of class `poster_theme`.

## Examples

``` r
th <- poster_theme(accent = "#2E7D32")
th$accent
#> [1] "#2E7D32"
```
