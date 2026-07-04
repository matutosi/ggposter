# Build the full-width title band of a poster

Build the full-width title band of a poster

## Usage

``` r
poster_title(
  title,
  subtitle = NULL,
  authors = NULL,
  affiliations = NULL,
  funding = NULL,
  logo = NULL,
  theme = poster_theme(),
  width = NULL
)
```

## Arguments

- title:

  Main title string.

- subtitle:

  Optional subtitle string.

- authors:

  Optional author list string (e.g. `"*A. One, B. Two"`).

- affiliations:

  Optional affiliations string.

- funding:

  Optional acknowledgement/funding string, drawn smaller.

- logo:

  Optional path to a logo image, placed at the right edge.

- theme:

  A
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  object.

- width:

  Wrap width for all text rows, as a
  [grid::unit](https://rdrr.io/r/grid/unit.html) or millimetres.
  Typically the poster width minus outer margins. `NULL` disables
  wrapping (not recommended for long titles).

## Value

A grob spanning the full poster width.

## Examples

``` r
g <- poster_title("A Title", authors = "*A. Author", theme = poster_theme())
```
