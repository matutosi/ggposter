# Render a poster to PDF or PNG at true size

Saves a
[`poster()`](https://matutosi.github.io/ggposter/reference/poster.md)
object at its real paper size (e.g. 594x841mm for A1). The output format
is chosen from `file`'s extension: `.pdf` uses grDevices::cairo_pdf
(vector output with CJK glyph embedding), anything else uses
[ragg::agg_png](https://ragg.r-lib.org/reference/agg_png.html).

## Usage

``` r
render_poster(x, file, scale = 1, dpi = 300)
```

## Arguments

- x:

  A `ggposter` object, as returned by
  [`poster()`](https://matutosi.github.io/ggposter/reference/poster.md).

- file:

  Output file path. Extension determines the device (`.pdf` or `.png`).

- scale:

  Shrink factor applied to the true paper size, e.g. `0.25` for an
  A4-sized preview of an A1 poster. Font/line sizes stay proportional
  because the whole canvas (not just DPI) is scaled.

- dpi:

  Resolution for PNG output. Ignored for PDF.

## Value

The output file path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- poster(spec)
render_poster(p, "poster.pdf")
render_poster(p, "preview.png", scale = 0.25)
} # }
```
