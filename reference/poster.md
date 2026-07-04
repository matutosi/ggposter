# Build a poster from a declarative spec

Assembles a title band and a multi-column body of section cards into a
single [gtable::gtable](https://gtable.r-lib.org/reference/gtable.html)
sized for a real paper size (default A1 portrait). The spec can be an R
list (built by hand, or by reading YAML with
[`read_poster_yaml()`](https://matutosi.github.io/ggposter/reference/read_poster_yaml.md))
or a path to a YAML file, in which case it is read automatically.

## Usage

``` r
poster(spec, objects = list(), theme = NULL, base_dir = NULL)
```

## Arguments

- spec:

  An R list following the schema above, or a path to a YAML file.

- objects:

  Named list of R objects (ggplots, data.frames, grobs) referenced from
  `sections[[.]]$body$object` in the spec.

- theme:

  A
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  object. If `NULL` (default), it is built from `spec$theme`.

- base_dir:

  Directory used to resolve relative image paths in the spec. Defaults
  to the YAML file's directory, or the working directory for a list
  spec.

## Value

An object of class `ggposter`, a thin wrapper around a
[gtable::gtable](https://gtable.r-lib.org/reference/gtable.html). Print
it to preview, or pass it to
[`render_poster()`](https://matutosi.github.io/ggposter/reference/render_poster.md)
to save a PDF/PNG at true size.

## Details

See the package vignette for the full spec schema. In short:

- `poster`:

  `size` (e.g. `"A1"`) and `orientation`.

- `theme`:

  Arguments passed to
  [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md).

- `title`:

  Arguments passed to
  [`poster_title()`](https://matutosi.github.io/ggposter/reference/poster_title.md).

- `layout`:

  `columns`, `left`, `right`: which section names go in which column,
  top to bottom.

- `sections`:

  Named list; each has `header`, optional relative `height`, and a
  `body` of `type` `"text"`, `"table"`, `"figure"`, or `"image"` plus
  that type's arguments.

## Examples

``` r
spec <- list(
  title = list(title = "Example poster"),
  layout = list(left = "intro", right = "outro"),
  sections = list(
    intro = list(header = "INTRO", body = list(type = "text", md = "- Hello")),
    outro = list(header = "OUTRO", body = list(type = "text", md = "- Bye"))
  )
)
p <- poster(spec)
```
