# Register a font so its exact file is used by graphics devices

Thin convenience wrapper over
[`systemfonts::register_font()`](https://systemfonts.r-lib.org/reference/register_font.html);
useful when a specific TTF/OTF must be embedded (e.g. a lab-mandated
typeface).

## Usage

``` r
poster_register_font(
  family,
  plain,
  bold = plain,
  italic = plain,
  bolditalic = plain
)
```

## Arguments

- family:

  Name to register the font under.

- plain, bold, italic, bolditalic:

  Paths to font files for each face. Only `plain` is required.

## Value

Invisibly, the registered family name.
