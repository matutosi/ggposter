# Find an available font family for a poster

Chooses a font family that is actually installed on the system, so that
output renders (and embeds into PDF) without falling back to a
substitute. Latin text prefers clean sans-serif faces; CJK text prefers
a Japanese capable family.

## Usage

``` r
poster_font(cjk = FALSE, preferred = NULL, fallback = "")
```

## Arguments

- cjk:

  If `TRUE`, look for a CJK (Japanese) capable family.

- preferred:

  Character vector of family names to try, in order. If `NULL`, a
  built-in list of common families is used.

- fallback:

  Family name to return when none of `preferred` is installed. Defaults
  to the empty string, which lets the graphics device pick.

## Value

A single font family name (character scalar).

## Examples

``` r
poster_font()
#> [1] "DejaVu Sans"
poster_font(cjk = TRUE)
#> [1] ""
```
