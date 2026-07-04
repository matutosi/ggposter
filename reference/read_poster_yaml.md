# Read a poster spec from a YAML file

Parses a YAML file into the list structure expected by
[`poster()`](https://matutosi.github.io/ggposter/reference/poster.md).
Only does light validation (presence of `sections` and `layout`); the
content builders raise their own errors for malformed section bodies.

## Usage

``` r
read_poster_yaml(path)
```

## Arguments

- path:

  Path to a YAML file.

## Value

A list following the
[`poster()`](https://matutosi.github.io/ggposter/reference/poster.md)
spec schema.

## Examples

``` r
path <- system.file("extdata", "poster_sample.yml", package = "ggposter")
spec <- read_poster_yaml(path)
```
