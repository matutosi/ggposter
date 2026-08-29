#' Read a poster spec from a YAML file
#'
#' Parses a YAML file into the list structure expected by [poster()]. Only
#' does light validation (presence of `sections` and either `layout` or
#' `grid`); the content builders raise their own errors for malformed
#' section bodies.
#'
#' @param path Path to a YAML file.
#' @return A list following the [poster()] spec schema.
#' @export
#' @examples
#' path <- system.file("extdata", "poster_sample.yml", package = "ggposter")
#' spec <- read_poster_yaml(path)
read_poster_yaml <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("YAML file not found: {.path {path}}")
  }
  spec <- yaml::read_yaml(path)
  if (is.null(spec$sections)) {
    cli::cli_abort("Poster YAML must have a top-level {.field sections} map.")
  }
  if (is.null(spec$layout) && is.null(spec$grid)) {
    cli::cli_abort("Poster YAML must have a top-level {.field layout} map (with {.field left}/{.field right}) or a {.field grid} map (with {.field columns}/{.field boxes}).")
  }
  spec
}
