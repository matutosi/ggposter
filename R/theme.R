#' Theme (visual settings) for a ggposter poster
#'
#' `poster_theme()` bundles colours, fonts, and spacing used across a poster.
#' Every card, title band, and content builder receives the same theme object so
#' that the whole poster shares one look. Individual values can be overridden per
#' call.
#'
#' @param accent Accent colour for section-header tabs and box borders.
#' @param header_text Text colour used on the accent-coloured header tab.
#' @param box_fill Fill colour of the section box (card background).
#' @param box_border Border colour of the section box. Defaults to `accent`.
#' @param body_text Colour of body text.
#' @param corner_radius Corner radius of rounded boxes, as a [grid::unit] or a
#'   number interpreted as millimetres.
#' @param border_width Line width of box borders (in points).
#' @param base_size Base font size in points (for A1 output).
#' @param base_family Font family for Latin text. `NULL` picks a sensible
#'   default via [poster_font()].
#' @param cjk_family Font family for CJK (Japanese) text. `NULL` falls back to
#'   `base_family`.
#' @param pad Inner padding of a card, as a [grid::unit] or millimetres.
#' @param gap Gap between stacked cards, as a [grid::unit] or millimetres.
#'
#' @return An object of class `poster_theme`.
#' @export
#' @examples
#' th <- poster_theme(accent = "#2E7D32")
#' th$accent
poster_theme <- function(accent = "#2E7D32",
                         header_text = "#FFFFFF",
                         box_fill = "#FFFFFF",
                         box_border = NULL,
                         body_text = "#1A1A1A",
                         corner_radius = 5,
                         border_width = 2,
                         base_size = 26,
                         base_family = NULL,
                         cjk_family = NULL,
                         pad = 5,
                         gap = 6) {
  if (is.null(box_border)) box_border <- accent
  if (is.null(base_family)) base_family <- poster_font()
  if (is.null(cjk_family)) cjk_family <- poster_font(cjk = TRUE, fallback = base_family)

  structure(
    list(
      accent        = accent,
      header_text   = header_text,
      box_fill      = box_fill,
      box_border    = box_border,
      body_text     = body_text,
      corner_radius = as_mm_unit(corner_radius),
      border_width  = border_width,
      base_size     = base_size,
      base_family   = base_family,
      cjk_family    = cjk_family,
      pad           = as_mm_unit(pad),
      gap           = as_mm_unit(gap)
    ),
    class = "poster_theme"
  )
}

#' @export
print.poster_theme <- function(x, ...) {
  cli::cli_h1("poster_theme")
  cli::cli_dl(c(
    accent      = x$accent,
    box_fill    = x$box_fill,
    box_border  = x$box_border,
    base_family = x$base_family,
    cjk_family  = x$cjk_family,
    base_size   = paste0(x$base_size, " pt")
  ))
  invisible(x)
}

#' A green preset theme resembling the bundled sample poster
#'
#' @inheritParams poster_theme
#' @param ... Further arguments passed to [poster_theme()].
#' @return A `poster_theme` object.
#' @export
theme_green <- function(accent = "#2E7D32", ...) {
  poster_theme(accent = accent, box_border = accent, ...)
}

#' Coerce a number (millimetres) or unit into a grid unit
#' @param x A [grid::unit] or a numeric value interpreted as millimetres.
#' @return A [grid::unit].
#' @keywords internal
#' @noRd
as_mm_unit <- function(x) {
  if (grid::is.unit(x)) x else grid::unit(x, "mm")
}
