#' Default-if-null operator
#' @param x Value to test.
#' @param y Fallback used when `x` is `NULL`.
#' @return `x` unless it is `NULL`, in which case `y`.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Resolve a named paper size (or explicit width/height) to millimetres
#'
#' @param size Either a recognised paper size name (`"A0"`-`"A4"`) or a numeric
#'   vector `c(width, height)` in millimetres.
#' @param orientation `"portrait"` or `"landscape"`.
#' @return A numeric vector `c(width, height)` in millimetres.
#' @keywords internal
#' @noRd
poster_size <- function(size = "A1", orientation = "portrait") {
  known <- list(
    A0 = c(841, 1189), A1 = c(594, 841), A2 = c(420, 594),
    A3 = c(297, 420),  A4 = c(210, 297)
  )
  wh <- if (is.character(size)) {
    key <- toupper(size)
    if (!key %in% names(known)) {
      cli::cli_abort("Unknown paper size {.val {size}}. Use one of {.val {names(known)}} or c(width, height).")
    }
    known[[key]]
  } else {
    as.numeric(size)
  }
  if (identical(orientation, "landscape")) wh <- rev(wh)
  stats::setNames(wh, c("width", "height"))
}

#' Anchor a grob to the top-left of its parent cell at its natural size
#'
#' Card bodies (text, tables, image strips) sit inside a `"null"`-sized
#' gtable cell that is usually larger than the content. Without an explicit
#' anchor, grid centres the grob in that cell. This wraps `g` in a viewport
#' pinned to the top-left corner, sized to `g`'s own natural width/height
#' (evaluated lazily via [grid::grobWidth()]/[grid::grobHeight()]).
#'
#' Do not use this for ggplot grobs: their internal panel already uses
#' `"null"` units to fill the available space, so anchoring would just clip
#' them to their unexpanded natural size.
#'
#' @param g A grob.
#' @param clip `"on"` or `"off"`, passed to the wrapping [grid::viewport()].
#'   Use `"on"` as a safety net when `g` might still exceed its parent cell
#'   (e.g. a table that could not be shrunk to fit).
#' @return `g` wrapped in a top-left-anchored viewport.
#' @keywords internal
#' @noRd
anchor_top_left <- function(g, clip = "off") {
  vp <- grid::viewport(x = grid::unit(0, "npc"), y = grid::unit(1, "npc"),
                       just = c("left", "top"), clip = clip,
                       width = measure_width(g), height = measure_height(g))
  grid::grobTree(g, vp = vp)
}

#' Measure a grob's width/height, correctly for [gtable::gtable] objects
#'
#' [grid::grobWidth()]/[grid::grobHeight()] can return the wrong (often tiny)
#' size for a `gtable` produced by [gridExtra::tableGrob()] -- some versions
#' carry a stale `vp` that generic grob measurement picks up instead of
#' summing the actual column/row units. [gtable::gtable_width()] and
#' [gtable::gtable_height()] sum the real column/row units and are reliable
#' for any `gtable`; for a plain grob, the grid generics are used instead.
#'
#' @param g A grob or gtable.
#' @return A [grid::unit] scalar.
#' @keywords internal
#' @noRd
measure_width <- function(g) {
  if (inherits(g, "gtable")) gtable::gtable_width(g) else grid::grobWidth(g)
}

#' @rdname measure_width
#' @keywords internal
#' @noRd
measure_height <- function(g) {
  if (inherits(g, "gtable")) gtable::gtable_height(g) else grid::grobHeight(g)
}
