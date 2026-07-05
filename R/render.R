#' Render a poster to PDF or PNG at true size
#'
#' Saves a [poster()] object at its real paper size (e.g. 594x841mm for A1).
#' The output format is chosen from `file`'s extension: `.pdf` uses
#' [grDevices::cairo_pdf] (vector output with CJK glyph embedding), anything
#' else uses [ragg::agg_png].
#'
#' @param x A `ggposter` object, as returned by [poster()].
#' @param file Output file path. Extension determines the device (`.pdf` or
#'   `.png`).
#' @param scale Shrink factor applied to the true paper size, e.g. `0.25` for
#'   an A4-sized preview of an A1 poster. Font/line sizes stay proportional
#'   because the whole canvas (not just DPI) is scaled.
#' @param dpi Resolution for PNG output. Ignored for PDF.
#' @param show_plot_area If `TRUE`, output a version with a dashed border
#'   drawn around each card's content area (header tab, and each body /
#'   notes column), on top of the content without hiding it. Useful for
#'   checking a layout. It's an output option, not part of the poster's
#'   content, so the same `x` renders both the normal and the outlined
#'   version without rebuilding it yourself.
#'
#' @return The output file path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' p <- poster(spec)
#' render_poster(p, "poster.pdf")
#' render_poster(p, "preview.png", scale = 0.25)
#' render_poster(p, "layout-check.png", scale = 0.25, show_plot_area = TRUE)
#' }
render_poster <- function(x, file, scale = 1, dpi = 300, show_plot_area = FALSE) {
  if (!inherits(x, "ggposter")) {
    cli::cli_abort("{.arg x} must be a {.cls ggposter} object, as returned by {.fn poster}.")
  }
  # Rebuild when scaling, or when the plot-area overlay is requested (the
  # borders are baked in at build time, so a plain poster object doesn't
  # carry them). scale = 1 here is an identity rescale that just adds them.
  if (scale != 1 || isTRUE(show_plot_area)) {
    x <- rescale_poster(x, scale, show_plot_area = show_plot_area)
  }

  ext <- tolower(tools::file_ext(file))
  width  <- x$size_mm[["width"]]
  height <- x$size_mm[["height"]]

  if (ext == "pdf") {
    grDevices::cairo_pdf(file, width = width / 25.4, height = height / 25.4,
                         family = x$theme$base_family %||% "sans")
    on.exit(grDevices::dev.off())
    print(x)
  } else {
    ggplot2::ggsave(file, plot = x$patchwork, width = width, height = height,
                    units = "mm", dpi = dpi, device = ragg::agg_png, bg = "white")
  }
  invisible(file)
}
