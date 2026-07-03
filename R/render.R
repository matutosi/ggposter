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
#'
#' @return The output file path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' p <- poster(spec)
#' render_poster(p, "poster.pdf")
#' render_poster(p, "preview.png", scale = 0.25)
#' }
render_poster <- function(x, file, scale = 1, dpi = 300) {
  if (!inherits(x, "ggposter")) {
    cli::cli_abort("{.arg x} must be a {.cls ggposter} object, as returned by {.fn poster}.")
  }
  if (scale != 1) x <- rescale_poster(x, scale)

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
