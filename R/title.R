#' Build the full-width title band of a poster
#'
#' @param title Main title string.
#' @param subtitle Optional subtitle string.
#' @param authors Optional author list string (e.g. `"*A. One, B. Two"`).
#' @param affiliations Optional affiliations string.
#' @param funding Optional acknowledgement/funding string, drawn smaller.
#' @param logo Optional path to a logo image, placed at the right edge.
#' @param theme A [poster_theme()] object.
#' @param width Wrap width for all text rows, as a [grid::unit] or
#'   millimetres. Typically the poster width minus outer margins. `NULL`
#'   disables wrapping (not recommended for long titles).
#'
#' @return A grob spanning the full poster width.
#' @export
#' @examples
#' g <- poster_title("A Title", authors = "*A. Author", theme = poster_theme())
poster_title <- function(title, subtitle = NULL, authors = NULL,
                         affiliations = NULL, funding = NULL, logo = NULL,
                         theme = poster_theme(), width = NULL) {
  gp <- function(mult, face = "plain", col = theme$header_text) {
    grid::gpar(fontsize = theme$base_size * mult, fontface = face,
              fontfamily = theme$base_family, col = col)
  }
  wrap_width <- if (is.null(width)) NULL else as_mm_unit(width)
  txt_grob <- function(label, ...) {
    if (is.null(wrap_width)) {
      grid::textGrob(label, ...)
    } else {
      gridtext::textbox_grob(label, gp = list(...)$gp, width = wrap_width,
                             halign = 0.5, hjust = 0.5, x = grid::unit(0.5, "npc"))
    }
  }

  rows <- list()
  rows$title <- txt_grob(title, gp = gp(1.9, "bold"))
  if (!is.null(subtitle))     rows$subtitle     <- txt_grob(subtitle, gp = gp(1.2))
  if (!is.null(authors))      rows$authors      <- txt_grob(authors, gp = gp(1.0))
  if (!is.null(affiliations)) rows$affiliations <- txt_grob(affiliations, gp = gp(0.85))
  if (!is.null(funding))      rows$funding      <- txt_grob(funding, gp = gp(0.6))

  heights <- lapply(rows, function(g) grid::grobHeight(g) + grid::unit(4, "mm"))
  band <- gtable::gtable(widths = grid::unit(1, "null"),
                        heights = do.call(grid::unit.c, heights))
  for (i in seq_along(rows)) {
    band <- gtable::gtable_add_grob(band, rows[[i]], t = i, l = 1, name = names(rows)[[i]])
  }
  band <- gtable::gtable_add_padding(band, grid::unit(c(8, 10, 8, 10), "mm"))

  bg <- grid::rectGrob(gp = grid::gpar(fill = theme$accent, col = NA))
  band <- gtable::gtable_add_grob(band, bg, t = 1, l = 1, b = nrow(band), r = ncol(band),
                                 z = -Inf, name = "bg")

  if (!is.null(logo)) {
    img <- magick::image_read(logo)
    rg  <- grid::rasterGrob(grDevices::as.raster(img), x = grid::unit(1, "npc") - grid::unit(10, "mm"),
                            hjust = 1, width = grid::unit(30, "mm"))
    band <- gtable::gtable_add_grob(band, rg, t = 1, b = nrow(band), l = ncol(band),
                                   z = Inf, name = "logo")
  }
  band
}
