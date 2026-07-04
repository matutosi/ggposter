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
              fontfamily = theme$base_family, col = col,
              lineheight = theme$lineheight)
  }
  wrap_width <- if (is.null(width)) NULL else as_mm_unit(width)
  txt_grob <- function(label, gp, hjust = 0.5) {
    x <- grid::unit(hjust, "npc")
    if (is.null(wrap_width)) {
      grid::textGrob(label, x = x, hjust = hjust, gp = gp)
    } else {
      gridtext::textbox_grob(label, gp = gp, width = wrap_width,
                             halign = hjust, hjust = hjust, x = x)
    }
  }

  # Each row's box is its own measured text height times content_pad_factor,
  # rather than a flat fixed-mm pad -- this is what keeps the whole band's
  # height proportioned to what's actually in it (see the outer padding
  # below, which is now just a thin margin, not additional line spacing).
  row_names   <- character(0)
  row_grobs   <- list()
  row_heights <- list()
  push <- function(name, g, raw_height = NULL) {
    row_names   <<- c(row_names, name)
    row_grobs   <<- c(row_grobs, list(g))
    h <- if (!is.null(raw_height)) raw_height else grid::grobHeight(g) * theme$content_pad_factor
    row_heights <<- c(row_heights, list(h))
  }

  # Authors and affiliations are set at the same size as body text (1.0x);
  # only the title/subtitle are enlarged and funding is shrunk and
  # right-aligned, set apart from the author/affiliation block above it.
  push("title", txt_grob(title, gp(1.9, "bold")))
  if (!is.null(subtitle))     push("subtitle", txt_grob(subtitle, gp(1.2)))
  if (!is.null(authors))      push("authors", txt_grob(authors, gp(1.0)))
  if (!is.null(affiliations)) push("affiliations", txt_grob(affiliations, gp(1.0)))
  if (!is.null(funding)) {
    funding_grob <- txt_grob(funding, gp(0.6), hjust = 1)
    if (length(row_grobs) > 0) {
      # An extra spacer row, sized to duplicate the gap every other pair of
      # rows already gets from their own content_pad_factor padding, so the
      # gap above funding specifically comes out twice as large.
      prev_grob <- row_grobs[[length(row_grobs)]]
      gap_extra <- (grid::grobHeight(prev_grob) + grid::grobHeight(funding_grob)) *
                   (theme$content_pad_factor - 1) / 2
      push("funding_gap", grid::nullGrob(), raw_height = gap_extra)
    }
    push("funding", funding_grob)
  }

  band <- gtable::gtable(widths = grid::unit(1, "null"),
                        heights = do.call(grid::unit.c, row_heights))
  for (i in seq_along(row_grobs)) {
    band <- gtable::gtable_add_grob(band, row_grobs[[i]], t = i, l = 1, name = row_names[[i]])
  }
  band <- gtable::gtable_add_padding(band, grid::unit(c(3, 10, 3, 10), "mm"))

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
