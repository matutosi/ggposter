#' Build a section card: a rounded box with a header tab and content
#'
#' A *card* is the repeating unit of a poster. It draws a rounded background box
#' (border in the theme accent), an optional header "tab" in the accent colour,
#' and a body grob underneath. Every content type (text, table, figure, image)
#' is wrapped in a card so the poster keeps one consistent look.
#'
#' The returned object is a [gtable::gtable] (a grob), which can be placed in
#' another gtable or drawn directly with [grid::grid.draw()].
#'
#' @param body The content. Either a grob, or a ggplot object (converted with
#'   [ggplot2::ggplotGrob()]).
#' @param header Header label for the tab. `NULL`, `NA`, or `""` draws no header.
#' @param theme A [poster_theme()] object.
#' @param header_size Font size of the header, in points. Defaults to
#'   `theme$base_size * 1.15`.
#' @param fit_content If `TRUE`, size the body cell to `body`'s own measured
#'   height times `theme$content_pad_factor`, instead of filling however
#'   much vertical space the card is given (the latter is what you want when
#'   the card's height is a relative share of the column, e.g. from
#'   [poster()]'s numeric `section$height`; the former is what
#'   `section$height = "auto"` uses).
#' @param show_plot_area If `TRUE`, draw a dashed border around the header
#'   tab and around the body content, separately, on top of the content
#'   without hiding it. When the body is a table/figure paired with a
#'   bullet-list description (see `with_notes()` in `R/content.R`), the
#'   main content and the notes column each get
#'   their own border instead of one around the combined pair. Useful for
#'   seeing exactly how much of a card each of its parts fills.
#'
#' @return A [gtable::gtable] grob of class `poster_card`.
#' @export
#' @examples
#' body <- grid::textGrob("hello")
#' card <- poster_card(body, header = "OBJECTIVES", theme = poster_theme())
#' # grid::grid.newpage(); grid::grid.draw(card)
poster_card <- function(body, header = NULL, theme = poster_theme(),
                        header_size = NULL, fit_content = FALSE,
                        show_plot_area = FALSE) {
  body <- as_body_grob(body)
  pad  <- theme$pad
  has_header <- !is.null(header) && !all(is.na(header)) && nzchar(header[[1]])
  body_h <- if (fit_content) {
    # A body with a cached "measured_size" (see anchor_top_left()/
    # poster_fix_size()) already has an explicit, deliberately-chosen size
    # -- a table's actual rendered height, or a figure's explicit width/
    # height -- not a tight text measurement that needs extra breathing
    # room, so content_pad_factor is skipped for it (applying it on top of
    # an already-fixed figure height, for example, was compounding into a
    # visibly oversized card).
    if (!is.null(attr(body, "measured_size"))) {
      measure_height(body)
    } else {
      measure_height(body) * theme$content_pad_factor
    }
  } else {
    grid::unit(1, "null")
  }

  bg <- grid::roundrectGrob(
    r  = theme$corner_radius,
    gp = grid::gpar(fill = theme$box_fill, col = theme$box_border,
                    lwd = theme$border_width)
  )

  if (has_header) {
    if (is.null(header_size)) header_size <- theme$base_size * 1.15
    tab <- header_tab(header, theme, header_size, show_plot_area = show_plot_area)
    tab_h <- tab$height  # read directly: heightDetails.header_tab dispatch is
                         # unreliable once the package has been loaded via
                         # devtools::load_all() (returns 0 for unknown reasons)
    gap <- grid::unit(1.5, "mm")
    content <- gtable::gtable(
      widths  = grid::unit(1, "null"),
      heights = grid::unit.c(tab_h, gap, body_h)
    )
    content <- gtable::gtable_add_grob(content, tab,  t = 1, l = 1, name = "header")
    content <- gtable::gtable_add_grob(content, body, t = 3, l = 1, name = "body")
    body_t <- 3
  } else {
    content <- gtable::gtable(
      widths  = grid::unit(1, "null"),
      heights = body_h
    )
    content <- gtable::gtable_add_grob(content, body, t = 1, l = 1, name = "body")
    body_t <- 1
  }

  # A body already carrying its own separate plot-area borders (see
  # with_notes()'s "plot_area_drawn" attribute) draws main/notes borders
  # itself; adding another one around the whole combined body here would
  # just nest a third, redundant border around both of them.
  if (show_plot_area && !isTRUE(attr(body, "plot_area_drawn"))) {
    plot_area <- grid::rectGrob(
      gp = grid::gpar(fill = NA, col = "#FF00FF", lty = "dashed", lwd = 1.2)
    )
    content <- gtable::gtable_add_grob(content, plot_area, t = body_t, l = 1,
                                       z = Inf, name = "plot_area")
  }

  # The gap between the card's own border and the header tab is half the
  # card's usual inner padding; left/right/bottom padding (and top, for a
  # headerless card) stay at the full one-line-height pad.
  top_pad <- if (has_header) pad * 0.5 else pad
  content <- gtable::gtable_add_padding(content, grid::unit.c(top_pad, pad, pad, pad))
  card <- gtable::gtable_add_grob(
    content, bg,
    t = 1, l = 1, b = nrow(content), r = ncol(content),
    z = -Inf, name = "bg"
  )
  class(card) <- c("poster_card", class(card))
  card
}

#' Header tab grob: accent-filled rounded rectangle sized to the label text
#' @param show_plot_area If `TRUE`, draw a dashed border around the tab
#'   itself (`tw` x `th`, not the full-width cell it's placed in by
#'   [poster_card()]), on top of it.
#' @keywords internal
#' @noRd
header_tab <- function(label, theme, header_size, show_plot_area = FALSE) {
  gp_txt <- grid::gpar(col = theme$header_text, fontsize = header_size,
                       fontfamily = theme$base_family, fontface = "bold")
  measure <- grid::textGrob(label, gp = gp_txt)
  # Kept as lazy grobWidth()/grobHeight() units, resolved at draw time
  # against whatever device is actually current then: poster() typically
  # builds the whole poster before render_poster() opens the real output
  # device, so resolving these eagerly here would measure the label against
  # a fallback font-metrics device and (with a fallback narrower than the
  # real font) clip the label inside its own tab.
  tw <- grid::grobWidth(measure)  + grid::unit(4, "mm")
  th <- grid::grobHeight(measure) + grid::unit(2.5, "mm")

  rect <- grid::roundrectGrob(r = grid::unit(2, "mm"),
    gp = grid::gpar(fill = theme$accent, col = NA))
  txt <- grid::textGrob(label, x = grid::unit(2, "mm"), hjust = 0, gp = gp_txt)

  # A left-anchored box sized exactly to the label, so the tab doesn't
  # stretch to fill the (wider) gtable cell it's placed in: a small gtable
  # with an explicit tab-width column and a `"null"` filler column, rather
  # than a `grid::viewport()` push, so the lazy tw/th units above are
  # resolved the same way as any other gtable column/row size.
  #
  # `height`/`width` are read directly by poster_card() (as
  # `tab$height`/`tab$width`), not via grobHeight()/grobWidth() dispatch:
  # heightDetails/widthDetails S3 methods for a custom gtable subclass are
  # unreliable once the package has been loaded with devtools::load_all()
  # (they silently return 0), so we avoid the generic entirely rather than
  # documenting and shipping a foot-gun.
  tab <- gtable::gtable(widths = grid::unit.c(tw, grid::unit(1, "null")), heights = th)
  tab <- gtable::gtable_add_grob(tab, grid::grobTree(rect, txt), t = 1, l = 1, name = "tab")
  if (show_plot_area) {
    plot_area <- grid::rectGrob(
      gp = grid::gpar(fill = NA, col = "#FF00FF", lty = "dashed", lwd = 1.2)
    )
    tab <- gtable::gtable_add_grob(tab, plot_area, t = 1, l = 1, z = Inf, name = "plot_area")
  }
  tab$height <- th
  tab$width <- tw
  class(tab) <- c("header_tab", class(tab))
  tab
}

#' Normalise body content to a grob
#' @keywords internal
#' @noRd
as_body_grob <- function(body) {
  if (inherits(body, "ggplot")) return(ggplot2::ggplotGrob(body))
  if (grid::is.grob(body) || inherits(body, "gTree") || inherits(body, "gDesc")) {
    return(body)
  }
  if (inherits(body, "gtable")) return(body)
  cli::cli_abort("{.arg body} must be a grob or a ggplot object, not {.cls {class(body)}}.")
}

#' @export
print.poster_card <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
