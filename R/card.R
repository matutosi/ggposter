#' Build a section card: a rounded box with a header tab and content
#'
#' A *card* is the repeating unit of a poster. It draws a rounded background box
#' (border in the theme accent), an optional header "tab" in the accent colour,
#' and a body grob underneath. Every content type (text, table, figure, image)
#' is wrapped in a card so the poster keeps one consistent look.
#'
#' The returned object is a [gtable::gtable] (a grob), which can be placed by
#' [patchwork::wrap_elements()] or drawn directly with [grid::grid.draw()].
#'
#' @param body The content. Either a grob, or a ggplot object (converted with
#'   [ggplot2::ggplotGrob()]).
#' @param header Header label for the tab. `NULL`, `NA`, or `""` draws no header.
#' @param theme A [poster_theme()] object.
#' @param header_size Font size of the header, in points. Defaults to
#'   `theme$base_size * 1.15`.
#'
#' @return A [gtable::gtable] grob of class `poster_card`.
#' @export
#' @examples
#' body <- grid::textGrob("hello")
#' card <- poster_card(body, header = "OBJECTIVES", theme = poster_theme())
#' # grid::grid.newpage(); grid::grid.draw(card)
poster_card <- function(body, header = NULL, theme = poster_theme(),
                        header_size = NULL) {
  body <- as_body_grob(body)
  pad  <- theme$pad
  has_header <- !is.null(header) && !all(is.na(header)) && nzchar(header[[1]])

  bg <- grid::roundrectGrob(
    r  = theme$corner_radius,
    gp = grid::gpar(fill = theme$box_fill, col = theme$box_border,
                    lwd = theme$border_width)
  )

  if (has_header) {
    if (is.null(header_size)) header_size <- theme$base_size * 1.15
    tab <- header_tab(header, theme, header_size)
    tab_h <- tab$height  # read directly: heightDetails.header_tab dispatch is
                         # unreliable once the package has been loaded via
                         # devtools::load_all() (returns 0 for unknown reasons)
    gap <- grid::unit(3, "mm")
    content <- gtable::gtable(
      widths  = grid::unit(1, "null"),
      heights = grid::unit.c(tab_h, gap, grid::unit(1, "null"))
    )
    content <- gtable::gtable_add_grob(content, tab,  t = 1, l = 1, name = "header")
    content <- gtable::gtable_add_grob(content, body, t = 3, l = 1, name = "body")
  } else {
    content <- gtable::gtable(
      widths  = grid::unit(1, "null"),
      heights = grid::unit(1, "null")
    )
    content <- gtable::gtable_add_grob(content, body, t = 1, l = 1, name = "body")
  }

  content <- gtable::gtable_add_padding(content, pad)
  card <- gtable::gtable_add_grob(
    content, bg,
    t = 1, l = 1, b = nrow(content), r = ncol(content),
    z = -Inf, name = "bg"
  )
  class(card) <- c("poster_card", class(card))
  card
}

#' Header tab grob: accent-filled rounded rectangle sized to the label text
#' @keywords internal
#' @noRd
header_tab <- function(label, theme, header_size) {
  gp_txt <- grid::gpar(col = theme$header_text, fontsize = header_size,
                       fontfamily = theme$base_family, fontface = "bold")
  measure <- grid::textGrob(label, gp = gp_txt)
  tw <- grid::grobWidth(measure)  + grid::unit(8, "mm")
  th <- grid::grobHeight(measure) + grid::unit(5, "mm")

  rect <- grid::roundrectGrob(r = grid::unit(2, "mm"),
    gp = grid::gpar(fill = theme$accent, col = NA))
  txt <- grid::textGrob(label, x = grid::unit(4, "mm"), hjust = 0, gp = gp_txt)

  # A left-anchored, top-aligned box sized exactly to the label. Placing it in a
  # viewport whose width/height match the label keeps the tab from stretching to
  # fill the (wider) gtable cell. `height`/`width` are read directly by
  # poster_card() (as `tab$height`/`tab$width`), not via grobHeight()/
  # grobWidth() dispatch: heightDetails/widthDetails S3 methods for a custom
  # gTree class are unreliable once the package has been loaded with
  # devtools::load_all() (they silently return 0), so we avoid the generic
  # entirely rather than documenting and shipping a foot-gun.
  vp <- grid::viewport(x = grid::unit(0, "npc"), y = grid::unit(1, "npc"),
                       just = c("left", "top"), width = tw, height = th)
  grid::gTree(children = grid::gList(rect, txt), vp = vp,
              height = th, width = tw, cl = "header_tab")
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
