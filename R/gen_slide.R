#' Generate slide.
#'
#' @param title A text.
#' @param fig      ggplot object.
#' @param text     A body text
#' @param gp_title gpar of title
#' @param gp_text  gpar of text
#' @param paper    Slide size. default: 'a4'.
#' @param width    Slide width.
#' @param height   Slide height.
#' @param layout   Layout of slide.
#' @param shrink   A numeric.
#' @param name     A name of grob.
#' @param ...      Others.
#'
#' @return Grob
#'
#' @examples
#' # title
#' title <- "THIS IS TITLE WORDS"
#' # gp_title
#' fontsize <- 40
#' gp_title <- grid::gpar(fontsize = fontsize)
#' # figure
#' data("mpg", package = "ggplot2")
#' fig <-
#'   ggplot2::ggplot(mpg) +
#'   ggplot2::geom_point(ggplot2::aes(x = .data[["displ"]], y = .data[["hwy"]])) +
#'   ggplot2::theme_bw()
#' fig # draw ggplot object
#' # text
#' t_1 <- "This is a sample text. "
#' t_2 <- "When longer than width, "
#' t_3 <- "text will be split and add breaks. "
#' t_4 <- "This text is separated into 5 objects, "
#' t_5 <- "because of R document specification."
#' text <- stringr::str_c(t_1, t_2, t_3, t_4, t_5)
#' # gp_text
#' fontsize <- 20
#' lineheight <- 1.1
#' gp_text <- grid::gpar(fontsize = fontsize, lineheight = lineheight)
#' # generate slide and draw
#' slide <- gen_slide(title, fig, text, gp_title, gp_text)
#' grid::grid.draw(slide)
#'
#' @export
gen_slide <- function(title, fig, text, gp_title, gp_text, paper = "", width = grid::unit(1, "npc"),
                      height = grid::unit(1, "npc"), layout = NULL, shrink = 1, name = NULL, ...) {
  # set paper size
  if (paper == "a4") {
    width <- grid::unit(297, "mm")
    height <- grid::unit(210, "mm")
  }
  # viewport
  vp <- grid::viewport(width = width, height = height)
  # textbox_grob
  tbg_title <- gridtext::textbox_grob(
    text = title, halign = 0.5, gp = gp_title, name = "title",
    ...
  )
  tbg_text <- gridtext::textbox_grob(text = text, gp = gp_text, name = "text", ...)
  # layout
  if (is.null(layout)) {
    respect <- rbind(c(0, 0), c(1, 0))
    layout_height <- grid::unit(c(1, 1), c("grobheight", "null"), list(tbg_title, NULL))
    layout <- grid::grid.layout(2, 2, heights = layout_height, respect = respect)
  }
  # ggplot -> grid
  fig <- ggplot2::ggplotGrob(fig)
  # fig <- cowplot::as_grob(fig) frame and place
  if (is.null(name)) {
    name <- grobName(prefix = "slide")
  }
  slide <- grid::frameGrob(layout = layout, name = name, vp = vp)
  slide <- grid::placeGrob(slide, tbg_title, row = 1)
  slide <- grid::placeGrob(slide, fig, row = 2, col = 1)
  slide <- grid::placeGrob(slide, tbg_text, row = 2, col = 2)
  slide
}


#' Fix grob size and shrink cex
#'
#' @param grb          A grob.
#' @param width,height A grid unit.
#' @param shrink   A numeric.
#' @return Grob.
#'
#' @examples
#' data("mpg", package = "ggplot2")
#' fig <-
#'   ggplot2::ggplot(mpg) +
#'   ggplot2::geom_point(ggplot2::aes(x = .data[["displ"]], y = .data[["hwy"]])) +
#'   ggplot2::theme_bw()
#' grb <- ggplot2::ggplotGrob(fig)
#' grb %>%
#'   fix_size(width = grid::unit(80, "mm")) %>%
#'   grid::grid.draw()
#' grid::grid.newpage()
#' grb %>%
#'   fix_size(width = grid::unit(40, "mm"), height = grid::unit(80, "mm")) %>%
#'   grid::grid.draw()
#'
#' @export
fix_size <- function(grb, width = grid::unit(50, "mm"), height = width, shrink = 1) {
  n <- 1
  layout <- grid::grid.layout(n, n, width = width, height = height)
  gp <- grid::gpar(cex = shrink)
  res <- grid::frameGrob(layout = layout, gp = gp)
  grid::placeGrob(res, grb, row = n, col = n)
}

#' Set or shrink font size for ggplot.
#'
#' Helper function of ggplot2::theme().
#' @param gg       A ggplot object.
#' @param fontsize A numeric.
#' @param shrink   A numeric.
#' @return         A ggplot object.
#'
#' @examples
#' data("mpg", package = "ggplot2")
#' fig <-
#'   ggplot2::ggplot(mpg, ggplot2::aes(x = displ, y = hwy)) +
#'   ggplot2::geom_point() +
#'   ggplot2::theme_bw()
#' set_font_size(fig, fontsize = 26)
#' shrink_font(fig, shrink = 0.5)
#'
#' @export
set_font_size <- function(gg, fontsize) {
  gg + ggplot2::theme(text = ggplot2::element_text(size = fontsize))
}

#' @rdname set_font_size
#' @export
shrink_font <- function(gg, shrink) {
  fontsize <- grid::get.gpar()$fontsize * shrink
  set_font_size(gg, fontsize)
}

#' Appose figure and descripion text
#'
#' @param fig      A ggplot object or a grob.
#' @param text     A string or a text grob.
#' @param space    A grid unit. Space between grobs.
#' @param widths   A pair of grid units for fig and text.
#' @param fontsize A numeric.
#' @param shrink   A numeric.
#' @return A combined grob.
#'
#' @export
appose_fig_text <- function(fig, text, 
                            space = grid::unit(1, "mm"), 
                            widths = grid::unit(rep(50, 2), "mm"), 
                            fontsize = NULL, shrink = 1) {
  # set font size
  if (is.null(fontsize)) {
    fontsize <- grid::get.gpar()$fontsize
  }
  # convert fig and fix size
  if (!grid::is.grob(fig)) {
    fig <- set_font_size(fig, fontsize)
    fig <- ggplot2::ggplotGrob(fig)
  }
  fig <- fix_size(fig, width = widths[1], shrink = shrink)
  # convert text
  if (!grid::is.grob(text)) {
    gp <- grid::gpar(fontsize = fontsize * shrink)
    text <- split_text_grob(text, width = widths[2], gp = gp)
  }
  # combine
  #   appose_grobs(fig, text, space = space)
  combine_grobs(fig, text, direction = "horizontal", unify="as_is", space = space)
}
