#' Generate slide.
#' 
#' @param title A text.
#' @param fig      ggplot object.
#' @param text     A body text
#' @param gp_title gpar of title
#' @param gp_text  gpar of text
#' @param paper    Slide size. default: "a4".
#' @param width    Slide width.
#' @param height   Slide height.
#' @param layout   Layout of slide.
#' @param name     A name of grob.
#' 
#' @return Grob
#' 
#' @examples
#'   # title
#' title <- "THIS IS TITLE WORDS"
#'   # gp_title
#' fontsize <- 40
#' gp_title <- grid::gpar(fontsize=fontsize)
#'   # figure
#' data("mpg", package="ggplot2")
#' fig <- 
#'   ggplot2::ggplot(mpg) + 
#'   ggplot2::geom_point(ggplot2::aes(x = .data[["displ"]], y = .data[["hwy"]])) +
#'   ggplot2::theme_bw()
#' fig  # draw ggplot object
#'   # text
#' t_1 <- "This is a sample text. "
#' t_2 <- "When longer than width, "
#' t_3 <- "text will be split and add breaks. "
#' t_4 <- "This text is separated into 5 objects, "
#' t_5 <- "because of R document specification."
#' text <- stringr::str_c(t_1, t_2, t_3, t_4, t_5)
#'   # gp_text
#' fontsize <- 20
#' lineheight <- 1.1
#' gp_text <- grid::gpar(fontsize=fontsize, lineheight=lineheight)
#'   # generate slide and draw
#' slide <- gen_slide(title, fig, text, gp_title, gp_text)
#' grid::grid.draw(slide)
#' 
#' @export
gen_slide <- function(title, fig, text, gp_title, gp_text, paper="", width=unit(1, "npc"), height=unit(1, "npc"), layout=NULL, name=NULL, ...){
    # set paper size
  if(paper=="a4"){
    width  <- grid::unit(297, "mm")
    height <- grid::unit(210, "mm")
  }
    # viewport
  vp <- grid::viewport(width=width, height=height)
    # textbox_grob
  tbg_title <- gridtext::textbox_grob(text=title, halign=0.5, gp=gp_title, name="title", ...)
  tbg_text  <- gridtext::textbox_grob(text=text,              gp=gp_text,  name="text", ...)
    # layout
  if(is.null(layout)){
    respect <- rbind(c(0,0), c(1,0))
    layout_height <- grid::unit(c(1, 1), c("grobheight", "null"), list(tbg_title, NULL))
    layout <- grid::grid.layout(2, 2, heights=layout_height, respect=respect)
  }
    #  ggplot -> grid
  fig <- ggplot2::ggplotGrob(fig)
  #   fig <- cowplot::as_grob(fig)
    # frame and place
  if(is.null(name)) name <- grobName(prefix="slide")
  slide <- grid::frameGrob(layout=layout, name=name, vp=vp)
  slide <- grid::placeGrob(slide, tbg_title, row=1)
  slide <- grid::placeGrob(slide, fig,       row=2, col=1)
  slide <- grid::placeGrob(slide, tbg_text,  row=2, col=2)
  slide
}
