  #' @rdname combine_grobs
  #' @export
appose_grobs <- function(...){
  dots <- list(...)
  n <- length(dots)
    # layout
  widths <- grid::unit(rep(1, n), rep("grobwidth", n), dots)
  layout <- grid::grid.layout(nrow=1, ncol=n, widths=widths)
    # frame and place
  grobs <- grid::frameGrob(layout=layout)
  for(i in  seq_along(dots)){
    grobs <- grid::placeGrob(grobs, dots[[i]], col=i)
  }
  grobs
}

  #' @rdname combine_grobs
  #' @export
stack_grobs <- function(...){
  dots <- list(...)
  n <- length(dots)
    # layout
  heights <- grid::unit(rep(1, n), rep("grobheight", n), dots)
  layout <- grid::grid.layout(nrow=n, ncol=1, heights=heights)
    # frame and place
  grobs <- grid::frameGrob(layout=layout)
  for(i in  seq_along(dots)){
    grobs <- grid::placeGrob(grobs, dots[[i]], row=i)
  }
  grobs
}

  #' Appose or stack two grobs in a line.
  #' 
  #' @name combine_grobs
  #' @param x,y,... grob
  #' @return apposed or stack grob in a line
  #' @examples
  #' x <- grid::textGrob("foo_bar")
  #' y <- grid::textGrob("xxx_yyy")
  #' z <- grid::textGrob("hogehoge")
  #' ag <- appose_grob(x, y)
  #' sg <- stack_grob(x, y)
  #' ags <- appose_grobs(x, y, z)
  #' sgs <- stack_grobs(x, y, z)
  #' grid::grid.draw(ag)
  #' grid::grid.newpage()
  #' grid::grid.draw(sg)
  #' grid::grid.newpage()
  #' grid::grid.draw(ags)
  #' grid::grid.newpage()
  #' grid::grid.draw(sgs)
  #' 
  #' @export
appose_grob <- function(x, y){
    # layout
  widths <- grid::unit(rep(1, 2), rep("grobwidth", 2), list(x, y))
  layout <- grid::grid.layout(nrow=1, ncol=2, widths=widths)
    # frame and place
  x_y <- grid::frameGrob(layout=layout)
  x_y <- grid::placeGrob(x_y, x, col=1)
  x_y <- grid::placeGrob(x_y, y, col=2)
  x_y
}

  #' @rdname combine_grobs
  #' @export
stack_grob <- function(x, y){
    # layout
  heights <- grid::unit(rep(1, 2), rep("grobheight", 2), list(x, y))
  layout <- grid::grid.layout(nrow=2, ncol=1, heights=heights)
    # frame and place
  x_y <- grid::frameGrob(layout=layout)
  x_y <- grid::placeGrob(x_y, x, row=1)
  x_y <- grid::placeGrob(x_y, y, row=2)
  x_y
}


  #' Wrapper function of converting text into arranged text grob.
  #'
  #' @inheritParams txt2tibble
  #' @param hjust   A numeric 0-1. 1: left", 0: "right"
  #' @param widths  grid::unit
  #' @param silent  Logical. TRUE: no massage, FALSE: shows massage.
  #' @return        textGrob
  #' @examples
  #' txt_title    <- "This is a title."
  #' txt_ITEM_1   <- "ITEM text is larger than item text."
  #' txt_item_2   <- "This is a item text, too."
  #' txt_BODY_1   <- "BODY text is larger than body text."
  #' txt_body_2   <- "This is a body text, too."
  #' arranged_txt_1 <- arrange_txt(txt_title, txt_ITEM_1, txt_item_2)
  #' grid::grid.draw(arranged_txt_1)
  #' arranged_txt_2 <- arrange_txt(txt_BODY_1, txt_body_2, hjust=1)
  #' grid::grid.draw(arranged_txt_2)
  #'
  #' @export
arrange_txt <- function(..., hjust=1, widths=grid::unit(841, "mm"), silent=TRUE){
  tbl <- txt2tibble(...)
  tg <-
    tbl %>%
    dplyr::mutate(font_size = purrr::pmap_dbl(tbl, get_font_size, silent=silent)) %>%
    dplyr::mutate(hjust=hjust) %>%
    purrr::pmap(as_tg)
  layout <- tg2layout(tg, widths=widths)
  tg_arrange(tg=tg, layout=layout)
}

  #' Create layout to fit height and width with text grob.
  #'
  #' @param tg A text grob
  #' @param widths grid::unit
  #' @param heights A numeric. Default is null to fit text grob.
  #' @param row_margin A numeric. Default is 1.5. This makes rows 1.5 heights of text grobs.
  #'
  #' @return Lauyout grob
  #'
  #' @export
tg2layout <- function(tg, widths=grid::unit(841, "mm"), heights=NULL, row_margin=1.5  # height = height * row_margin
  ){
  nrow <- length(tg)
  grid::grid.layout(nrow, 1, widths=widths, heights=grid::unit(rep(1.5, nrow), "grobheight", tg))
}

  #' Arrange text grob with layout.
  #'
  #' @param tg A text grob
  #' @param layout A Grid layout, or NULL.
  #'        This can be used to initialise the frame with a number of rows.
  #' @param name A character identifier of frame name.
  #'
  #' @export
tg_arrange <- function(tg, layout, name=NULL){
  res <- grid::frameGrob(layout=layout, name=name)
  for(i in seq_along(tg)) res <- grid::placeGrob(res, tg[[i]], row=i)
  res
}
