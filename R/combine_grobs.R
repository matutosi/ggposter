  #' Appose or stack two grobs or more in a line (row or col).
  #' 
  #' appose_grob() and tack_grob() conbine two grobs, 
  #' while appose_grobs() and stack_grobs() can conbine more. 
  #' `%oo%` and `%8%` are shortcuts of appose_grob() and tack_grob(). 
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
  #' grid::grid.newpage()
  #' grid::grid.draw(y %oo% z)
  #' grid::grid.newpage()
  #' grid::grid.draw(y %8% z)
  #' grid::grid.newpage()
  #' 
  #' @export
appose_grob <- function(x, y){
    # layout
  widths <- grid::unit(rep(1, 2), rep("grobwidth", 2), list(x, y))
  heights <- max(grid::unit(rep(1, 2), rep("grobheight", 2), list(x, y)))
  layout <- grid::grid.layout(nrow=1, ncol=2, widths=widths, heights=heights)
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
  widths <- max(grid::unit(rep(1, 2), rep("grobwidth", 2), list(x, y)))
  heights <- grid::unit(rep(1, 2), rep("grobheight", 2), list(x, y))
  layout <- grid::grid.layout(nrow=2, ncol=1, widths=widths, heights=heights)
    # frame and place
  x_y <- grid::frameGrob(layout=layout)
  x_y <- grid::placeGrob(x_y, x, row=1)
  x_y <- grid::placeGrob(x_y, y, row=2)
  x_y
}

  #' @rdname combine_grobs
  #' @export
appose_grobs <- function(...){
  dots <- list(...)
  n <- length(dots)
    # layout
  widths <- grid::unit(rep(1, n), rep("grobwidth", n), dots)
  heights <- max(grid::unit(rep(1, n), rep("grobheight", n), dots))
  layout <- grid::grid.layout(nrow=1, ncol=n, widths=widths, heights=heights)
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
  widths <- max(grid::unit(rep(1, n), rep("grobwidth", n), dots))
  heights <- grid::unit(rep(1, n), rep("grobheight", n), dots)
  layout <- grid::grid.layout(nrow=n, ncol=1, widths=widths, heights=heights)
    # frame and place
  grobs <- grid::frameGrob(layout=layout)
  for(i in  seq_along(dots)){
    grobs <- grid::placeGrob(grobs, dots[[i]], row=i)
  }
  grobs
}

  #' @rdname combine_grobs
  #' @export
"%oo%" <- function(x, y) appose_grob(x, y)

  #' @rdname combine_grobs
  #' @export
"%8%" <- function(x, y) stack_grob(x, y)
