  #' Appose or stack two grobs or more in a line (row or col).
  #' 
  #' appose_grob() and tack_grob() combine two grobs, 
  #' while appose_grobs() and stack_grobs() can combine more. 
  #' `%oo%` and `%8%` are shortcuts of appose_grob() and tack_grob(). 
  #' 
  #' @name            combine_grobs
  #' @param gx,gy,... grob.
  #' @param name      A string. grob name
  #' @param space     A grid unit. Space among grobs. 
  #' @return          combined (apposed or stacked) grob in a line.
  #' @examples
  #' library(grid)
  #' x <- grid::textGrob("foo_bar", hjust=1, vjust=0)
  #' x2 <- grid::textGrob("foo_bar", x=0, y=1, hjust=1, vjust=1)
  #' y <- gridtext::textbox_grob(
  #'      "textbox_grob generate wrapped text in a box.", 
  #'      width=grid::unit(30, "mm"), box_gp=grid::gpar(col="black"))
  #' z <- gridtext::richtext_grob(
  #'      "richtext_grob can NOT wrap text", 
  #'      box_gp=grid::gpar(col="black"))
  #' ag <- appose_grob(x, y, name="apppsed_grob")
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
  #' grid::grid.draw(x %oo% y %8% z)
  #' grid::grid.newpage()
  #' grid::grid.draw(x %oo% (y %8% z))
  #' grid::grid.newpage()
  #' grid::grid.draw(x2 %oo% (y %8% z))
  #' 
  #' # NOTE: do no work in gridtext package when use "npc" in grid::unit
  #' # library(gridtext)
  #' # a <- gridtext::textbox_grob(
  #' #      "textbox_grob generate wrapped text in a box.", 
  #' #       width=grid::unit(0.5, "npc"), box_gp=grid::gpar(col="black"))
  #' # b <- gridtext::richtext_grob(
  #' #      "richtext_grob can NOT wrap text", 
  #' #      box_gp=grid::gpar(col="black"))
  #' # grid::grid.draw(a %oo% b)
  #' # grid::grid.draw(a %8% b)
  #' 
  #' @export
appose_grob <- function(gx, gy, space=grid::unit(0, "mm"), name=NULL){
    # layout
  widths <- grid::unit(rep(1, 2), rep("grobwidth", 2), list(gx, gy)) + space
  heights <- max(grid::unit(rep(1, 2), rep("grobheight", 2), list(gx, gy)))
  layout <- grid::grid.layout(nrow=1, ncol=2, widths=widths, heights=heights)
    # frame and place
  gx_gy <- grid::frameGrob(layout=layout, name=name)
  gx_gy <- grid::placeGrob(gx_gy, gx, col=1)
  gx_gy <- grid::placeGrob(gx_gy, gy, col=2)
  gx_gy
}

  #' @rdname combine_grobs
  #' @export
stack_grob <- function(gx, gy, space=grid::unit(0, "mm"), name=NULL){
    # layout
  widths <- max(grid::unit(rep(1, 2), rep("grobwidth", 2), list(gx, gy)))
  heights <- grid::unit(rep(1, 2), rep("grobheight", 2), list(gx, gy)) + space
  layout <- grid::grid.layout(nrow=2, ncol=1, widths=widths, heights=heights)
    # frame and place
  gx_gy <- grid::frameGrob(layout=layout, name=name)
  gx_gy <- grid::placeGrob(gx_gy, gx, row=1)
  gx_gy <- grid::placeGrob(gx_gy, gy, row=2)
  gx_gy
}

  #' @rdname combine_grobs
  #' @export
"%oo%" <- function(gx, gy) appose_grob(gx, gy)

  #' @rdname combine_grobs
  #' @export
"%8%" <- function(gx, gy) stack_grob(gx, gy)

  #' @rdname combine_grobs
  #' @export
appose_grobs <- function(..., space=grid::unit(0, "mm"), name=NULL){
  grobs <- list(...)
  n <- length(grobs)
    # layout
  widths <- grid::unit(rep(1, n), rep("grobwidth", n), grobs) + space
  heights <- max(grid::unit(rep(1, n), rep("grobheight", n), grobs))
  layout <- grid::grid.layout(nrow=1, ncol=n, widths=widths, heights=heights)
    # frame and place
  combined_grobs <- grid::frameGrob(layout=layout, name=name)
  for(i in  seq_along(grobs)){
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], col=i)
  }
  combined_grobs
}

  #' @rdname combine_grobs
  #' @export
stack_grobs <- function(..., space=grid::unit(0, "mm"), name=NULL){
  grobs <- list(...)
  n <- length(grobs)
    # layout
  widths <- max(grid::unit(rep(1, n), rep("grobwidth", n), grobs))
  heights <- grid::unit(rep(1, n), rep("grobheight", n), grobs) + space
  layout <- grid::grid.layout(nrow=n, ncol=1, widths=widths, heights=heights)
    # frame and place
  combined_grobs <- grid::frameGrob(layout=layout, name=name)
  for(i in  seq_along(grobs)){
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], row=i)
  }
  combined_grobs
}

  #' Appose or stack two grobs or more in a line (row or col).
  #' 
  #' @param ...          grob.
  #' @param width,height A grid unit. 
  #' @param grow         A string. "height", "width", or "none"
  #'                     Images will be grown to fill the width or height. 
  #'                     Work ONLY when both of width and height are set. 
  #' @param unify        A string.
  #' @param name         A string. grob name
  #' @param space        A grid unit. Space among grobs. 
  #' @return             combined (apposed or stacked) grob in a line.
  #' @examples
  #' library(grid)
  #' small <- rectGrob(width=unit(1, "cm"), height=unit(1, "cm"), gp=gpar(fill="black"))
  #' tall  <- rectGrob(width=unit(1, "cm"), height=unit(2, "cm"), gp=gpar(fill="red"))
  #' wide  <- rectGrob(width=unit(2, "cm"), height=unit(1, "cm"), gp=gpar(fill="blue"))
  #' large <- rectGrob(width=unit(2, "cm"), height=unit(2, "cm"), gp=gpar(fill="white"))
  #' combined_grobs <- 
  #'   (small %oo% tall %oo% wide %oo% large) %8% # original
  #'   appose_image_grobs(small, tall, wide, large, space=grid::unit(1, "mm"))
  #' grid.draw(combined_grobs)
  #' 
  #' @export
appose_image_grobs <- function(..., width=NULL, height=NULL, grow=TRUE, unify="height", space=grid::unit(0, "mm"), name=NULL){
  grobs <- list(...)
  n <- length(grobs)
    # convert width and height
  if(!is.null(width))  width  <- grid::convertUnit(width,  "mm", valueOnly=TRUE)
  if(!is.null(height)) height <- grid::convertUnit(height, "mm", valueOnly=TRUE)
  #   if(!is.null(height) & !is.null(width)) unify <- "grow"

    # widths and heights (convert into simple unit to improve performance)
  widths  <- grid::convertUnit(grid::unit(rep(1, n), rep("grobwidth",  n), grobs), "mm")
  heights <- grid::convertUnit(grid::unit(rep(1, n), rep("grobheight", n), grobs), "mm")

    # reverse ratio
  if(unify=="height") rev_ratio <- reverse_ratio(heights) else 
  if(unify=="width")  rev_ratio <- reverse_ratio(widths)  else
  if(unify=="as_is")  rev_ratio <- 1                      else   # do nothing
  #   if(unify=="grow" )  rev_ratio <- 1                      else { # do nothing
                      rev_ratio <- 1
                      message('no match argument, unify="as_is" was set')
  }
    # output widths and heights
  widths  <- widths  * rev_ratio
  heights <- heights * rev_ratio

    # expantion rate
  expantion <- 1
  if(!is.null(width)){
    expantion <- 
      sum(width - grid::convertUnit(space * n, "mm", valueOnly=TRUE)) / 
      sum(        grid::convertUnit(widths,    "mm", valueOnly=TRUE))
  } else if(!is.null(height)) {
    expantion <- height / max(grid::convertUnit(heights, "mm", valueOnly=TRUE))
  }
  widths  <- widths  * expantion
  heights <- heights * expantion

    # layout
  layout <- grid::grid.layout(nrow=1, ncol=n, widths=widths + space, heights=max(heights))

    # overwrite setting when unify=="grow"
  #   if(unify=="grow"){
  #     widths  <- grid::unit(rep(width/n, n), "mm") - space
  #     heights <- grid::unit(rep(height,  n), "mm")
  #     layout <- grid::grid.layout(nrow=1, ncol=n, widths=widths + space, heights=max(heights))
  #   }

    # frame and place
  combined_grobs <- frame_place_grobs(grobs, layout, widths, heights, space, by_row=TRUE, name)
  combined_grobs
}

  #' Frame and place grobs in line (row or col)
  #' 
  #' This function is used in appose_image_grobs() and stack_image_grobs(). 
  #' @param grobs      grobs to be combined
  #' @param widths     grid unit. 
  #' @param heights    grid unit. 
  #' @param layout     grid layout.
  #' @param space      A grid unit. Space among grobs. 
  #' @param by_row     A logical. TRUE: line in a row. FALSE: line in a col.
  #' @param name       A string. Name of combined grob.
  #' @return           combined grobs by layout.
  #' 
  #' @export
frame_place_grobs <- function(grobs, layout, widths, heights, space, by_row=TRUE, name=NULL){
  # frame_place_grobs <- function(grobs, layout, space, name=NULL){
  combined_grobs <- grid::frameGrob(layout=layout, name=name)
  col <- 1
  row <- 1
  for(i in  seq_along(grobs)){
    grobs[[i]]$width  <- widths[[i]]
    grobs[[i]]$height <- heights[[i]]
    if(by_row) col <- i else row <- i
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], col=col, row=row)
  }
  combined_grobs
}

  #' Compute rev_ratio of length to max length
  #' 
  #' This function is used in appose_image_grobs() and stack_image_grobs(). 
  #' @param lengths    grid units. 
  #' @return           Numerics.
  #' @examples
  #' len <- grid::unit(c(1 ,2, 4), "npc")
  #' reverse_ratio(len)
  #' 
  #' @export
reverse_ratio <- function(lengths){
  grid::convertUnit(max(lengths), "mm", valueOnly=TRUE) / 
  grid::convertUnit(lengths,      "mm", valueOnly=TRUE)
}
