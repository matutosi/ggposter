
#' Appose or stack two grobs or more in a line (row or col).
#'
#' appose_grob() and tack_grob() combine two grobs,
#' while appose_grobs() and stack_grobs() can combine more.
#' `%oo%` and `%8%` are shortcuts of appose_grob() and tack_grob().
#'
#' @name            combine_grobs
#' @param gx,gy,... grob.
#' @param gp        gpar().
#' @param space     A grid unit. Space between grobs.
#' @param name      A string. grob name
#' @return          combined (apposed or stacked) grob in a line.
#' @examples
#' library(grid)
#' x <- grid::textGrob("foo_bar", hjust = 1, vjust = 0)
#' x2 <- grid::textGrob("foo_bar", x = 0, y = 1, hjust = 1, vjust = 1)
#' y <- gridtext::textbox_grob(
#'   "textbox_grob generate wrapped text in a box.",
#'   width = grid::unit(30, "mm"), box_gp = grid::gpar(col = "black")
#' )
#' z <- gridtext::richtext_grob(
#'   "richtext_grob can NOT wrap text",
#'   box_gp = grid::gpar(col = "black")
#' )
#' ag <- appose_grob(x, y, name = "apppsed_grob")
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
#' # NOTE: do no work in gridtext package when use 'npc' in grid::unit
#' # library(gridtext)
#' # a <- gridtext::textbox_grob(
#' #      'textbox_grob generate wrapped text in a box.',
#' #       width=grid::unit(0.5, 'npc'), box_gp=grid::gpar(col='black'))
#' # b <- gridtext::richtext_grob(
#' #      'richtext_grob can NOT wrap text',
#' #      box_gp=grid::gpar(col='black'))
#' # grid::grid.draw(a %oo% b)
#' # grid::grid.draw(a %8% b)
#'
#' @export
appose_grob <- function(gx, gy, space = grid::unit(0, "mm"), gp = grid::gpar(), name = NULL) {
  # layout
  widths <- grid::unit(rep(1, 2), rep("grobwidth", 2), list(gx, gy)) + space
  heights <- max(grid::unit(rep(1, 2), rep("grobheight", 2), list(gx, gy)))
  layout <- grid::grid.layout(nrow = 1, ncol = 2, widths = widths, heights = heights)
  # frame and place
  gx_gy <- grid::frameGrob(layout = layout, gp = gp, name = name)
  gx_gy <- grid::placeGrob(gx_gy, gx, col = 1)
  gx_gy <- grid::placeGrob(gx_gy, gy, col = 2)
  gx_gy
}

#' @rdname combine_grobs
#' @export
stack_grob <- function(gx, gy, space = grid::unit(0, "mm"), gp = grid::gpar(), name = NULL) {
  # layout
  widths <- max(grid::unit(rep(1, 2), rep("grobwidth", 2), list(gx, gy)))
  heights <- grid::unit(rep(1, 2), rep("grobheight", 2), list(gx, gy)) + space
  layout <- grid::grid.layout(nrow = 2, ncol = 1, widths = widths, heights = heights)
  # frame and place
  gx_gy <- grid::frameGrob(layout = layout, gp = gp, name = name)
  gx_gy <- grid::placeGrob(gx_gy, gx, row = 1)
  gx_gy <- grid::placeGrob(gx_gy, gy, row = 2)
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
appose_grobs <- function(..., space = grid::unit(0, "mm"), gp = grid::gpar(), name = NULL) {
  grobs <- dots2list(...)
  # layout
  widths  <-     grob_widths(grobs) + space
  heights <- max(grob_heights(grobs))
  layout <- grid::grid.layout(nrow = 1, ncol = length(grobs), widths = widths, heights = heights)
  # frame and place
  combined_grobs <- grid::frameGrob(layout = layout, gp = gp, name = name)
  for (i in seq_along(grobs)) {
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], col = i)
  }
  combined_grobs
}

#' @rdname combine_grobs
#' @export
stack_grobs <- function(..., space = grid::unit(0, "mm"), gp = grid::gpar(), name = NULL) {
  grobs <- dots2list(...)
  # layout
  widths  <- max(grob_widths(grobs))
  heights <-     grob_heights(grobs) + space
  layout <- grid::grid.layout(nrow = length(grobs), ncol = 1, widths = widths, heights = heights)
  # frame and place
  combined_grobs <- grid::frameGrob(layout = layout, gp = gp, name = name)
  for (i in seq_along(grobs)) {
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], row = i)
  }
  combined_grobs
}

#' @rdname combine_grobs
#' @export
appose_grobs_conv <- function(..., space = grid::unit(0, "mm"), gp = grid::gpar(), name = NULL) {
  grobs <- dots2list(...)
  # layout
  widths  <-     grob_widths(grobs, convert_to = "mm") + space
  heights <- max(grob_heights(grobs, convert_to = "mm"))
  layout <- grid::grid.layout(nrow = 1, ncol = length(grobs), widths = widths, heights = heights)
  # frame and place
  combined_grobs <- grid::frameGrob(layout = layout, gp = gp, name = name)
  for (i in seq_along(grobs)) {
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], col = i)
  }
  combined_grobs
}

#' @rdname combine_grobs
#' @export
stack_grobs_conv <- function(..., space = grid::unit(0, "mm"), gp = grid::gpar(), name = NULL) {
  grobs <- dots2list(...)
  # layout
  widths  <- max(grob_widths(grobs, convert_to = "mm"))
  heights <-     grob_heights(grobs, convert_to = "mm") + space
  layout <- grid::grid.layout(nrow = length(grobs), ncol = 1, widths = widths, heights = heights)
  # frame and place
  combined_grobs <- grid::frameGrob(layout = layout, gp = gp, name = name)
  for (i in seq_along(grobs)) {
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], row = i)
  }
  combined_grobs
}

#' Appose or stack two grobs or more in a line (row or col).
#'
#' @param ...          grob.
#' @param width,height A grid unit.
#' @param grow         Do not work yet!
#'                     A string. 'height', 'width', or 'none'
#'                     Images will be grown to fill the width or height.
#'                     Work ONLY when both of width and height are set.
#' @name               combine_iamge_grobs
#' @param unify        A string.
#' @param space        A grid unit.
#' @param gp           gpar() object.
#' @param shrink       A numeric.
#' @param name         A string. grob name
#' @return             combined (apposed or stacked) grob in a line.
#' @examples
#' library(grid)
#' small <- rectGrob(width = unit(1, "cm"), height = unit(1, "cm"), gp = gpar(fill = "black"))
#' tall <- rectGrob(width = unit(1, "cm"), height = unit(2, "cm"), gp = gpar(fill = "red"))
#' wide <- rectGrob(width = unit(2, "cm"), height = unit(1, "cm"), gp = gpar(fill = "blue"))
#' large <- rectGrob(width = unit(2, "cm"), height = unit(2, "cm"), gp = gpar(fill = "white"))
#' combined_grobs <-
#'   (small %oo% tall %oo% wide %oo% large) %8% # original
#'   appose_image_grobs(small, tall, wide, large, space = grid::unit(1, "mm"))
#' grid.draw(combined_grobs)
#'
#' @export
appose_image_grobs <- function(..., width = NULL, height = NULL, grow = TRUE, unify = "height", 
                               space = grid::unit(0, "mm"), 
                               gp = grid::gpar(), shrink = 1, name = NULL) {
  grobs <- dots2list(...)

  # convert width and height
  if (!is.null(width))  width <- grid::convertUnit(width,  "mm", valueOnly = TRUE)
  if (!is.null(height)) height <- grid::convertUnit(height, "mm", valueOnly = TRUE)
  # if(!is.null(height) & !is.null(width)) unify <- 'grow' widths and heights (convert into
  # simple unit to improve performance)
  widths <-  grob_widths(grobs, convert_to = "mm")
  heights <- grob_heights(grobs, convert_to = "mm")

  # reverse ratio
  ratio_rev <- ratio_reverse(heights = heights, widths = widths, unify=unify)
  # output widths and heights
  widths <- widths * ratio_rev
  heights <- heights * ratio_rev

  # expantion rate
  expantion <- ratio_expantion(height = height, width = width, 
                               heights = heights, widths = widths, space = space)
  widths <- widths * expantion
  heights <- heights * expantion

  # shrink
  widths <- widths * shrink
  heights <- heights * shrink

  # layout
  layout <- grid::grid.layout(nrow = 1, ncol = length(grobs), widths = widths + space, heights = max(heights))
  # overwrite setting when unify=='grow' 
  # n <- length(grobs)
  # if(unify=='grow'){ 
  #   widths <- grid::unit(rep(width/n, n), 'mm') - space 
  #   heights <- grid::unit(rep(height, n), 'mm') 
  #   layout  <- grid::grid.layout(nrow=1, ncol=n, widths=widths + space, heights=max(heights)) 
  # } 
  # 
  # frame and place
  combined_grobs <- frame_place_grobs(grobs, layout, widths, heights, by_row = TRUE, gp, name)
  combined_grobs
}

#' Frame and place grobs in line (row or col)
#'
#' This function is used in appose_image_grobs() and stack_image_grobs().
#' @param grobs      grobs to be combined
#' @param widths     grid unit.
#' @param heights    grid unit.
#' @param layout     grid layout.
#' @param by_row     A logical. TRUE: line in a row. FALSE: line in a col.
#' @param gp        gpar().
#' @param name       A string. Name of combined grob.
#' @return           combined grobs by layout.
#'
#' @export
frame_place_grobs <- function(grobs, layout, 
                              widths = NULL, heights = NULL, 
                              by_row = TRUE, gp = grid::gpar(), name = NULL) {
  combined_grobs <- grid::frameGrob(layout = layout, name = name)
  col <- 1
  row <- 1
  for (i in seq_along(grobs)) {
    if(!is.null(widths))  grobs[[i]]$width <- widths[[i]]
    if(!is.null(heights)) grobs[[i]]$height <- heights[[i]]
    if (by_row) {
      col <- i
    } else {
      row <- i
    }
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], col = col, row = row)
  }
  combined_grobs
}

#' Compute ratio_rev of length to max length
#'
#' This function is used in appose_image_grobs() and stack_image_grobs().
#' @param heights   A grid units.
#' @param widths    A grid units.
#' @param unify     A string.
#' @return          Numerics.
#' @examples
#' heights <- grid::unit(c(10, 20, 40), "mm")
#' widths  <- grid::unit(c(10, 20, 40), "mm")
#' ratio_reverse(widths, heights, unify = "heights")
#' ratio_reverse(widths, heights, unify = "widths")
#'
#' @export
ratio_reverse <- function(heights, widths, unify = unify) {
  if (unify == "height") {
    lengths <- heights
  } else if (unify == "width") {
    lengths <- widths
  } else if (unify == "as_is") {
    return(1)
  } else {
    message("no match argument, unify=\"as_is\" was set")
    return(1)
  }
  lengths <- grid::convertUnit(lengths, "mm", valueOnly = TRUE)
  return(max(lengths) / lengths)
}

#' Compute ratio_expantion
#'
#' This function is used in appose_image_grobs() and stack_image_grobs().
#' @param height    A grid unit.
#' @param width     A grid unit.
#' @param heights   Grid units.
#' @param widths    Grid units.
#' @param space     A grid unit.
#' @return          Numerics.
#' @examples
#' height <- grid::unit(80, "mm")
#' width  <- grid::unit(80, "mm")
#' heights <- grid::unit(c(10, 20, 40), "mm")
#' widths  <- grid::unit(c(10, 20, 40), "mm")
#' space <- grid::unit(10, "mm")
#'
#' @export
ratio_expantion <- function(height, width, heights, widths, space){
  expantion <- 1
  if(is.null(height) & is.null(width)) return(expantion)
  if (!is.null(height)) {
    n <- length(heights)
    expantion <- 
      sum(
        height - grid::convertUnit(space * n, "mm", valueOnly = TRUE)) / 
             sum(grid::convertUnit(heights, "mm", valueOnly = TRUE
    ))
  } else if (!is.null(width)) {
    expantion <- width / max(grid::convertUnit(widths, "mm", valueOnly = TRUE))
  }
  return(expantion)
}

#' @rdname combine_iamge_grobs
#' @export
stack_image_grobs <- function(..., width = NULL, height = NULL, 
                              grow = TRUE, unify = "width", 
                              space = grid::unit(0, "mm"), 
                              gp = grid::gpar(), shrink = 1, name = NULL) {
  grobs <- dots2list(...)
  # convert width and height
  if (!is.null(width))  width <-  grid::convertUnit(width,  "mm", valueOnly = TRUE)
  if (!is.null(height)) height <- grid::convertUnit(height, "mm", valueOnly = TRUE)
  # if(!is.null(height) & !is.null(width))  unify <- 'grow' 
  #   widths and heights (convert intosimple unit to improve performance)
  widths  <- grob_widths(grobs,  convert_to = "mm")
  heights <- grob_heights(grobs, convert_to = "mm")

  # reverse ratio
  ratio_rev <- ratio_reverse(heights = heights, widths = widths, unify=unify)
  # output widths and heights
  widths <- widths * ratio_rev
  heights <- heights * ratio_rev

  # expantion rate
  expantion <- ratio_expantion(height = height, width = width, 
                               heights = heights, widths = widths, space = space)
  widths <- widths * expantion
  heights <- heights * expantion

  # shrink
  widths <- widths * shrink
  heights <- heights * shrink
  # layout
  layout <- grid::grid.layout(nrow = length(grobs), ncol = 1, heights = heights + space, widths = max(widths))
  # overwrite setting when unify=='grow' 
  # n <- length(grobs)
  # if(unify=='grow'){ widths <- grid::unit(rep(width/n, n), 'mm') - space
  #   heights <- grid::unit(rep(height, n), 'mm') 
  #   layout <-grid::grid.layout(nrow=1, ncol=n, widths=widths + space, heights=max(heights)) 
  # } 
  # frame and place
  combined_grobs <- frame_place_grobs(grobs, layout, widths, heights, by_row = FALSE, gp, name)
  combined_grobs
}
