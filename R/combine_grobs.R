#' Combine grobs horizontally or vertically in a line.
#' 
#' appose_grobs() and stack_grobs() are helper function that set arguments 
#' direction ("horizontal" and "vertical") and unify ("as_is").
#' @param ...            grobs.
#' @param direction      A string. "horizontal" or "vertical".
#' @param height,width   A grid unit.
#' @param grow           A logical. Not work yet.
#' @param unify          A string. "width", "height", or "as_is".
#' @param space          A grid unit.
#' @param gp             A gpar() object.
#' @param shrink         A numeric.
#' @param name           A string. grob name.
#' @param grobs           A list of grobs to be combined
#' @param layout          grid layout.
#' @param widths,heights  grid units.
#' @param convert_to      A grid unit.
#' 
#' @return  A combined grobs.
#' @export
combine_grobs <- function(..., direction = "horizontal", 
                                width = NULL, height = NULL, 
                                grow = TRUE, unify = "width", 
                                space = grid::unit(0, "mm"), 
                                gp = grid::gpar(), shrink = 1, name = NULL) {
  grobs <- dots2list(...)
  widths  <- grob_widths(grobs,  convert_to = "mm")
  heights <- grob_heights(grobs, convert_to = "mm")

  # reverse ratio
  ratio_rev <- ratio_reverse(heights = heights, widths = widths, unify=unify)
  # output widths and heights
  widths <- widths * ratio_rev
  heights <- heights * ratio_rev

  # expantion rate
  expantion <- ratio_expantion(height = height, width = width, 
                               heights = heights, widths = widths, space = space,
                               direction = direction)
  widths <- widths * expantion
  heights <- heights * expantion

  # shrink
  widths <- widths * shrink
  heights <- heights * shrink

  if(direction == "horizontal"){
    nrow <- 1
    ncol <- length(grobs)
    layout_heights <- max(heights)
    layout_widths  <- widths + space
  } else if (direction == "vertical"){
    nrow <- length(grobs)
    ncol <- 1
    layout_heights <- heights + space
    layout_widths  <- max(widths)
  } else {
    stop('"direction" should be "horizontal" or "vertical"')
  }
  # layout
  layout <- 
    grid::grid.layout(nrow = nrow, ncol = ncol, heights = layout_heights, widths = layout_widths)
  combined_grobs <- 
    frame_place_grobs(grobs, layout, widths, heights, direction = direction, gp, name)
  combined_grobs
}

#' @rdname combine_grobs
#' @export
appose_grobs <- function(...,  width = NULL, height = NULL, 
                                grow = TRUE, unify = "as_is", 
                                space = grid::unit(0, "mm"), 
                                gp = grid::gpar(), shrink = 1, name = NULL) {
  combine_grobs(
    ..., direction = "horizontal", 
    width, height, grow, unify, space, gp, shrink, name)
}

#' @rdname combine_grobs
#' @export
stack_grobs <- function(...,  width = NULL, height = NULL, 
                                grow = TRUE, unify = "as_is", 
                                space = grid::unit(0, "mm"), 
                                gp = grid::gpar(), shrink = 1, name = NULL) {
  combine_grobs(
    ..., direction = "vertical", 
    width, height, grow, unify, space, gp, shrink, name)
}

#' @rdname combine_grobs
#' @export
frame_place_grobs <- function(grobs, layout, 
                              widths = NULL, heights = NULL, 
                              direction, gp = grid::gpar(), name = NULL) {
  combined_grobs <- grid::frameGrob(layout = layout, name = name)
  col <- 1
  row <- 1
  for (i in seq_along(grobs)) {
    if(!is.null(widths))  grobs[[i]]$width <- widths[[i]]
    if(!is.null(heights)) grobs[[i]]$height <- heights[[i]]
    if (direction == "horizontal") {
      col <- i
    } else {
      row <- i
    }
    combined_grobs <- grid::placeGrob(combined_grobs, grobs[[i]], col = col, row = row)
  }
  combined_grobs
}

#' Compute ratio_rev and ratio_expantion.
#'
#' This function is used in combine_grobs().
#' @name compute_ratio
#' @inheritParams combine_grobs
#' @return        Numerics.
#' @examples
#' heights <- grid::unit(c(10, 20, 40), "mm")
#' widths  <- grid::unit(c(10, 20, 40), "mm")
#' ratio_reverse(widths, heights, unify = "heights")
#' ratio_reverse(widths, heights, unify = "widths")
#'
#' height <- grid::unit(80, "mm")
#' width  <- grid::unit(80, "mm")
#' heights <- grid::unit(c(10, 20, 40), "mm")
#' widths  <- grid::unit(c(10, 20, 40), "mm")
#' space <- grid::unit(10, "mm")
#' ratio_expantion(height = height, heights = heights, widths = widths, 
#'   space = space, direction = "horizontal")
#' ratio_expantion(width  = width,  heights = heights, widths = widths, 
#'   space = space, direction = "vertical")
#'
#' @export
ratio_reverse <- function(heights, widths, unify) {
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

#' @rdname compute_ratio
#' @export
ratio_expantion <- function(height = NULL, width = NULL, 
                            heights, widths, space, direction){
  if(!is.null(height) & !is.null(width)) stop("can NOT use BOTH of height and width!")
  # convert unit
  if (!is.null(height)) height <- grid::convertUnit(height, "mm", valueOnly = TRUE)
  if (!is.null(width))  width  <- grid::convertUnit(width,  "mm", valueOnly = TRUE)
  heights <- grid::convertUnit(heights, "mm", valueOnly = TRUE)
  widths  <- grid::convertUnit(widths,  "mm", valueOnly = TRUE)
  space <- grid::convertUnit(space, "mm", valueOnly = TRUE) * length(heights)

  # default
  expantion <- 1
  if(is.null(height) & is.null(width)) return(expantion)

  if(direction == "horizontal"){
    if (!is.null(width))       expantion <- (width - space) / sum(widths)
    else if (!is.null(height)) expantion <- height          / max(heights)
  } else if(direction == "vertical"){
    if (!is.null(height))     expantion <- (height - space) / sum(heights)
    else if (!is.null(width)) expantion <- width            / max(widths)
  } else {
    message("direction should be 'horizontal' or 'vertical")
  }

  return(expantion)
}
