#' @rdname combine_grobs
#' @export
grob_widths <- function(grobs, convert_to=NULL){
  #   if(length(grobs) == 1){
  #     if(grid::convertUnit(grid::grobWidth(grobs), unitTo = "mm", valueOnly = TRUE) == 0){
  #       if(is.grtree(grobs)) grobs <- grobs$children
  #     }
  #   }
  n <- if(grid::is.grob(grobs)) 1 else length(grobs)
  widths <- grid::unit(rep(1, n), rep("grobwidth", n), grobs)
  if(!is.null(convert_to)) widths <- grid::convertUnit(widths, convert_to)
  return(widths)
}

#' @rdname combine_grobs
#' @export
grob_heights <- function(grobs, convert_to=NULL){
  #   if(length(grobs) == 1){
  #     if(grid::convertUnit(grid::grobHeight(grobs), unitTo = "mm", valueOnly = TRUE) == 0){
  #       if(is.grtree(grobs)) grobs <- grobs$children
  #     }
  #   }
  n <- if(grid::is.grob(grobs)) 1 else length(grobs)
  heights <- grid::unit(rep(1, n), rep("grobheight", n), grobs)
  if(!is.null(convert_to)) heights <- grid::convertUnit(heights, convert_to)
  return(heights)
}

is.grtree <- function (x) inherits(x, "gTree")
