#' @rdname frame_place_grobs
#' @param convert_to A grid unit.
#' @export
grob_widths <- function(grobs, convert_to=NULL){
  n <- length(grobs)
  widths <- grid::unit(rep(1, n), rep("grobwidth", n), grobs)
  if(!is.null(convert_to)) widths <- grid::convertUnit(widths, convert_to)
  return(widths)
}

#' @rdname frame_place_grobs
#' @param convert_to A grid unit.
#' @export
grob_heights <- function(grobs, convert_to=NULL){
  n <- length(grobs)
  heights <- grid::unit(rep(1, n), rep("grobheight", n), grobs)
  if(!is.null(convert_to)) heights <- grid::convertUnit(heights, convert_to)
  return(heights)
}
