#' @rdname combine_grobs
#' @export
grob_widths <- function(grobs, convert_to=NULL){
  n <- if(is.grob(grobs)) 1 else length(grobs)
  widths <- grid::unit(rep(1, n), rep("grobwidth", n), grobs)
  if(!is.null(convert_to)) widths <- grid::convertUnit(widths, convert_to)
  return(widths)
}

#' @rdname combine_grobs
#' @export
grob_heights <- function(grobs, convert_to=NULL){
  n <- if(is.grob(grobs)) 1 else length(grobs)
  heights <- grid::unit(rep(1, n), rep("grobheight", n), grobs)
  if(!is.null(convert_to)) heights <- grid::convertUnit(heights, convert_to)
  return(heights)
}
