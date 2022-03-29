#' @export
grob_widths <- function(grobs){
  n <- length(grobs)
  grid::unit(rep(1, n), rep("grobwidth", n), grobs)
}

#' @export
grob_heights <- function(grobs){
  n <- length(grobs)
  grid::unit(rep(1, n), rep("grobheight", n), grobs)
}
