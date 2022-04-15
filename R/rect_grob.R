#' Add a rectGrob behind a grob
#'
#' @param grob                A grid grob.
#' @param width,height,space  A grid unit.
#' @return grob
#'
#' @export
add_rect <- function(grob, 
                     width = NULL, height= NULL, space = NULL, ...) {
  if(is.null(space))  space <- grid::unit(2, "mm")
  space <- grid::convertUnit(space, unitTo = "mm")
  if(is.null(width))  width  <- grob_widths(grob,  convert_to = "mm") + space
  if(is.null(height)) height <- grob_heights(grob, convert_to = "mm") + space
print(width)
print(height)
  rect <- shoot(grid::rectGrob, width, height, ...)
  gTree(children = gList(rect, grob))
}
