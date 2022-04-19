#' Add a rectGrob behind a grob
#'
#' @param grob                A grid grob.
#' @param width,height,space  A grid unit.
#' @param ...                 Other arguments for rectGrob().
#' @return grob
#'
#' @export
add_rect <- function(grob, 
                     width = NULL, height= NULL, space = NULL, 
                     ...) {
    if(is.null(space))  space <- grid::unit(2, "mm")
    space <- grid::convertUnit(space, unitTo = "mm")
    if(is.null(width))  width  <- grob_widths(grob,  convert_to = "mm") + space
    if(is.null(height)) height <- grob_heights(grob, convert_to = "mm") + space
    rect <- grid::rectGrob(width = width, height = height, ...)
      # To get width and height, set frame and place grob
    layout <- grid::grid.layout(nrow = 1, ncol = 1, heights = height, widths = width)
    frame <- grid::frameGrob(layout = layout)
    grob <- grid::gTree(children = grid::gList(rect, grob))
    grid::placeGrob(frame, grob, col = 1, row = 1)
}
