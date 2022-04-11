#' Show layout and drow grob
#'
#' A simple helper function.
#' @param grob grid grob. 
#' @export
show_layout_and_draw <- function(grob){
  grid::grid.show.layout(grob$frame$layout)
  grid::grid.draw(grob)
}
