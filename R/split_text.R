  #' Split text to fit viewpeort width
  #' 
  #' This function is a little bit modification of splitString in "R Graphics"
  #'     Chapter 6:  Developing New Graphics Functions and Objects
  #'     https://www.stat.auckland.ac.nz/~paul/RGraphics/interactgrid-calcdraw.R
  #' @param text      A string. 
  #' @param ...       See grid::grob()
  #' @inheritParams   grid::drawDetails
  #' @return textGrob
  #' @name split_text
  #' @examples
  #' # grid::grid.newpage()
  #' t_1 <- "This is a sample text. "
  #' t_2 <- "When longer than viewpeort width, "
  #' t_3 <- "text will be split and add a break. "
  #' t_4 <- "This text is separated into 5 objects, "
  #' t_5 <- "because of R document specification."
  #' text <- stringr::str_c(t_1, t_2, t_3, t_4, t_5)
  #' fontsize <- 16
  #' stg <- splitTextGrob(text, name="test", gp=grid::gpar(fontsize=fontsize))
  #' grid::grid.draw(stg)
  #' 
  #' @export
splitString <- function(text){
  strings    <- stringr::str_split(text, " ")[[1]] # base -> stringr
  newstring  <- strings[1]
  linewidth  <- grid::stringWidth(newstring)
  gapwidth   <- grid::stringWidth(" ")
  availwidth <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly=TRUE)  # "inches" -> "mm"
  for (i in 2:length(strings)) {
    width <- grid::stringWidth(strings[i])            # "inches" -> "mm"
    if (grid::convertWidth(linewidth + gapwidth + width, "mm", valueOnly=TRUE) < availwidth) {
      sep <- " "
      linewidth <- linewidth + gapwidth + width
    } else {
      sep <- "\n"
      linewidth <- width
    }
    newstring <- stringr::str_c(newstring, strings[i], sep=sep) # base -> stringr
  }
  newstring
}

  #' @rdname split_text
  #' @export
splitTextGrob <- function(text, ...) {
  grid::grob(text=text, cl="splitText", ...)
}

  #' @rdname split_text
  #' @export
drawDetails.splitText <- function(x, recording) {
  grid::grid.text(splitString(x$text), x=0, y=1, just=c("left", "top")) 
}
