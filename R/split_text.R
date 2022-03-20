#' Wrapper function to generate split textGrob for grid::textGrob. 
#' 
#' @param text      A string.
#'                  label in grid::textGrob(). 
#' @param width     grid::unit
#' @param font_size A numeric of font size.
#'                  fontsize in grid::textGrob(). 
#' @param use       A string.
#'                  name in grid::textGrob(). 
#' @param ...       Others. 
#' @seealso grid::textGrob()
#' @export
as_stg <- function(text, width=NULL, font_size, use=NULL, ...){
  split_text_grob(text=text, width=width, gp=grid::gpar(fontsize=font_size), name=use, ...)
}

#' Wrapper function of converting text into arranged text grob.
#' 
#' @param ...     text.
#' @param width   grid::unit
#' @param silent  Logical. TRUE: no massage, FALSE: shows massage. 
#' @return textGrob
#' @examples
#' txt_title    <- "This is a title."
#' txt_ITEM_1   <- "ITEM text is larger than item text."
#' txt_item_2   <- "This is a item text, too."
#' txt_BODY_1   <- "BODY text is larger than body text."
#' txt_body_2   <- "This is a body text, too."
#' arranged_txt_1 <- arrange_split_txt(txt_title, txt_ITEM_1, txt_item_2)
#' # purrr::map(arranged_txt_1, grid::grid.draw)
#' 
#' @export
arrange_split_txt <- function(..., width=NULL, silent=TRUE){
  tbl <- txt2tibble(...)
  stg <- 
    tbl %>%
    dplyr::mutate(font_size = purrr::pmap_dbl(tbl, get_font_size, silent=silent)) %>%
    dplyr::mutate(width = width) %>%
  #     dplyr::mutate(gp = gp) %>%
  #     dplyr::mutate(just = just) %>%
    purrr::pmap(as_stg)
  #   layout <- tg2layout(stg, width = width)
  #   tg_arrange(tg = tg, layout = layout)
}

#' Split text to fit viewport width
#' 
#' This function is a little bit modified version of splitString in "R Graphics"
#'   Chapter 6: Developing New Graphics Functions and Objects
#'   https://www.stat.auckland.ac.nz/~paul/RGraphics/interactgrid-calcdraw.R
#' @param text      A string. 
#' @param ...       See grid::grob()
#' @param width     grid::unit
#' @param gp        gpar(). 
#' @param name      A string. grob name.
#' @return textGrob
#' @name split_text
#' @examples
#' # grid::grid.newpage()
#' library(grid)
#' t_1 <- "This is a sample text. "
#' t_2 <- "When longer than viewport width, "
#' t_3 <- "text will be split and add a break. "
#' t_4 <- "This text is separated into 5 objects, "
#' t_5 <- "because of R document specification."
#' text <- stringr::str_c(t_1, t_2, t_3, t_4, t_5)
#' fontsize <- 40
#' stg <- splitTextGrob(text, name="test", gp=grid::gpar(fontsize=fontsize))
#' grid.draw(stg)
#' grid::grid.gedit("test", gp=grid::gpar(fontsize=40))
#' 
#' @export
split_string <- function(text, width=grid::unit(1, "npc")){
    # base::split() -> stringr::str_split()
    # base::paste() -> stringr::str_c()
    # "inches"     -> "mm"
    #  availwidth  -> width
    #  width       -> stri_width
  strings    <- stringr::str_split(text, " ")[[1]]
  newstring  <- strings[1]
  linewidth  <- grid::stringWidth(newstring)
  gapwidth   <- grid::stringWidth(" ") 
  width <- grid::convertWidth(width, "mm", valueOnly=TRUE)
  for(i in 2:length(strings)) {
    stri_width <- grid::stringWidth(strings[i])
    if(grid::convertWidth(linewidth + gapwidth + stri_width, "mm", valueOnly=TRUE) < width) {
      sep <- " "
      linewidth <- linewidth + gapwidth + stri_width
    } else {
      sep <- "\n"
      linewidth <- stri_width
    }
    newstring <- stringr::str_c(newstring, strings[i], sep=sep)
  }
  newstring
}

#' Generate split text grob
#' 
#' @rdname split_text
#' @export
split_text_grob <- function(text, width=NULL){
  split_string <- split_string(text, width)
  grid::textGrob(split_string, x=0, y=1, just=c("left", "top"))
}

#' split string
#' 
#' @param text   A string.
#' @export
splitString <- function(text) {
  strings   <- strsplit(text, " ")[[1]]
  newstring <- strings[1]
  linewidth <- grid::stringWidth(newstring)
  gapwidth  <- grid::stringWidth(" ")
  availwidth <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly=TRUE) 
  for (i in 2:length(strings)) {
    width <- grid::stringWidth(strings[i])
    if (grid::convertWidth(linewidth + gapwidth + width, "inches", valueOnly=TRUE) < availwidth) {
      sep <- " "
      linewidth <- linewidth + gapwidth + width
    } else {
      sep <- "\n"
      linewidth <- width
    }
    newstring <- paste(newstring, strings[i], sep=sep)
  }
  newstring
}

#' split string draw
#' 
#' @importFrom grid drawDetails
#' @method drawDetails splitText
#' @param  x         A text grob
#' @param  recording A logical value indicating whether a grob is being added to
#'                   the display list or redrawn from the display list.
#' @export
drawDetails.splitText <- function(x, recording) {
  grid::grid.text(splitString(x$text), x=0, y=1, just=c("left", "top"))
}


#' Add "splitText" class to split text grob
#' 
#' Called when drawDetails are run. 
#' @rdname split_text
#' @export
splitTextGrob <- function(text, ...) {
  grid::grob(text=text, cl="splitText", ...)
}
