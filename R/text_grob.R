  #' Convert text into tibble. 
  #' 
  #' @param ... Text object(s). Multiple object can be input. 
  #'        Object name of text: foo_keyword or foo_keyword_bar
  #'        examples: txt_title, txt_body_1, proj_ITEM_big
  #'        keyword: "tiny", "script", "footnote", "caption", 
  #'                 "body", "BODY", "item", "ITEM", 
  #'                 "thank", "author", "affil", 
  #'                 "subt", "kw" or "title"
  #' @return Tibble with "text" and "use" colnames. 
  #'         Values of "text" are character of value to be input. 
  #'         Values of "use" are characters of value name to be input. 
  #' @seealso get_font_size(), arrange_txt(), 
  #' @examples
  #' txt_title <- "This is a title."
  #' txt_ITEM_1 <- "ITEM is larger than item."
  #' txt_item_2 <- "This is a item."
  #' txt_BODY_1 <- "BODY is larger than body."
  #' txt_body_2 <- "This is a body text."
  #' txt2tibble(txt_title, txt_ITEM_1, txt_item_2, txt_BODY_1, txt_body_2)
  #' 
  #' @export
txt2tibble <- function(...){
  data.frame(...) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames("text") %>%
    tibble::rownames_to_column("use") %>%
    tibble::remove_rownames() %>%
    tidyr::separate(rlang::sym("use"), into=c(NA, "use"), sep="_", extra="drop", fill="left")
}

  #' Get font size from font_size_list with name or use. 
  #' 
  #' @param base A numeric of base font size: 
  #'             8, 10, 12, 14, 18, 22, 26, 32, or 38)
  #'             If base=NULL, base=26 will be set in get_font_size(). 
  #' @param name A string of font size name. 
  #'             "tiny", "scriptsize", "footnotesize", "small", 
  #'             "normalsize", "large", "Large", "LARGE", 
  #'             "huge", "Huge", or "HUGE"
  #' @param use A string of font size name for use. 
  #'             "tiny", "script", "footnote", "caption", 
  #'             "body", "BODY", "item", "ITEM", "thank",
  #'             "author", "affil", "subt", "kw", or "title"
  #'               thank: acknowledgement
  #'               affil: affiliation
  #'               subt: subtitle
  #'               kw: keyword
  #' @param text   A string, but this argument will be omitted.
  #' @param silent Logical. TRUE: no massage, FALSE: shows massage. 
  #' @return A numeric of font size.
  #' @seealso font_size_list
  #' @examples
  #' get_font_size(base=NULL, name="large", use=NULL)
  #' get_font_size(base=NULL, name="large", use="title")
  #' get_font_size(base=NULL, name=NULL,    use="ITEM")
  #' get_font_size(base=22,   name=NULL,    use="kw")
  #' 
  #' @export
get_font_size <- function(base=NULL, name=NULL, use=NULL, silent=TRUE, text=""){
  if(is.null(base)){
    if(!silent) message("26pt is used for base size.")
    base <- 26
  }
  if(is.null(name) & is.null(use)){
    if(!silent) message("input must be ONE of name or use. 26pt is used.")
    return(26)
  }
  if(!is.null(name) & !is.null(use)){
    if(!silent) message("'use' is used to set font size. Please input one of name OR use.") 
    name <- NULL
  }
  utils::data("font_size_list")
  font <- dplyr::filter(eval(rlang::sym("font_size_list")), .data[["base_size"]]==base)
  if(!is.null(name)) font <- dplyr::filter(font, .data[["font_name"]]==name)
  if(!is.null(use))  font <- dplyr::filter(font, stringr::str_detect(.data[["font_use"]], use))
  font$size
}

  #' Wrapper function to generate textGrob for grid::textGrob. 
  #' 
  #' @param text      A string.
  #'                  label in grid::textGrob(). 
  #' @param x,y       A numeric. Specify x- or y-value.
  #' @param hjust     A numeric 0-1. 0: left, 1: right.
  #' @param font_size A numeric of font size.
  #'                  fontsize in grid::textGrob(). 
  #' @param use       A string.
  #'                  name in grid::textGrob(). 
  #' @param ...       Some more arguments
  #' @seealso grid::textGrob()
  #' @export
as_tg <- function(text, x, y, hjust, vjust, font_size, use, ...){
  grid::textGrob(label=text, x=x, y=y, hjust=hjust, vjust=vjust, gp=grid::gpar(fontsize=font_size), name=use, ...)
}

  #' Split text to fit viewpeort width
  #' 
  #' This function is a little bit modification of splitString in "R Graphics"
  #'   Chapter 6: Developing New Graphics Functions and Objects
  #'   https://www.stat.auckland.ac.nz/~paul/RGraphics/interactgrid-calcdraw.R
  #' @param text      A string. 
  #' @param width     grid::unit()
  #' @param gp        grid::gpar()
  #' @param name      A string. 
  #' @return splitString(): split text by "\\n". split_text_grob: textGrob.
  #' @name split_text_grob
  #' @examples
  #' # grid::grid.newpage()
  #' library(grid)
  #' t_1 <- "This is a sample text. "
  #' t_2 <- "When longer than viewpeort width, "
  #' t_3 <- "text will be split and add a break. "
  #' t_4 <- "This text is separated into 5 objects, "
  #' t_5 <- "because of R document specification."
  #' text <- stringr::str_c(t_1, t_2, t_3, t_4, t_5)
  #' # Split text in the context of current grid.gpar()
  #' splitString(text)
  #' # Generate split textGrob on condisions of arguments.
  #' fontsize <- 40
  #' stg <- split_text_grob(text, gp=gpar(fontsize=40, font=2))
  #' grid.draw(stg)
  #' 
  #' @export
split_text_grob <- function(text, width=NULL, gp=NULL, name=NULL){
  if(is.null(width))         width         <- grid::unit(1, "npc")
  if(is.null(gp$lineheight)) gp$lineheight <- 1.1
  if(is.null(gp$fontsize))   gp$fontsize   <- 12
  vp <- grid::viewport(width=width, gp=gp)
  grid::pushViewport(vp=vp)
  st <- splitString(text)
  grid::popViewport()
  grid::textGrob(label=st, x=0, y=1, just=c("left", "top"), name=name, gp=gp)
}

  #' @rdname split_text_grob
  #' @export
splitString <- function(text){
    # base::split(): stringr::str_split()
    # base::paste(): stringr::str_c()
    # "inches" -> "mm"
  strings    <- stringr::str_split(text, " ")[[1]]
  newstring  <- strings[1]
  linewidth  <- grid::stringWidth(newstring)
  gapwidth   <- grid::stringWidth(" ") 
  availwidth <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly=TRUE)
  for(i in 2:length(strings)) {
    width <- grid::stringWidth(strings[i])
    if(grid::convertWidth(linewidth + gapwidth + width, "mm", valueOnly=TRUE) < availwidth) {
      sep <- " "
      linewidth <- linewidth + gapwidth + width
    } else {
      sep <- "\n"
      linewidth <- width
    }
    newstring <- stringr::str_c(newstring, strings[i], sep=sep)
  }
  newstring
}
