  #' Wrapper function of converting text into arranged text grob.
  #' 
  #' @param ... Text object(s). Multiple object can be input. 
  #'        Object name of text: foo_useword or foo_keyword_bar
  #'        examples: txt_title, txt_body_1, proj_ITEM_big
  #'        useword: "tiny", "script", "footnote", "caption", 
  #'                 "body", "BODY", "item", "ITEM", 
  #'                 "thank", "author", "affil", 
  #'                 "subt", "kw", or "title"
  #' @return textGrob
  #' @examples
  #' txt_title    <- "This is a title."
  #' txt_ITEM_1   <- "ITEM text is larger than item text."
  #' txt_item_2   <- "This is a item text, too."
  #' txt_BODY_1   <- "BODY text is larger than body text."
  #' txt_body_2   <- "This is a body text, too."
  #' arranged_txt <- arrange_txt(txt_title, txt_ITEM_1, txt_item_2, txt_BODY_1, txt_body_2)
  #' grid::grid.draw(arranged_txt)
  #' 
  #' @export
arrange_txt <- function(...){
  tg <- 
    txt2tibble(...) %>%
    purrr::pmap(as_tg)
  layout <- tg2layout(tg)
  tg_arrange(tg=tg, layout=layout)
}

  #' Convert text to text grob.  
  #' 
  #' @param text      A string. 
  #' @param font_size A numeric of font size.
  #' @param base A numeric of base font size: 
  #'             8, 10, 12, 14, 18, 22, 26, 32, or 38)
  #'             See font_size_list. 
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
  #'
  #' @export
as_tg <- function(text, font_size=NULL, base=NULL, name=NULL, use=NULL){
    # check input arguments
  if(!check_text(text))         return(NULL)
  if(!check_text(use))          return(NULL)
  if(!check_text(name))         return(NULL)
  if(!check_numeric(font_size)) return(NULL)
  if(!check_numeric(base))      return(NULL)
    # font size
  if(is.null(font_size)){
    font_size <- get_font_size(base, name, use)
  }
    # make grob
  grid::textGrob(label=text, gp=grid::gpar(fontsize=font_size), name=use)
}

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
  #' txt_ITEM_1 <- "ITEM text is larger than item text."
  #' txt_item_2 <- "This is a item text, too."
  #' txt_BODY_1 <- "BODY text is larger than body text."
  #' txt_body_1 <- "This is a body text, too."
  #' txt2tibble(txt_title, txt_ITEM_1, txt_item_2, txt_BODY_1, txt_body_1)
  #' 
  #' @export
txt2tibble <- function(...){
  tibble::tibble(...=...) %>%
    dplyr::mutate(!!"tmp" := 1) %>%
    tidyr::pivot_longer(!rlang::sym("tmp"), names_to="use", values_to = "text") %>%
    dplyr::select(-tidyselect::all_of("tmp")) %>%
    tidyr::separate(rlang::sym("use"), into=c(NA, "use"), sep="_", extra="drop")
}


  #' Get font size from font_size_list with name or use. 
  #' 
  #' @param base A numeric of base font size: 
  #'             8, 10, 12, 14, 18, 22, 26, 32, or 38)
  #'             See font_size_list. 
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
  #' @return A numeric of font size
  #' @seealso font_size_list
  #' @examples
  #' get_font_size(base=NULL, name="large", use=NULL)
  #' get_font_size(base=NULL, name="large", use="title")
  #' get_font_size(base=NULL, name=NULL,    use="ITEM")
  #' get_font_size(base=22,   name=NULL,    use="kw")
  #' 
  #' @export
get_font_size <- function(base=NULL, name=NULL, use=NULL){
  #   utils::globalVariables(c("font_size_list", "base_size", "font_name", "font_use"))
  if(is.null(base)){
    message("26pt is used for base size.")
    base <- 26
  }
  if(is.null(name) & is.null(use)){
    message("input must be ONE of name or use. 26pt is used.")
    return(26)
  }
  if(!is.null(name) & !is.null(use)){
    message("'use' is used to set font size. Please input one of name OR use.") 
    name <- NULL
  }
  utils::data(font_size_list, envir=environment())
  font <- dplyr::filter(font_size_list, .data[["base_size"]]==base)
  if(!is.null(name)) font <- dplyr::filter(font, .data[["font_name"]]==name)
  if(!is.null(use))  font <- dplyr::filter(font, stringr::str_detect(.data[["font_use"]], use))
  #   font <- dplyr::filter(font_size_list, rlang::sym("base_size")==base)        # not work. why?
  #   font <- dplyr::filter(font_size_list, rlang::.data[["base_size"]]==base)    # not work. why?
  #   if(!is.null(name)) font <- dplyr::filter(font, rlang::sym("font_name")==name)
  #   if(!is.null(use))  font <- dplyr::filter(font, stringr::str_detect(rlang::sym("font_use"), use))
  font$size
}

  #' Layout  
  #' 
  #' @param tg A text grob
  #' @param width A numeric. 
  #' @param height A numeric. 
  #' @param row_margin A numeric. 
  #' 
  #' @return Lauyout grob
  #' 
  #' @export
tg2layout <- function(tg, 
  width=NULL, 
  height=NULL, 
  row_margin=1.5  # height = height * row_margin
  ){
  nrow <- length(tg)
  grid::grid.layout(nrow, 1, width=grid::unit(841, "mm"), height=grid::unit(rep(1.5, nrow), "grobheight", tg))
  # layout <- grid::grid.layout(nrow, 1, width=unit(841, "mm"), height=unit(1, "null"))
}

  #' Title. 
  #' 
  #' @param tg A text grob
  #' @param layout A Grid layout, or NULL. 
  #'        This can be used to initialise the frame with a number of 
  #'        rows and columns, with initial widths and heights, etc.
  #' @param name A character identifier of frame name. 
  #' 
  #' @export
tg_arrange <- function(tg, layout, name=NULL){
  # tg <- tg_header
  # layout <- tg_layout
  # name <- NULL
  res <- grid::frameGrob(layout=layout, name=name)
  for(i in seq_along(tg)) res <- grid::placeGrob(res, tg[[i]], row=i)
  res
}
