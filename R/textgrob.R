  #' Wrapper function of converting text into arranged text grob.
  #' 
  #' @inheritParams txt2tibble
  #' @param just      A string. 
  #'                  "left", "right", "centre", "center", "bottom", and "top". 
  #'                  See grid::textGrob() in detail.
  #' @param silent Logical. TRUE: no massage, FALSE: shows massage. 
  #' @return textGrob
  #' @examples
  #' txt_title    <- "This is a title."
  #' txt_ITEM_1   <- "ITEM text is larger than item text."
  #' txt_item_2   <- "This is a item text, too."
  #' txt_BODY_1   <- "BODY text is larger than body text."
  #' txt_body_2   <- "This is a body text, too."
  #' arranged_txt_1 <- arrange_txt(txt_title, txt_ITEM_1, txt_item_2)
  #' grid::grid.draw(arranged_txt_1)
  #' arranged_txt_2 <- arrange_txt(txt_BODY_1, txt_body_2, just="left")
  #' grid::grid.draw(arranged_txt_2)
  #' 
  #' @export
arrange_txt <- function(..., just="center", silent=TRUE){
  tbl <- txt2tibble(...)
  tg <- 
    tbl %>%
    dplyr::mutate(font_size = purrr::pmap_dbl(tbl, get_font_size)) %>%
    dplyr::mutate(just=just) %>%
    purrr::pmap(as_tg)
  layout <- tg2layout(tg)
  tg_arrange(tg=tg, layout=layout)
  #   tg <- 
  #     txt2tibble(...) %>%
  #     purrr::pmap(as_tg)
  #   layout <- tg2layout(tg)
  #   tg_arrange(tg=tg, layout=layout)
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
  #' txt_body_2 <- "This is a body text, too."
  #' txt2tibble(txt_title, txt_ITEM_1, txt_item_2, txt_BODY_1, txt_body_2)
  #' 
  #' @export
txt2tibble <- function(...){
  tibble::tibble(...=...) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames("text") %>%
    tibble::rownames_to_column("use") %>%
    tibble::remove_rownames() %>%
  #     tidyr::separate(rlang::sym("use"), into=c(NA, "use"), sep="_", extra="drop")
    tidyr::separate(rlang::sym("use"), into=c(NA, "use"), sep="_", extra="drop", fill="left")
}

  #' Wrapper function for grid::textGrob. 
  #' 
  #' @param text      A string.
  #'                  label in grid::textGrob(). 
  #' @param font_size A numeric of font size.
  #'                  fontsize in grid::textGrob(). 
  #' @param use       A string.
  #'                  name in grid::textGrob(). 
  #' @param just      A string. 
  #'                  "left", "right", "centre", "center", "bottom", and "top". 
  #'                  See grid::textGrob() in detail.
  #' @seealso grid::textGrob()
  #' @export
as_tg <- function(text, font_size, use, just){
  grid::textGrob(label=text, gp=grid::gpar(fontsize=font_size), name=use, just=just)
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

  #' Create layout to fit height and width with text grob. 
  #' 
  #' @param tg A text grob
  #' @param width A numeric. Default is null to fit text grob. 
  #' @param height A numeric. Default is null to fit text grob. 
  #' @param row_margin A numeric. Default is 1.5. This makes rows 1.5 heights of text grobs. 
  #' 
  #' @return Lauyout grob
  #' 
  #' @export
tg2layout <- function(tg, width=NULL, height=NULL, row_margin=1.5  # height = height * row_margin
  ){
  nrow <- length(tg)
  grid::grid.layout(nrow, 1, width=grid::unit(841, "mm"), height=grid::unit(rep(1.5, nrow), "grobheight", tg))
  # layout <- grid::grid.layout(nrow, 1, width=unit(841, "mm"), height=unit(1, "null"))
}

  #' Arrange text grob with layout.
  #' 
  #' @param tg A text grob
  #' @param layout A Grid layout, or NULL. 
  #'        This can be used to initialise the frame with a number of rows.
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
