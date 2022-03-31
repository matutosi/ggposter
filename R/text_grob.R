#' Convert text into tibble.
#'
#' @param ... Text object(s). Multiple object can be input.
#'        Object name of text: foo_keyword or foo_keyword_bar
#'        examples: txt_title, txt_body_1, proj_ITEM_big
#'        keyword: 'tiny', 'script', 'footnote', 'caption',
#'                 'body', 'BODY', 'item', 'ITEM',
#'                 'thank', 'author', 'affil',
#'                 'subt', 'kw' or 'title'
#' @return Tibble with 'text' and 'use' colnames.
#'         Values of 'text' are character of value to be input.
#'         Values of 'use' are characters of value name to be input.
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
txt2tibble <- function(...) {
  data.frame(...) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames("text") %>%
    tibble::rownames_to_column("use") %>%
    tibble::remove_rownames() %>%
    tidyr::separate(rlang::sym("use"), into = c(NA, "use"), sep = "_", extra = "drop", fill = "left")
}

#' Get font size from font_size_list with name or use.
#'
#' @param base A numeric of base font size:
#'             8, 10, 12, 14, 18, 22, 26, 32, or 38)
#'             If base=NULL, base=26 will be set in get_font_size().
#' @param name A string of font size name.
#'             'tiny', 'scriptsize', 'footnotesize', 'small',
#'             'normalize', 'large', 'Large', 'LARGE',
#'             'huge', 'Huge', or 'HUGE'
#' @param use A string of font size name for use.
#'             'tiny', 'script', 'footnote', 'caption',
#'             'body', 'BODY', 'item', 'ITEM', 'thank',
#'             'author', 'affil', 'subt', 'kw', or 'title'
#'               thank: acknowledgement
#'               affil: affiliation
#'               subt: subtitle
#'               kw: keyword
#' @param shrink A numeric. Shrink rate of font size.
#'               In preparation, 0.25 is recommended.
#'               This make the paper size from A0 (810mm * 1189) to A4 (210 * 297).
#' @param text   A string, but this argument will be omitted.
#' @param silent Logical. TRUE: no massage, FALSE: shows massage.
#' @return A numeric of font size.
#' @seealso font_size_list
#' @examples
#' get_font_size(base = NULL, name = "large", use = NULL)
#' get_font_size(base = NULL, name = "large", use = "title")
#' get_font_size(base = NULL, name = NULL, use = "ITEM")
#' get_font_size(base = 22, name = NULL, use = "kw")
#'
#' @export
get_font_size <- function(base = NULL, name = NULL, use = NULL, shrink = 1, silent = 1, text = "") {
  if (is.null(base)) {
    if (!silent) {
      message("26pt is used for base size.")
    }
    base <- 26
  }
  if (is.null(name) & is.null(use)) {
    if (!silent) {
      message("input must be ONE of name or use. 26pt is used.")
    }
    return(26)
  }
  if (!is.null(name) & !is.null(use)) {
    if (!silent) {
      message("'use' is used to set font size. Please input one of name OR use.")
    }
    name <- NULL
  }
  utils::data("font_size_list")
  font <- dplyr::filter(eval(rlang::sym("font_size_list")), .data[["base_size"]] == base)
  if (!is.null(name)) {
    font <- dplyr::filter(font, .data[["font_name"]] == name)
  }
  if (!is.null(use)) {
    font <- dplyr::filter(font, stringr::str_detect(.data[["font_use"]], use))
  }
  font$size * shrink
}

#' Wrapper function to generate textGrob for grid::textGrob.
#'
#' @param text           A string.
#'                       label in grid::textGrob().
#' @param x,y            A numeric. Specify x- or y-value.
#' @param hjust,vjust    A numeric 0-1. 0: left, 1: right.
#' @param font_size      A numeric of font size.
#'                       fontsize in grid::textGrob().
#' @param use            A string. Name in grid::textGrob().
#' @param ...       Some more arguments
#' @seealso grid::textGrob()
#' @export
as_tg <- function(text, x, y, hjust, vjust, font_size, use, ...) {
  grid::textGrob(
    label = text, x = x, y = y, 
    hjust = hjust, vjust = vjust, 
    gp = grid::gpar(fontsize = font_size),
    name = use, ...)
}
