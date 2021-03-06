#' Wrapper function of converting text into arranged text grob.
#'
#' @inheritParams txt2tibble
#' @param x,y           A numeric. Specify x- or y-value.
#' @param hjust,vjust   A numeric 0-1. 1: left', 0: 'right'
#' @param widths        grid::unit
#' @param convert       Logical. TRUE: convert unit to simple unit ('mm')
#'                      to improve performance.
#'                      Not implemented yet.
#' @param shrink        A numeric. Shrink rate of font size.
#'                      In preparation, 0.25 is recommended.
#'                      This make the paper size from A0 (810mm * 1189) to A4 (210 * 297).
#' @param silent        Logical. TRUE: no massage, FALSE: shows massage.
#' @return              textGrob
#' @examples
#' txt_title <- "This is a title."
#' txt_ITEM_1 <- "ITEM text is larger than item text."
#' txt_item_2 <- "This is a item text, too."
#' txt_BODY_1 <- "BODY text is larger than body text."
#' txt_body_2 <- "This is a body text, too."
#' arranged_txt_1 <- arrange_txt(txt_title, txt_ITEM_1, txt_item_2)
#' grid::grid.draw(arranged_txt_1)
#' arranged_txt_2 <- arrange_txt(txt_BODY_1, txt_body_2, hjust = 1)
#' grid::grid.draw(arranged_txt_2)
#'
#' @export
arrange_txt <- function(..., x = 0, y = 0.5,
                        hjust = 0, vjust = 0.5,
                        widths = grid::unit(841, "mm"),
                        convert = FALSE, shrink = 1, silent = TRUE) {
  widths <- widths * shrink
  tbl <- txt2tibble(...)
  tg <- 
    tbl %>%
    dplyr::mutate(font_size = purrr::pmap_dbl(tbl, get_font_size, shrink = shrink, silent = silent)) %>%
    dplyr::mutate(x = x) %>%
    dplyr::mutate(y = y) %>%
    dplyr::mutate(hjust = hjust) %>%
    dplyr::mutate(vjust = vjust) %>%
    purrr::pmap(as_tg)
  layout <- tg2layout(tg, widths = widths)
  tg_arrange(tg = tg, layout = layout)
}

#' Create layout to fit height and width with text grob.
#'
#' @param tg A text grob
#' @param widths grid::unit
#' @param heights A numeric. Default is null to fit text grob.
#' @param row_margin A numeric. Default is 1.5. This makes rows 1.5 heights of text grobs.
#'
#' @return Layout grob
#'
#' @export
tg2layout <- function(tg, widths = grid::unit(841, "mm"), heights = NULL, row_margin = 1.5 # height = height * row_margin
) {
  nrow <- length(tg)
  grid::grid.layout(nrow, 1,
    widths = widths,
    heights = grid::unit(rep(1.5, nrow), "grobheight", tg))
}

#' Arrange text grob with layout.
#'
#' @param tg A text grob
#' @param layout A Grid layout, or NULL.
#'        This can be used to initialize the frame with a number of rows.
#' @param name A character identifier of frame name.
#'
#' @export
tg_arrange <- function(tg, layout, name = NULL) {
  res <- grid::frameGrob(layout = layout, name = name)
  for (i in seq_along(tg)) res <- grid::placeGrob(res, tg[[i]], row = i)
  res
}
