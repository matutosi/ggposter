#' Generate a faster table grob
#' 
#' This function is a little bit modificated vertion of grid.ftable 
#' in "Displaying tables as grid graphics" (gridExtra vignettes)
#' https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
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
#' t_2 <- "When longer than viewpeort width, "
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
grid.ftable <- function(d, padding = unit(4, "mm"), ...) {

  nc <- ncol(d)
  nr <- nrow(d)

  ## character table with added row and column names
  extended_matrix <- cbind(c("", rownames(d)),
                           rbind(colnames(d),
                                 as.matrix(d)))

  ## string width and height
  w <- apply(extended_matrix, 2, strwidth, "inch")
  h <- apply(extended_matrix, 2, strheight, "inch")

  widths <- apply(w, 2, max)
  heights <- apply(h, 1, max)

  padding <- convertUnit(padding, unitTo = "in", valueOnly = TRUE)

  x <- cumsum(widths + padding) - 0.5 * padding
  y <- cumsum(heights + padding) - padding

  rg <- rectGrob(x = unit(x - widths/2, "in"),
                 y = unit(1, "npc") - unit(rep(y, each = nc + 1), "in"),
                 width = unit(widths + padding, "in"),
                 height = unit(heights + padding, "in"))

  tg <- textGrob(c(t(extended_matrix)), x = unit(x - widths/2, "in"),
                 y = unit(1, "npc") - unit(rep(y, each = nc + 1), "in"),
                 just = "center")

  g <- gTree(children = gList(rg, tg), ...,
             x = x, y = y, widths = widths, heights = heights)

  grid.draw(g)
  invisible(g)
}
