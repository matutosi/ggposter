  #' List of font size for ggposter. 
  #'
  #' This data was generated as below. 
  #' base  <- c(8, 10, 12, 14, 18, 22, 26, 32, 38)
  #' ratio <- round(c(1/(1.2^(4:1)), 1.2^(0:6)), 2)
  #' name  <- c(
  #'        "tiny",         "scriptsize", "footnotesize", "small",   
  #'        "normalsize",   "large",      "Large",        "LARGE",     
  #'        "huge",         "Huge",       "HUGE"
  #'         )
  #' use   <- c(
  #'        "tiny",         "script",     "footnote",     "caption", 
  #'        "body",         "BODY",       "item",         "ITEM|thank",
  #'        "author|affil", "subt|kw",    "title"
  #'        )
  #' font_size_list <- 
  #'   tibble::tibble(
  #'     base_size=rep(base,   each=length(ratio)), 
  #'     ratio=rep(ratio,      times=length(base)), 
  #'     font_name=rep(name,   times=length(base)), 
  #'     font_use=rep(use,     times=length(base))) %>%
  #'   dplyr::mutate(size=round(base_size * ratio, 0))
  #' usethis::use_data(font_size_list, font_size_list)
  #' 
  #' You can check font size by base_size. 
  #' font_size_list %>% split(.$base_size)
  #' 
  #' You can see font size as plot by ggplot visualisation as below. 
  #' font_size_list %>%
  #'   ggplot2::ggplot(aes(x=base_size, y=size)) + 
  #'     ggplot2::geom_point() + 
  #'     ggplot2::theme_bw()
  #'
  #' @format A tibble with : 99 rows and 5 variables. 
  #' \describe{
  #'   \item{base_size}{base font size in ggposter}
  #'   \item{ratio}    {ratio to base_size}
  #'   \item{font_name}{font name like LaTeX}
  #'   \item{font_use} {use case for poster}
  #'   \item{size}     {font size}
  #' }
  #' 
  #' @seealso get_font_size()
"font_size_list"
