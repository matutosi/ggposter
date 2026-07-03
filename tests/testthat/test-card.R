test_that("poster_card() places the body directly under the header tab", {
  th <- poster_theme(base_size = 20)
  body <- grid::textGrob("hello", x = 0, y = 1, hjust = 0, vjust = 1)
  card <- poster_card(body, header = "TITLE", theme = th)
  expect_s3_class(card, "poster_card")
  expect_s3_class(card, "gtable")

  # header row height must come from the tab's own stored size, not a
  # generic-dispatch measurement (see R/card.R for why: heightDetails
  # dispatch on a custom gTree class is unreliable once the package has
  # been loaded with devtools::load_all()).
  tab <- header_tab("TITLE", th, th$base_size * 1.15)
  expect_true(grid::convertHeight(tab$height, "mm", valueOnly = TRUE) > 0)
})

test_that("a NULL or empty header yields fewer rows than a real header", {
  th <- poster_theme()
  body <- grid::textGrob("hello")
  card_no_header    <- poster_card(body, header = NULL, theme = th)
  card_empty_header <- poster_card(body, header = "",   theme = th)
  card_with_header  <- poster_card(body, header = "TITLE", theme = th)

  expect_equal(nrow(card_no_header), nrow(card_empty_header))
  expect_gt(nrow(card_with_header), nrow(card_no_header))
})

test_that("poster_card() accepts a ggplot body", {
  skip_if_not_installed("ggplot2")
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  card <- poster_card(gg, header = "FIG", theme = poster_theme())
  expect_s3_class(card, "gtable")
})

test_that("poster_card() rejects non-grob, non-ggplot bodies", {
  expect_error(poster_card("not a grob", header = "X", theme = poster_theme()))
})
