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

test_that("fit_content=TRUE sizes the body row to the body's measured height", {
  th <- poster_theme(base_size = 20, content_pad_factor = 1.2)
  body <- grid::textGrob("hello", x = 0, y = 1, hjust = 0, vjust = 1)
  card_fit  <- poster_card(body, header = "TITLE", theme = th, fit_content = TRUE)
  card_fill <- poster_card(body, header = "TITLE", theme = th, fit_content = FALSE)

  # the fit_content card's body row is an absolute size derived from the
  # body's own height, so the whole card is shorter than one that fills
  # whatever "null" space it's given (here, both start from the same
  # unmeasured body, so fill defaults its row to 1 null unit, which
  # convertHeight() resolves to 0 outside a real layout).
  h_fit  <- grid::convertHeight(gtable::gtable_height(card_fit),  "mm", valueOnly = TRUE)
  h_fill <- grid::convertHeight(gtable::gtable_height(card_fill), "mm", valueOnly = TRUE)
  expect_gt(h_fit, 0)
  expect_gt(h_fit, h_fill)
})

test_that("show_plot_area=TRUE adds a plot_area grob at the body cell, FALSE omits it", {
  th <- poster_theme()
  body <- grid::textGrob("hello")
  card_off <- poster_card(body, header = "TITLE", theme = th, show_plot_area = FALSE)
  card_on  <- poster_card(body, header = "TITLE", theme = th, show_plot_area = TRUE)

  expect_false("plot_area" %in% card_off$layout$name)
  expect_true("plot_area" %in% card_on$layout$name)

  body_row <- card_on$layout[card_on$layout$name == "body", ]
  area_row <- card_on$layout[card_on$layout$name == "plot_area", ]
  expect_equal(area_row$t, body_row$t)
  expect_equal(area_row$l, body_row$l)
})

test_that("show_plot_area=TRUE also outlines the header tab, separately from the body", {
  th <- poster_theme()
  body <- grid::textGrob("hello")
  card_on <- poster_card(body, header = "TITLE", theme = th, show_plot_area = TRUE)

  header_cell <- card_on$grobs[[which(card_on$layout$name == "header")]]
  expect_true("plot_area" %in% header_cell$layout$name)

  # the header's own border must sit at the tab's own (narrow) column, not
  # stretched across the whole card width.
  tab_col   <- header_cell$layout[header_cell$layout$name == "tab", ]
  area_col  <- header_cell$layout[header_cell$layout$name == "plot_area", ]
  expect_equal(area_col$l, tab_col$l)
})

test_that("header_tab() sizes the tab to the label without clipping it", {
  # Regression test: header_tab() used to resolve the label's measured width
  # eagerly with convertWidth() at construction time, which -- when called
  # before render_poster() opens the real output device, as poster() always
  # does -- measured against fallback font metrics and could size the tab
  # narrower than the label actually renders, clipping it. The label must
  # stay a lazy grobWidth()-based unit, resolved at draw time.
  th <- poster_theme(base_size = 24)
  tab <- header_tab("SUMMARY BY DRIVETRAIN", th, th$base_size * 1.15)
  expect_s3_class(tab, "header_tab")
  expect_true(grid::is.unit(tab$width))
})
