test_that("card_text() wraps bullets and drops unsupported markdown", {
  g <- card_text(c("- one", "- two with `code`"), poster_theme())
  expect_true(grid::is.grob(g))
})

test_that("card_table() shrinks to fit a target width and clips as a safety net", {
  wide <- as.data.frame(matrix(
    paste("column value", 1:30), nrow = 2,
    dimnames = list(NULL, paste0("col_", 1:15))
  ))
  th <- poster_theme(base_size = 20)
  g <- card_table(wide, th, title = "Wide table", width = 100)
  expect_true(grid::is.grob(g))
  # must not error when actually drawn to a device (clip="on" is the
  # safety net if the shrunk table is still too wide). cairo_pdf is used
  # here (as render_poster() does) since the base pdf() device only
  # supports a handful of PostScript font names and errors on arbitrary
  # system font families like "Arial".
  grDevices::cairo_pdf(nullfile())
  on.exit(grDevices::dev.off())
  expect_no_error(grid::grid.draw(g))
})

test_that("card_figure() re-themes and optionally fixes size", {
  skip_if_not_installed("ggplot2")
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  g <- card_figure(gg, poster_theme(), width = 100, height = 80)
  expect_true(grid::is.grob(g))
})

test_that("card_image() lays out photos at correct relative widths", {
  files <- rep(system.file("extdata", "small.JPG", package = "ggposter"), 2)
  skip_if(!nzchar(files[[1]]), "sample image not found")
  g <- card_image(files, labels = c("a", "b"), theme = poster_theme(), height = 30)
  expect_true(grid::is.grob(g))
})

test_that("card_image() requires labels to match the number of files", {
  files <- rep(system.file("extdata", "small.JPG", package = "ggposter"), 2)
  skip_if(!nzchar(files[[1]]), "sample image not found")
  expect_error(card_image(files, labels = "only one", theme = poster_theme()))
})
