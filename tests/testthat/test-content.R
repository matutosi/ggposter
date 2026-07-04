test_that("card_text() wraps bullets and drops unsupported markdown", {
  g <- card_text(c("- one", "- two with `code`"), poster_theme())
  expect_true(grid::is.grob(g))
})

test_that("card_text() left-aligns text by default (bullet and plain lines)", {
  g <- card_text(c("- a bullet line", "a plain paragraph line"), poster_theme())
  bullet_row <- g$grobs[[which(g$layout$name == "line1")]]
  text_box   <- bullet_row$grobs[[which(bullet_row$layout$name == "text")]]
  plain_box  <- g$grobs[[which(g$layout$name == "line2")]]
  expect_equal(text_box$halign, 0)
  expect_equal(plain_box$halign, 0)
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

test_that("card_image(width=) solves a height that fits the target total width", {
  files <- rep(system.file("extdata", "small.JPG", package = "ggposter"), 2)
  skip_if(!nzchar(files[[1]]), "sample image not found")
  g <- card_image(files, labels = c("a", "b"), theme = poster_theme(), width = 150)
  expect_true(grid::is.grob(g))
})

test_that("card_image(label_position='inside') overlays labels on the photo", {
  files <- rep(system.file("extdata", "small.JPG", package = "ggposter"), 2)
  skip_if(!nzchar(files[[1]]), "sample image not found")
  g <- card_image(files, labels = c("a", "b"), theme = poster_theme(),
                   height = 30, label_position = "inside")
  expect_true(grid::is.grob(g))
})

test_that("with_notes() places main content and a bullet list side by side", {
  th <- poster_theme()
  main <- grid::textGrob("main content")
  g <- with_notes(main, c("- note one", "- note two"), th, total_width_mm = 150)
  expect_true(grid::is.grob(g))
  w_mm <- grid::convertWidth(ggposter:::measure_width(g), "mm", valueOnly = TRUE)
  expect_equal(w_mm, 150, tolerance = 0.5)
})

test_that("with_notes() top-aligns the notes column when it is shorter than main", {
  th <- poster_theme()
  main <- grid::rectGrob(height = grid::unit(80, "mm"))
  g <- with_notes(main, c("- a short note"), th, total_width_mm = 150)
  row <- g$children[[1]]
  notes_cell <- row$grobs[[which(row$layout$name == "notes")]]
  expect_equal(notes_cell$vp$justification, c(0, 1))
  expect_equal(grid::convertY(notes_cell$vp$y, "npc", valueOnly = TRUE), 1)
})
