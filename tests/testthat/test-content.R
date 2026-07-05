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

test_that("poster_fix_size() caches an explicit height even when width is left auto", {
  # Regression test: it used to cache the size only when BOTH width and
  # height were given, so a figure with an explicit height but auto width
  # had no measurable size -- a height='auto' card then underestimated it
  # and the next card overlapped it.
  g <- grid::rectGrob()
  fixed <- ggposter:::poster_fix_size(g, height = 100)
  expect_equal(grid::convertHeight(ggposter:::measure_height(fixed), "mm", valueOnly = TRUE), 100)
  ms <- attr(fixed, "measured_size")
  expect_false(is.null(ms$height))  # the given dimension is cached
  expect_null(ms$width)             # the auto dimension falls through
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

test_that("card_image() wraps a caption to its own (possibly narrow) photo's width instead of overflowing it", {
  # Regression test: a caption longer than a narrow (e.g. portrait) photo's
  # own width used to be a single-line, centred textGrob that overflowed
  # evenly on both sides and got clipped -- chopping text off both its
  # start and end. It must now be a wrapping textbox sized to that photo's
  # own width.
  f <- system.file("extdata", "tall.jpg", package = "ggposter")
  skip_if(!nzchar(f), "sample image not found")
  g <- card_image(f, labels = "a very long caption that will not fit on one line",
                   theme = poster_theme(), height = 60)
  cell  <- g$children[[1]]$grobs[[1]]
  label <- cell$grobs[[which(cell$layout$t == 3)]]
  expect_s3_class(label, "textbox_grob")
  photo_width_mm <- 60 * (100 / 150)  # tall.jpg's aspect ratio
  expect_equal(grid::convertWidth(label$width, "mm", valueOnly = TRUE), photo_width_mm, tolerance = 0.5)
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

test_that("with_notes() gives main's unused width to notes instead of leaving a gap", {
  # Regression test: main_width_mm used to be a flat fraction of
  # total_width_mm regardless of what main actually needed, so a table
  # narrower than its allocation left dead space between it and the notes
  # column. The notes column must now start right after main's own actual
  # width (plus the small fixed gap), not at the old fractional boundary.
  th <- poster_theme()
  main <- grid::rectGrob(width = grid::unit(20, "mm"), height = grid::unit(30, "mm"))
  attr(main, "measured_size") <- list(width = grid::unit(20, "mm"), height = grid::unit(30, "mm"))
  g <- with_notes(main, c("- a note"), th, total_width_mm = 150, notes_width = 0.35)
  row <- g$children[[1]]

  widths_mm <- grid::convertWidth(row$widths, "mm", valueOnly = TRUE)
  expect_equal(widths_mm[[1]], 20, tolerance = 0.01)          # main: its own width, not the ~95mm fractional cap
  expect_equal(widths_mm[[3]], 150 - 20 - 5 / 3, tolerance = 0.01)  # notes: everything else
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

test_that("with_notes(show_plot_area=TRUE) draws separate borders for main and notes", {
  th <- poster_theme()
  main <- grid::rectGrob(height = grid::unit(80, "mm"))
  g_off <- with_notes(main, c("- a note"), th, total_width_mm = 150, show_plot_area = FALSE)
  g_on  <- with_notes(main, c("- a note"), th, total_width_mm = 150, show_plot_area = TRUE)

  expect_null(attr(g_off, "plot_area_drawn"))
  expect_true(attr(g_on, "plot_area_drawn"))

  row <- g_on$children[[1]]
  expect_true("plot_area_main"  %in% row$layout$name)
  expect_true("plot_area_notes" %in% row$layout$name)

  main_cell  <- row$layout[row$layout$name == "main", ]
  notes_cell <- row$layout[row$layout$name == "notes", ]
  area_main  <- row$layout[row$layout$name == "plot_area_main", ]
  area_notes <- row$layout[row$layout$name == "plot_area_notes", ]
  expect_equal(area_main$l, main_cell$l)
  expect_equal(area_notes$l, notes_cell$l)
})

test_that("with_notes(show_plot_area=TRUE) sizes the main border to its own content, not the wider allocated column", {
  # Regression test: card_table() only shrinks a table that's too wide for
  # its column, it never stretches a narrower one to fill it, so a border
  # spanning the full allocated column showed a large, misleading "empty"
  # margin that was never really part of the table's own area.
  th <- poster_theme()
  narrow_main <- grid::rectGrob(width = grid::unit(20, "mm"), height = grid::unit(30, "mm"))
  attr(narrow_main, "measured_size") <- list(width = grid::unit(20, "mm"), height = grid::unit(30, "mm"))
  g <- with_notes(narrow_main, c("- a note"), th, total_width_mm = 150, show_plot_area = TRUE)
  row <- g$children[[1]]
  area_main <- row$grobs[[which(row$layout$name == "plot_area_main")]]
  expect_equal(grid::convertWidth(area_main$width, "mm", valueOnly = TRUE), 20)
})
