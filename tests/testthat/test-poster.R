simple_spec <- function() {
  list(
    title = list(title = "Test poster", authors = "*A. Author"),
    layout = list(left = "intro", right = "outro"),
    sections = list(
      intro = list(header = "INTRO",
        body = list(type = "text", md = c("- one", "- two"))),
      outro = list(header = "OUTRO",
        body = list(type = "text", md = c("- three")))
    )
  )
}

test_that("poster() measures 'auto' card heights against a real device, so long wrapped text doesn't overlap the next card", {
  # Regression test: build_column()'s "auto" height needs a concrete
  # numeric mm value, measured against whatever device is current. Without
  # a real device already open (as in a plain script before
  # render_poster() opens one, or in this test environment), that
  # measurement used to fall back to a device-less generic font
  # substitute -- narrower on average than the real font -- which wrapped
  # a long paragraph into fewer lines than it actually needs and
  # underestimated the card's height enough for the next "auto" card to
  # overlap it.
  long_text <- paste(rep("word", 40), collapse = " ")
  spec <- list(
    layout = list(left = c("a", "b")),
    sections = list(
      a = list(header = "A", height = "auto", body = list(type = "text", md = long_text)),
      b = list(header = "B", height = "auto", body = list(type = "text", md = long_text))
    )
  )
  n_dev_before <- length(grDevices::dev.list())
  p <- poster(spec)
  expect_equal(length(grDevices::dev.list()), n_dev_before)  # no leaked device

  col <- p$patchwork$grobs[[which(p$patchwork$layout$name == "left")]]
  heights_mm <- grid::convertHeight(col$heights, "mm", valueOnly = TRUE)
  # both cards must report a real, substantial height (many wrapped
  # lines' worth), not a near-zero/underestimated one
  expect_true(all(heights_mm[1:2] > 30))
})

test_that("a height='auto' figure card with an explicit height (no width) is sized to that height", {
  # Regression test: a figure given an explicit height but no width wasn't
  # measurable (poster_fix_size() only cached when BOTH were given), so the
  # "auto" card underestimated its height and the following card overlapped.
  skip_if_not_installed("ggplot2")
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  spec <- list(
    layout = list(left = c("a", "b")),
    sections = list(
      a = list(header = "A", height = "auto",
               body = list(type = "figure", object = "gg", height = 120)),
      b = list(header = "B", height = "auto", body = list(type = "text", md = "- next card"))
    )
  )
  p <- poster(spec, objects = list(gg = gg))
  col <- p$patchwork$grobs[[which(p$patchwork$layout$name == "left")]]
  heights_mm <- grid::convertHeight(col$heights, "mm", valueOnly = TRUE)
  # card A must be at least as tall as its 120mm figure (plus header/pad),
  # not underestimated
  expect_gt(heights_mm[1], 120)
})

test_that("poster() assembles a spec into a ggposter object", {
  p <- poster(simple_spec())
  expect_s3_class(p, "ggposter")
  expect_equal(unname(p$size_mm), c(594, 841))
})

test_that("render_poster(show_plot_area=TRUE) renders without error, and it is not a poster() argument", {
  # show_plot_area is an output/inspection option on render_poster(), not
  # part of the poster's content, so poster() must not carry it.
  expect_false("show_plot_area" %in% names(formals(poster)))

  p <- poster(simple_spec())
  out <- tempfile(fileext = ".png")
  expect_no_error(render_poster(p, out, scale = 0.1, dpi = 50, show_plot_area = TRUE))
  expect_true(file.exists(out))
})

test_that("rescale_poster(show_plot_area=TRUE) rebuilds with per-card plot-area borders", {
  p <- poster(simple_spec())
  scaled <- rescale_poster(p, 0.5, show_plot_area = TRUE)
  col  <- scaled$patchwork$grobs[[which(scaled$patchwork$layout$name == "left")]]
  card <- col$grobs[[1]]
  expect_true("plot_area" %in% card$layout$name)

  # the default (no overlay) rebuild must not add them
  plain <- rescale_poster(p, 0.5)
  col_p  <- plain$patchwork$grobs[[which(plain$patchwork$layout$name == "left")]]
  card_p <- col_p$grobs[[1]]
  expect_false("plot_area" %in% card_p$layout$name)
})

test_that("poster() errors when a layout entry has no matching section", {
  spec <- simple_spec()
  spec$layout$left <- c("intro", "missing_section")
  expect_error(poster(spec))
})

test_that("render_poster() writes a PNG at the requested scale", {
  p <- poster(simple_spec())
  out <- tempfile(fileext = ".png")
  render_poster(p, out, scale = 0.1, dpi = 50)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("render_poster() writes a true-size PDF", {
  skip_if_no_cairo()
  p <- poster(simple_spec())
  out <- tempfile(fileext = ".pdf")
  render_poster(p, out)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("poster() content is positioned under the header, not centred", {
  # Regression test for the bug where heightDetails.header_tab dispatch
  # returned 0 once the package was loaded as an installed package (rather
  # than sourced into globalenv()), pushing every card's body down into the
  # middle of the card instead of directly under the header tab.
  th <- poster_theme(base_size = 20)
  tab <- header_tab("INTRO", th, th$base_size * 1.15)
  expect_gt(grid::convertHeight(tab$height, "mm", valueOnly = TRUE), 0)
})

test_that("rescale_poster() scales theme font size, spacing, and paper size", {
  # Regression test: render_poster(scale=) and the auto-rescaling in
  # print.ggposter() used to just shrink the output canvas while leaving
  # font sizes and any absolute-mm wrap widths computed for the true paper
  # size, so a scaled-down preview had text and tables overflowing their
  # cards. rescale_poster() must shrink the theme and paper size together.
  p <- poster(simple_spec())
  scaled <- rescale_poster(p, 0.25)

  expect_equal(scaled$theme$base_size, p$theme$base_size * 0.25)
  expect_equal(unname(scaled$size_mm), unname(p$size_mm) * 0.25)
  expect_equal(
    grid::convertWidth(scaled$theme$pad, "mm", valueOnly = TRUE),
    grid::convertWidth(p$theme$pad, "mm", valueOnly = TRUE) * 0.25
  )
})

test_that("rescale_poster() scales explicit body width/height overrides", {
  spec <- simple_spec()
  spec$sections$intro$body$width  <- 200
  spec$sections$intro$body$height <- 100
  p <- poster(spec)
  scaled <- rescale_poster(p, 0.5)

  expect_equal(scaled$spec$sections$intro$body$width,  100)
  expect_equal(scaled$spec$sections$intro$body$height, 50)
})

test_that("render_poster(scale=) does not error for a heavily scaled-down preview", {
  p <- poster(simple_spec())
  out <- tempfile(fileext = ".png")
  expect_no_error(render_poster(p, out, scale = 0.15, dpi = 50))
})

test_that("build_body() does not pass an absolute mm width for text by default", {
  # gridtext's line breaking is unreliable at the very small absolute font
  # sizes a heavily scaled-down preview produces when combined with an
  # absolute-mm wrap target, so text must default to relative ("npc")
  # wrapping and only use an absolute width if the spec explicitly gives one.
  th <- poster_theme()
  body_no_override <- list(type = "text", md = "- x")
  body_explicit     <- list(type = "text", md = "- x", width = 123)
  g1 <- ggposter:::build_body(body_no_override, th, list(), ".", col_width_mm = 297)
  g2 <- ggposter:::build_body(body_explicit, th, list(), ".", col_width_mm = 297)
  expect_true(grid::is.grob(g1))
  expect_true(grid::is.grob(g2))
})

test_that("section$height = 'auto' fits the card to its content instead of a relative share", {
  spec <- simple_spec()
  spec$sections$intro$height <- "auto"
  spec$sections$outro$height <- 1
  p <- poster(spec)
  expect_s3_class(p, "ggposter")
  out <- tempfile(fileext = ".png")
  expect_no_error(render_poster(p, out, scale = 0.1, dpi = 50))
})

test_that("section$height = 'auto' works for every section in a column", {
  spec <- simple_spec()
  spec$sections$intro$height <- "auto"
  spec$sections$outro$height <- "auto"
  p <- poster(spec)
  out <- tempfile(fileext = ".png")
  expect_no_error(render_poster(p, out, scale = 0.1, dpi = 50))
})

test_that("a table/figure body with notes= is placed beside a bullet list", {
  spec <- simple_spec()
  spec$sections$intro$body <- list(
    type = "table", object = "tbl",
    notes = c("- point one", "- point two")
  )
  p <- poster(spec, objects = list(tbl = data.frame(a = 1:2, b = 3:4)))
  expect_s3_class(p, "ggposter")
  out <- tempfile(fileext = ".png")
  expect_no_error(render_poster(p, out, scale = 0.1, dpi = 50))
})

test_that("poster() reads a spec from a YAML file path", {
  path <- system.file("extdata", "poster_sample.yml", package = "ggposter")
  skip_if(!nzchar(path), "sample yaml not found")

  tbl <- data.frame(a = 1:2, b = 3:4)
  fig <- if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  } else {
    skip("ggplot2 not installed")
  }
  p <- poster(path, objects = list(
    tbl_courses = tbl, tbl_community = tbl,
    fig_cutting = fig, fig_ordination = fig
  ))
  expect_s3_class(p, "ggposter")
})

test_that("a height='auto' figure card with an explicit width (no height) is sized to that width", {
  # Regression test for the mirror image of the case above. poster_card()
  # skipped content_pad_factor whenever the body carried a "measured_size"
  # attribute at all, but poster_fix_size() caches each dimension on its
  # own: a width-only figure's cached height is NULL, so measure_height()
  # fell through to the bare grobTree() wrapper and returned 0. The card
  # collapsed to just its header and padding, and the figure was drawn into
  # a zero-height cell.
  skip_if_not_installed("ggplot2")
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  spec <- list(
    layout = list(left = c("a", "b")),
    sections = list(
      a = list(header = "A", height = "auto",
               body = list(type = "figure", object = "gg", width = 100)),
      b = list(header = "B", height = "auto", body = list(type = "text", md = "- next card"))
    )
  )
  p <- poster(spec, objects = list(gg = gg))
  col <- p$patchwork$grobs[[which(p$patchwork$layout$name == "left")]]
  heights_mm <- grid::convertHeight(col$heights, "mm", valueOnly = TRUE)
  # 100mm wide at the default 4:3 gives a 75mm figure, plus header and padding.
  expect_gt(heights_mm[1], 75)
})

test_that("a height='auto' figure card with neither width nor height is still measurable", {
  skip_if_not_installed("ggplot2")
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  spec <- list(
    layout = list(left = c("a", "b")),
    sections = list(
      a = list(header = "A", height = "auto", body = list(type = "figure", object = "gg")),
      b = list(header = "B", height = "auto", body = list(type = "text", md = "- next card"))
    )
  )
  p <- poster(spec, objects = list(gg = gg))
  col <- p$patchwork$grobs[[which(p$patchwork$layout$name == "left")]]
  heights_mm <- grid::convertHeight(col$heights, "mm", valueOnly = TRUE)
  # A ggplotGrob on its own measures to its fixed axis/label rows (its panel
  # is a "null" unit) -- around 18mm, whatever the poster's size. The card
  # must instead follow from the column width it was given.
  expect_gt(heights_mm[1], 100)
})

test_that("a figure in a card with a numeric height still fills its share of the column", {
  # The 4:3 default above must not leak into the relative-height case, where
  # the figure is meant to stretch to whatever share of the column it gets.
  skip_if_not_installed("ggplot2")
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  spec <- list(
    poster = list(size = "A1"),
    layout = list(left = c("a", "b")),
    sections = list(
      a = list(header = "A", height = 3, body = list(type = "figure", object = "gg", width = 100)),
      b = list(header = "B", height = 1, body = list(type = "text", md = "- next card"))
    )
  )
  p <- poster(spec, objects = list(gg = gg))
  col <- p$patchwork$grobs[[which(p$patchwork$layout$name == "left")]]
  heights_mm <- grid::convertHeight(col$heights, "mm", valueOnly = TRUE)
  expect_equal(heights_mm[1] / heights_mm[2], 3, tolerance = 1e-6)
})

test_that("a plain `columns` count keeps every column it asks for, even an empty one", {
  # Regression test: build_poster() dropped columns with no sections in
  # them, so `columns: 4` with three sections silently became three columns
  # a quarter wider than the spec asked for -- the same header laid out
  # differently here than in acposter/qtposter.
  spec <- list(
    poster = list(size = "A1"),
    columns = 4,
    sections = stats::setNames(
      lapply(c("a", "b", "c"), function(n) list(body = list(type = "text", md = n))),
      c("a", "b", "c"))
  )
  p <- poster(spec)
  widths_mm <- grid::convertWidth(p$patchwork$widths, "mm", valueOnly = TRUE)
  expect_length(widths_mm, 4)
  expect_equal(widths_mm, rep(594 / 4, 4), tolerance = 1e-6)
})
