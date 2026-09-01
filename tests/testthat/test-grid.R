grid_spec <- function() {
  list(
    title = list(title = "Grid test", authors = "*A. Author"),
    grid = list(
      columns = 3,
      boxes = list(
        list(name = "intro", x = 0, y = 0, w = 2),
        list(name = "tall",  x = 2, y = 0, h = 3),
        list(name = "a",     x = 0, y = 1),
        list(name = "b",     x = 1, y = 1),
        list(name = "wide",  x = 0, y = 2, w = 2)
      )
    ),
    sections = list(
      intro = list(header = "INTRO", height = "auto", body = list(type = "text", md = "- intro")),
      tall  = list(header = "TALL",  height = "auto", body = list(type = "text", md = c("- l1", "- l2", "- l3"))),
      a     = list(header = "A", height = "auto", body = list(type = "text", md = "- box a")),
      b     = list(header = "B", height = "auto", body = list(type = "text", md = "- box b")),
      wide  = list(header = "WIDE", height = "auto", body = list(type = "text", md = "- wide"))
    )
  )
}

test_that("poster() places a column-spanning and row-spanning grid: box correctly", {
  p <- poster(grid_spec())
  expect_s3_class(p, "ggposter")

  body <- p$patchwork$grobs[[which(p$patchwork$layout$name == "grid_body")]]
  by_name <- function(nm) body$layout[body$layout$name == nm, ]

  intro <- by_name("intro")
  expect_equal(c(intro$t, intro$l, intro$b, intro$r), c(1, 1, 1, 2))

  tall <- by_name("tall")
  expect_equal(c(tall$t, tall$l, tall$b, tall$r), c(1, 3, 3, 3))

  wide <- by_name("wide")
  expect_equal(c(wide$t, wide$l, wide$b, wide$r), c(3, 1, 3, 2))

  out <- tempfile(fileext = ".png")
  expect_no_error(render_poster(p, out, scale = 0.1, dpi = 50))
})

test_that("grid: a row-spanning box with many lines keeps its full column width", {
  # Regression test: build_grid_body() used to pin a row-spanning ("h > 1")
  # box to its cell with anchor_top_left(), which sizes its viewport from
  # measure_width()/measure_height(). A poster_card's *width* is a "null"
  # unit (it normally just fills whatever column it's given), and
  # gtable::gtable_width() resolves a lone "null" width, measured outside
  # any parent layout, to some small, meaningless value -- collapsing the
  # box to a sliver a few mm wide and clipping almost all of its content.
  spec <- grid_spec()
  spec$sections$tall$body$md <- paste0("- line ", 1:9)  # enough lines to
                                                          # make the bug visible
  p <- poster(spec)
  body <- p$patchwork$grobs[[which(p$patchwork$layout$name == "grid_body")]]
  tall_grob <- body$grobs[[which(body$layout$name == "tall")]]
  w_mm <- grid::convertWidth(tall_grob$vp$width, "mm", valueOnly = TRUE)
  # one grid column is 594/3 = 198mm; a collapsed box measured well under 100mm
  expect_gt(w_mm, 150)
})

test_that("grid: two same-row boxes are resolved to the same row height", {
  p <- poster(grid_spec())
  body <- p$patchwork$grobs[[which(p$patchwork$layout$name == "grid_body")]]
  a_row <- body$layout[body$layout$name == "a", "t"]
  b_row <- body$layout[body$layout$name == "b", "t"]
  expect_equal(a_row, b_row)
  # both single-row boxes, so the row height they were built at is shared
  heights_mm <- grid::convertHeight(body$heights, "mm", valueOnly = TRUE)
  expect_true(heights_mm[[a_row]] > 0)
})

test_that("grid: overlapping boxes raise an error naming both boxes", {
  spec <- grid_spec()
  spec$grid$boxes[[3]] <- list(name = "a", x = 0, y = 0)  # now overlaps intro
  expect_error(poster(spec), "overlap")
})

test_that("grid: a box positioned outside the column count raises an error", {
  spec <- grid_spec()
  spec$grid$boxes[[1]]$w <- 5
  expect_error(poster(spec), "overflow")
})

test_that("grid: a section not placed in any box raises an error", {
  spec <- grid_spec()
  spec$grid$boxes[[1]] <- NULL  # "intro" section now unplaced
  expect_error(poster(spec), "not placed")
})

test_that("grid: a box naming an undefined section raises an error", {
  spec <- grid_spec()
  spec$grid$boxes[[1]]$name <- "nope"
  expect_error(poster(spec), "not defined")
})

test_that("grid: content taller than the page warns instead of erroring", {
  spec <- grid_spec()
  long_text <- paste(rep("word", 400), collapse = " ")
  spec$sections$intro$body$md <- long_text
  expect_warning(poster(spec), "taller than the page")
})

test_that("layout and grid together: grid wins, with a warning", {
  spec <- grid_spec()
  spec$layout <- list(left = c("intro", "tall", "a", "b", "wide"))
  expect_warning(p <- poster(spec), "grid.*takes precedence")
  expect_true("grid_body" %in% p$patchwork$layout$name)
})

test_that("read_poster_yaml() accepts a grid: spec without layout:", {
  tmp <- tempfile(fileext = ".yml")
  yaml::write_yaml(list(sections = grid_spec()$sections, grid = grid_spec()$grid), tmp)
  expect_no_error(read_poster_yaml(tmp))
})

test_that("read_poster_yaml() reads an unquoted y: in grid boxes", {
  # R の yaml は YAML 1.1 なので，引用符の無い `y` を真偽値として読む
  # (キーが "TRUE" になる)．姉妹ツール (acposter・qtposter) からヘッダーを
  # そのまま移せるよう，読み込んだ直後にキー名を戻している．
  # **これは yaml11_bad_keys() の見張りも兼ねる** (test-yaml.R)．あちらは
  # 戻せないキーで止めるので，戻せるこの `y` を巻き込んでいないことを，
  # 重複を作らずここで確かめている．
  tmp <- tempfile(fileext = ".yml")
  writeLines(c(
    "grid:",
    "  columns: 2",
    "  boxes:",
    "    - {name: a, x: 0, y: 0, w: 2}",
    "    - {name: b, x: 0, y: 1}",
    "    - {name: c, x: 1, y: 1}",
    "sections:",
    "  a: {header: A, body: {type: text, md: [\"a\"]}}",
    "  b: {header: B, body: {type: text, md: [\"b\"]}}",
    "  c: {header: C, body: {type: text, md: [\"c\"]}}"
  ), tmp)
  spec <- read_poster_yaml(tmp)
  ys <- vapply(spec$grid$boxes, function(b) b$y, numeric(1))
  expect_equal(ys, c(0, 1, 1))
  expect_no_error(poster(spec))
})

test_that("grid: a zero or negative span is rejected instead of silently vanishing", {
  # Regression test: w/h went through as.integer() with no check, so `w: 0`
  # produced a box occupying no cells at all -- it slipped past the overlap
  # check and then made a gtable entry with r < l. `h: -1` reached gtable
  # and came back as "argument must be coercible to non-negative integer".
  mk <- function(...) list(
    poster = list(size = "A1"),
    grid = list(columns = 2, boxes = list(
      utils::modifyList(list(name = "a", x = 0, y = 0), list(...)),
      list(name = "b", x = 1, y = 0))),
    sections = list(a = list(body = list(type = "text", md = "a")),
                    b = list(body = list(type = "text", md = "b")))
  )
  expect_error(poster(mk(w = 0)), "invalid.*w")
  expect_error(poster(mk(h = 0)), "invalid.*h")
  expect_error(poster(mk(h = -1)), "invalid.*h")
})

test_that("grid: a fractional coordinate is rejected instead of being truncated", {
  # `x: 0.9` used to become column 0, not the column the spec named.
  spec <- list(
    poster = list(size = "A1"),
    grid = list(columns = 2, boxes = list(
      list(name = "a", x = 0.9, y = 0), list(name = "b", x = 1, y = 0))),
    sections = list(a = list(body = list(type = "text", md = "a")),
                    b = list(body = list(type = "text", md = "b")))
  )
  expect_error(poster(spec), "invalid.*x")
})

test_that("grid: a negative coordinate names the field it came from", {
  spec <- list(
    poster = list(size = "A1"),
    grid = list(columns = 2, boxes = list(list(name = "a", x = -1, y = 0))),
    sections = list(a = list(body = list(type = "text", md = "a")))
  )
  expect_error(poster(spec), "invalid.*x")
})
