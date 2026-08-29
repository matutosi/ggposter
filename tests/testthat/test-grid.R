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
