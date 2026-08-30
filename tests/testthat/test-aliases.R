test_that("shared poster-tool key names are accepted as aliases", {
  spec <- list(
    poster = list(paper = "a2"),
    theme = list("font-size" = 18),
    title = list(title = "T", author = "*A. Author", institute = "Somewhere",
                 note = "Funded by X"),
    layout = list(left = "intro"),
    sections = list(
      intro = list(header = "INTRO", body = list(type = "text", md = "- Hi"))
    )
  )
  expect_no_warning(p <- poster(spec))
  expect_s3_class(p, "ggposter")
  # `paper: "a2"` reached poster_size(), case-insensitively, and
  # `font-size: 18` reached the theme's base_size.
  expect_equal(unname(p$size_mm), c(420, 594))
  expect_equal(p$theme$base_size, 18)
})

test_that("normalize_spec rewrites aliases to ggposter's own keys", {
  spec <- normalize_spec(list(
    title = list(author = "A", institutes = "I", footer = "F"),
    poster = list(paper = "A0"),
    theme = list(font_size = 20, "cjk-family" = "Yu Gothic")
  ))
  expect_equal(spec$title, list(authors = "A", affiliations = "I", funding = "F"))
  expect_equal(spec$poster, list(size = "A0"))
  expect_equal(spec$theme, list(base_size = 20, cjk_family = "Yu Gothic"))
})

test_that("a key and its alias together keep the ggposter key and warn", {
  expect_warning(
    spec <- normalize_spec(list(title = list(authors = "kept", author = "dropped"))),
    "sets both"
  )
  expect_equal(spec$title, list(authors = "kept"))
})

test_that("specs without any alias are left untouched", {
  spec <- list(title = list(authors = "A"), poster = list(size = "A1"),
               theme = list(base_size = 24))
  expect_equal(normalize_spec(spec), spec)
})

test_that("bare size is not treated as a font size", {
  spec <- normalize_spec(list(poster = list(size = "A0"), theme = list()))
  expect_equal(spec$poster$size, "A0")
  expect_null(spec$theme$base_size)
})

# A header written the way acposter/qtposter write one: flat keys, no
# `title:`/`poster:`/`theme:` blocks, and a plain column count.
flat_spec <- function(...) {
  utils::modifyList(list(
    title = "Flat header",
    author = c("*A. One", "B. Two"),
    institute = "Somewhere Univ.",
    note = "Funded by X",
    paper = "a2",
    orientation = "landscape",
    "font-size" = 18,
    font = "sans",
    columns = 2,
    sections = list(
      a = list(header = "A", body = list(type = "text", md = "- a")),
      b = list(header = "B", body = list(type = "text", md = "- b")),
      c = list(header = "C", body = list(type = "text", md = "- c"))
    )
  ), list(...))
}

test_that("a flat acposter/qtposter-style header builds a poster", {
  expect_no_warning(p <- poster(flat_spec()))
  expect_s3_class(p, "ggposter")
  # A2 landscape, and the type size and family reached the theme.
  expect_equal(unname(p$size_mm), c(594, 420))
  expect_equal(p$theme$base_size, 18)
  expect_equal(p$theme$base_family, "sans")
})

test_that("flat keys are folded into the blocks they belong to", {
  spec <- normalize_spec(flat_spec())
  expect_equal(spec$title$title, "Flat header")
  # A list of authors is joined into the one line poster_title() draws.
  expect_equal(spec$title$authors, "*A. One, B. Two")
  expect_equal(spec$title$affiliations, "Somewhere Univ.")
  expect_equal(spec$title$funding, "Funded by X")
  expect_mapequal(spec$poster, list(size = "a2", orientation = "landscape"))
  expect_equal(spec$theme$base_size, 18)
  # The top level keeps only structure, not metadata.
  expect_setequal(names(spec), c("title", "poster", "theme", "layout", "sections"))
})

test_that("a plain `columns` count becomes a left-to-right flow layout", {
  spec <- normalize_spec(flat_spec())
  expect_equal(spec$layout, list(col1 = c("a", "b"), col2 = "c"))
  expect_equal(flow_layout(letters[1:6], 3),
               list(col1 = c("a", "b"), col2 = c("c", "d"), col3 = c("e", "f")))
  # The leftmost columns take the remainder, as in a newspaper flow.
  expect_equal(flow_layout(letters[1:5], 2), list(col1 = c("a", "b", "c"), col2 = c("d", "e")))
  # More columns than sections leaves the trailing ones empty rather than erroring.
  expect_equal(flow_layout(c("a", "b"), 3),
               list(col1 = "a", col2 = "b", col3 = character(0)))
  expect_error(flow_layout(letters[1:3], 0), "positive integer")
})

test_that("`columns` alongside layout/grid is ignored with a warning", {
  expect_warning(
    spec <- normalize_spec(flat_spec(layout = list(left = c("a", "b", "c")))),
    "only builds a layout when neither is given"
  )
  expect_equal(spec$layout, list(left = c("a", "b", "c")))
})

test_that("a flat key and its block form together keep the block's and warn", {
  expect_warning(
    spec <- normalize_spec(list(author = "flat", title = list(author = "block"))),
    "set both at the top level and inside"
  )
  expect_equal(spec$title, list(authors = "block"))
})

test_that("nested specs keep working untouched", {
  nested <- list(
    poster = list(size = "A1", orientation = "portrait"),
    theme = list(base_size = 24),
    title = list(title = "T", authors = "A"),
    layout = list(left = "a"),
    sections = list(a = list(header = "A", body = list(type = "text", md = "- a")))
  )
  expect_equal(normalize_spec(nested), nested)
})

test_that("the bundled flat sample reads and builds", {
  path <- system.file("extdata", "poster_sample_flat.yml", package = "ggposter")
  skip_if(path == "", "flat sample not installed")
  spec <- normalize_spec(read_poster_yaml(path))
  expect_equal(spec$title$authors, "*A. One, B. Two")
  expect_equal(spec$title$affiliations, "Example Univ., Example Museum")
  expect_equal(spec$poster$size, "A1")
  expect_equal(spec$theme$base_size, 22)
  expect_equal(spec$layout, list(col1 = c("objectives", "methods"),
                                 col2 = c("results", "conclusions")))
  expect_no_warning(p <- poster(read_poster_yaml(path)))
  expect_equal(unname(p$size_mm), c(594, 841))
})
