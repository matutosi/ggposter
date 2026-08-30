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
