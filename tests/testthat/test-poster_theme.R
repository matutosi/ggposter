test_that("poster_theme() has sensible defaults", {
  th <- poster_theme()
  expect_s3_class(th, "poster_theme")
  expect_equal(th$accent, "#2E7D32")
  expect_equal(th$box_border, th$accent)
  expect_true(grid::is.unit(th$pad))
  expect_true(grid::is.unit(th$corner_radius))
})

test_that("theme_green() overrides accent consistently", {
  th <- theme_green(accent = "#123456")
  expect_equal(th$accent, "#123456")
  expect_equal(th$box_border, "#123456")
})

test_that("poster_size() resolves named sizes and explicit dimensions", {
  expect_equal(unname(poster_size("A1")), c(594, 841))
  expect_equal(unname(poster_size("a1")), c(594, 841))
  expect_equal(unname(poster_size("A1", "landscape")), c(841, 594))
  expect_equal(unname(poster_size(c(100, 200))), c(100, 200))
  expect_error(poster_size("NOTASIZE"))
})

test_that("poster_font() returns an installed family or the fallback", {
  fam <- poster_font()
  expect_type(fam, "character")
  expect_length(fam, 1)
})

test_that("poster_size() rejects anything that is not a name or two dimensions", {
  # These used to surface as "'names' attribute [2] must be the same length
  # as the vector [0]", or to silently carry a third element along.
  expect_error(poster_size(numeric(0)), "width, height")
  expect_error(poster_size(c(100, 200, 300)), "width, height")
  expect_error(poster_size(c(100, NA)), "width, height")
  expect_error(poster_size(c(0, 200)), "width, height")
})

test_that("poster_register_font() registers a family under the name it is given", {
  fonts <- systemfonts::system_fonts()
  skip_if(nrow(fonts) == 0, "no system fonts to register")
  path <- fonts$path[[1]]
  family <- "ggposter test registered family"

  expect_invisible(poster_register_font(family, path))
  expect_equal(poster_register_font(family, path), family)
  expect_true(family %in% systemfonts::registry_fonts()$family)

  # All four faces default to `plain` when only `plain` is given.
  reg <- systemfonts::registry_fonts()
  reg <- reg[reg$family == family, ]
  expect_equal(nrow(reg), 4L)
  expect_true(all(reg$path == path))
})
