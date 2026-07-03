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
