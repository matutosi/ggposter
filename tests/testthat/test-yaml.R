test_that("read_poster_yaml() parses the bundled sample", {
  path <- system.file("extdata", "poster_sample.yml", package = "ggposter")
  skip_if(!nzchar(path), "sample yaml not found")
  spec <- read_poster_yaml(path)
  expect_type(spec, "list")
  expect_true(!is.null(spec$sections))
  expect_true(!is.null(spec$layout))
})

test_that("read_poster_yaml() errors on a missing file", {
  expect_error(read_poster_yaml(tempfile(fileext = ".yml")))
})

test_that("read_poster_yaml() errors when required top-level keys are absent", {
  bad <- tempfile(fileext = ".yml")
  writeLines("title:\n  title: no sections or layout here", bad)
  expect_error(read_poster_yaml(bad))
})
