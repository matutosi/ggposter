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

test_that("read_poster_yaml() reports a key YAML 1.1 turned into a boolean", {
  # restore_y_key() can recover `grid$boxes`' y (nothing else belongs
  # there), but in general the original spelling is unrecoverable -- `on`,
  # `yes` and `true` all arrive as "TRUE". Stopping with the path of the
  # broken key beats silently carrying a section nobody can reference.
  path <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("columns: 2", "sections:", "  on:", "    body: {md: a}"), path)
  expect_error(read_poster_yaml(path), "YAML 1.1")
  expect_error(read_poster_yaml(path), "sections[$]TRUE")
})
