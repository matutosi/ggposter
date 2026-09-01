# poster_title() -- the full-width title band. It had no test of its own
# until 2026-09-02, though every sample spec and the README build one.

band_names <- function(b) b$layout$name
grob_named <- function(b, nm) b$grobs[[which(b$layout$name == nm)]]

test_that("poster_title() draws one row per piece of metadata it is given", {
  th <- poster_theme(base_family = "Arial")
  only_title <- poster_title("T", theme = th, width = 200)
  expect_setequal(band_names(only_title), c("title", "bg"))

  everything <- poster_title("T", subtitle = "S", authors = "A", affiliations = "F",
                             funding = "G", theme = th, width = 200)
  expect_equal(setdiff(band_names(everything), "bg"),
               c("title", "subtitle", "authors", "affiliations", "funding_gap", "funding"))
})

test_that("poster_title() stacks the rows in the order they are declared", {
  th <- poster_theme(base_family = "Arial")
  b <- poster_title("T", subtitle = "S", authors = "A", affiliations = "F",
                    funding = "G", theme = th, width = 200)
  rows <- b$layout[b$layout$name != "bg", ]
  expect_equal(rows$name[order(rows$t)],
               c("title", "subtitle", "authors", "affiliations", "funding_gap", "funding"))
})

test_that("poster_title() sets funding apart with an extra spacer row above it", {
  th <- poster_theme(base_family = "Arial")
  with_funding <- poster_title("T", authors = "A", funding = "G", theme = th, width = 200)
  expect_true("funding_gap" %in% band_names(with_funding))
  gap_t <- with_funding$layout$t[with_funding$layout$name == "funding_gap"]
  fund_t <- with_funding$layout$t[with_funding$layout$name == "funding"]
  expect_equal(fund_t, gap_t + 1)

  without <- poster_title("T", authors = "A", theme = th, width = 200)
  expect_false("funding_gap" %in% band_names(without))
})

test_that("poster_title() scales the title up and the funding note down", {
  th <- poster_theme(base_size = 26, base_family = "Arial")
  b <- poster_title("T", subtitle = "S", authors = "A", funding = "G",
                    theme = th, width = 200)
  size <- function(nm) grob_named(b, nm)$gp$fontsize
  expect_equal(size("title"), 26 * 1.9)
  expect_equal(size("subtitle"), 26 * 1.2)
  expect_equal(size("authors"), 26 * 1.0)   # authors sit at body size
  expect_equal(size("funding"), 26 * 0.6)
})

test_that("poster_title() wraps only when given a width", {
  th <- poster_theme(base_family = "Arial")
  # gridtext wraps; a plain textGrob does not.
  expect_s3_class(grob_named(poster_title("T", theme = th, width = 200), "title"),
                  "textbox_grob")
  expect_s3_class(grob_named(poster_title("T", theme = th), "title"), "text")
})

test_that("poster_title() fills the band with the accent colour, behind everything", {
  th <- poster_theme(accent = "#123456", base_family = "Arial")
  b <- poster_title("T", authors = "A", theme = th, width = 200)
  bg_row <- b$layout[b$layout$name == "bg", ]
  expect_equal(grob_named(b, "bg")$gp$fill, "#123456")
  # gtable renumbers z, so the test is that nothing is drawn under the fill.
  expect_equal(bg_row$z, min(b$layout$z))
  expect_lt(bg_row$z, max(b$layout$z))
  expect_equal(c(bg_row$t, bg_row$b), c(1, nrow(b)))       # spans the padding too
  expect_equal(c(bg_row$l, bg_row$r), c(1, ncol(b)))
})

test_that("poster_title() picks the font family row by row", {
  th <- poster_theme(base_family = "Arial", cjk_family = "Helvetica")
  b <- poster_title("植生調査", authors = "Yamada", theme = th, width = 200)
  expect_equal(grob_named(b, "title")$gp$fontfamily, "Helvetica")
  expect_equal(grob_named(b, "authors")$gp$fontfamily, "Arial")
})

test_that("poster_title() places a logo at the right edge, spanning the band", {
  f <- system.file("extdata", "small.JPG", package = "ggposter")
  skip_if(!nzchar(f), "sample image not found")
  th <- poster_theme(base_family = "Arial")
  b <- poster_title("T", logo = f, theme = th, width = 200)
  expect_true("logo" %in% band_names(b))
  logo_row <- b$layout[b$layout$name == "logo", ]
  expect_equal(c(logo_row$t, logo_row$b), c(1, nrow(b)))
  expect_equal(logo_row$l, ncol(b))                        # the right-hand column
  expect_equal(logo_row$z, max(b$layout$z))                # on top of the band
})

test_that("poster() folds a spec's title block into the band", {
  spec <- list(
    poster = list(size = "A1"),
    title = list(title = "T", authors = "A"),
    layout = list(left = "a"),
    sections = list(a = list(header = "A", body = list(type = "text", md = "- x")))
  )
  p <- poster(spec)
  expect_true("title" %in% p$patchwork$layout$name)
  # The body row is pushed below the band, not overlaid on it.
  expect_equal(p$patchwork$layout$t[p$patchwork$layout$name == "title"], 1)
  expect_equal(p$patchwork$layout$t[p$patchwork$layout$name == "left"], 2)
})
