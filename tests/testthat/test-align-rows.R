# `layout$align_rows` -- the two-pass build in build_poster()/build_column():
# the first pass only *measures* each "auto" card, and once the tallest card
# at each row position is known, the affected columns are rebuilt so the
# shorter cards stretch to match. It is used by three of the bundled sample
# specs and by the README, and had no test of its own until 2026-09-02.

# One "auto" card per section, its height driven by how many lines it holds.
align_spec <- function(lines, layout, ...) {
  list(
    poster = list(size = "A1"),
    layout = layout,
    sections = stats::setNames(
      lapply(names(lines), function(n) {
        list(header = toupper(n), height = "auto",
             body = list(type = "text", md = rep(paste("line for", n), lines[[n]])))
      }),
      names(lines))
  )
}

col_heights_mm <- function(p, name) {
  col <- p$patchwork$grobs[[which(p$patchwork$layout$name == name)]]
  grid::convertHeight(col$heights, "mm", valueOnly = TRUE)
}

card_at <- function(p, name, i) {
  p$patchwork$grobs[[which(p$patchwork$layout$name == name)]]$grobs[[i]]
}

LINES <- list(a = 1, b = 6, c = 1, d = 1)   # b is the tall one

test_that("align_rows lines each row up across columns to the tallest card there", {
  aligned <- poster(align_spec(LINES, list(align_rows = TRUE,
                                           left = c("a", "c"), right = c("b", "d"))))
  left  <- col_heights_mm(aligned, "left")
  right <- col_heights_mm(aligned, "right")
  expect_equal(left[1], right[1])
  expect_equal(left[2], right[2])

  # The target is the *tallest* card at that row, not an average or the
  # first column's: row 1's height must be what b needs on its own.
  plain <- poster(align_spec(LINES, list(left = c("a", "c"), right = c("b", "d"))))
  expect_equal(left[1], col_heights_mm(plain, "right")[1])
  expect_gt(left[1], col_heights_mm(plain, "left")[1])
})

test_that("without align_rows each column keeps its own row heights", {
  plain <- poster(align_spec(LINES, list(left = c("a", "c"), right = c("b", "d"))))
  left  <- col_heights_mm(plain, "left")
  right <- col_heights_mm(plain, "right")
  # a (1 line) and b (6 lines) sit at the same row position but are unrelated.
  expect_lt(left[1], right[1])
  expect_equal(left[2], right[2])   # c and d are the same size
})

test_that("align_rows rebuilds the short card so it stretches, rather than padding around it", {
  # The second pass passes fit_content = FALSE for a card whose target
  # exceeds its own measured height, so its body cell becomes a "null" unit
  # that fills the taller row. The already-tallest card is left alone.
  aligned <- poster(align_spec(LINES, list(align_rows = TRUE,
                                           left = c("a", "c"), right = c("b", "d"))))
  expect_true("null" %in% grid::unitType(card_at(aligned, "left", 1)$heights))
  expect_false("null" %in% grid::unitType(card_at(aligned, "right", 1)$heights))
})

test_that("align_rows leaves the column's total height unchanged", {
  # Growing a row must come out of the trailing spacer, not off the page.
  for (layout in list(list(align_rows = TRUE, left = c("a", "c"), right = c("b", "d")),
                      list(left = c("a", "c"), right = c("b", "d")))) {
    p <- poster(align_spec(LINES, layout))
    expect_equal(sum(col_heights_mm(p, "left")), 841, tolerance = 1e-6)
    expect_equal(sum(col_heights_mm(p, "right")), 841, tolerance = 1e-6)
  }
})

test_that("align_rows handles columns of different lengths", {
  aligned <- poster(align_spec(LINES, list(align_rows = TRUE,
                                           left = c("a", "c", "d"), right = "b")))
  left  <- col_heights_mm(aligned, "left")
  right <- col_heights_mm(aligned, "right")
  expect_equal(left[1], right[1])       # the shared row is aligned
  expect_length(right, 2)               # b, then the spacer
  expect_length(left, 4)                # a, c, d, then the spacer
  expect_equal(sum(left), 841, tolerance = 1e-6)
  expect_equal(sum(right), 841, tolerance = 1e-6)
})

test_that("align_rows only moves 'auto' cards, leaving a relative height alone", {
  # A section with a numeric height takes a share of the leftover space; it
  # has no measured height to line up with, and must not be dragged to one.
  spec <- align_spec(LINES, list(align_rows = TRUE,
                                 left = c("a", "c"), right = c("b", "d")))
  spec$sections$a$height <- 1
  mixed <- poster(spec)
  plain <- poster(align_spec(LINES, list(left = c("a", "c"), right = c("b", "d"))))
  # b keeps its own measured height even though a, beside it, is relative.
  expect_equal(col_heights_mm(mixed, "right")[1], col_heights_mm(plain, "right")[1])
  # a takes everything the "auto" cards in its own column leave behind.
  left <- col_heights_mm(mixed, "left")
  expect_equal(sum(left), 841, tolerance = 1e-6)
  expect_equal(left[1], 841 - left[2], tolerance = 1e-6)
})

test_that("align_rows is a no-op for a single column", {
  one <- poster(align_spec(LINES, list(align_rows = TRUE, only = names(LINES))))
  plain <- poster(align_spec(LINES, list(only = names(LINES))))
  expect_equal(col_heights_mm(one, "only"), col_heights_mm(plain, "only"))
})

test_that("align_rows is not treated as a column of sections", {
  # It shares the layout map with the column names, so it must be filtered
  # out before the columns are built (as `columns` is).
  aligned <- poster(align_spec(LINES, list(align_rows = TRUE,
                                           left = c("a", "c"), right = c("b", "d"))))
  expect_equal(p_names <- aligned$patchwork$layout$name, c("left", "right"))
  expect_false("align_rows" %in% p_names)
})
