test_that("grob_widths and grob_heights work", {
  small <- grid::rectGrob(width=grid::unit(1, "cm"), height=grid::unit(1, "cm"), gp=grid::gpar(fill="black"))
  tall  <- grid::rectGrob(width=grid::unit(1, "cm"), height=grid::unit(2, "cm"), gp=grid::gpar(fill="red"))
  grobs <- list(small, tall)
  n <- length(grobs)
  # width
  w1 <- grid::unit(rep(1, n), rep("grobwidth",  n), grobs)
  w2 <- grob_widths(grobs)
  expect_equal(w1, w2)
  # height
  h1 <- grid::unit(rep(1, n), rep("grobheight", n), grobs)
  h2 <- grob_heights(grobs)
  expect_equal(h1, h2)
  # converted
  expect_equal(grid::convertUnit(w1, "mm"), grid::convertUnit(w2, "mm"))
  expect_equal(grid::convertUnit(h1, "mm"), grid::convertUnit(h2, "mm"))
})

