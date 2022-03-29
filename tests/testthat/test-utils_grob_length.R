test_that("grob_widths and grob_heights work", {
  small <- grid::rectGrob(width=grid::unit(10, "mm"), height=grid::unit(10, "mm"), gp=grid::gpar(fill="black"))
  tall  <- grid::rectGrob(width=grid::unit(10, "mm"), height=grid::unit(20, "mm"), gp=grid::gpar(fill="red"))
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
  w1 <- grid::convertUnit(grid::unit(rep(1, n), rep("grobwidth",  n), grobs), "mm")
  w2 <- grob_widths(grobs, convert_to="mm")
  expect_equal(w1, w2)
  # height
  h1 <- grid::convertUnit(grid::unit(rep(1, n), rep("grobheight", n), grobs), "mm")
  h2 <- grob_heights(grobs, convert_to="mm")
})
