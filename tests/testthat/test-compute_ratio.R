test_that("multiplication works", {

  heights <- grid::unit(c(10, 20, 10, 20), "mm")
  widths  <- grid::unit(c(10, 10, 20, 20), "mm")

  height <- grid::unit(200, "mm")
  width  <- grid::unit(160, "mm")
  space <- grid::unit(10, "mm")

  expect_equal(ratio_reverse(heights = heights, widths = widths, unify = "height"), c(2, 1, 2, 1))
  expect_equal(ratio_reverse(heights = heights, widths = widths, unify = "width"),  c(2, 2, 1, 1))

  ratio_exp1 <- ratio_expantion(height = height, heights = heights, widths = widths, space = space, method="appose")
  ratio_exp2 <- ratio_expantion(width  = width,  heights = heights, widths = widths, space = space, method="appose")
  expect_equal(ratio_exp1, 10)
  expect_equal(ratio_exp2, 2)

  ratio_exp3 <- ratio_expantion(heights = heights, widths = widths, space = space, method="appose")
  expect_equal(ratio_exp3, 1)

})
