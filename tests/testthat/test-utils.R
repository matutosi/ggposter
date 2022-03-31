test_that("dots2list works", {
  expect_equal(dots2list(1,2,3,4), list(1,2,3,4))
  expect_equal(dots2list(list(1,2,3,4)), list(1,2,3,4))
})
