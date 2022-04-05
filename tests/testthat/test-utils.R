test_that("dots2list works", {
  expect_equal(list(1,2,3,4), dots2list(1,2,3,4))
  expect_equal(list(1,2,3,4), dots2list(list(1,2,3,4)))
})
