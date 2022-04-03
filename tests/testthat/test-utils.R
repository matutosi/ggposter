test_that("dots2list works", {
  expect_equal(list(1,2,3,4), dots2list(1,2,3,4))
  expect_equal(list(1,2,3,4), dots2list(list(1,2,3,4)))
})

test_that("as_same can create 'argument = argument, ...'", {
  x <- "a"
  times <- 10
  expect_equal("x = x, times = times", as_same(x, times))
  expect_equal(list(1,2,3,4), dots2list(list(1,2,3,4)))
})

test_that("same can create the same result", {
  x <- "a"
  times <- 10
  expect_equal(rep(x = x, times = times), do.call(rep.int, same(x, times)))
  expect_equal(rep(x = x, times = times), do.call(rep.int, same(times, x)))
})
