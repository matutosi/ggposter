test_that("shoot create the same result", {
  x <- "a"
  times <- 10
  expect_equal(rep.int(x = x, times = times), shoot(rep.int, x, times))
  expect_equal(rep.int(x = x, times = times), shoot(rep.int, times, x))
})

test_that("same create the same result", {
  x <- "a"
  times <- 10
  expect_equal(rep.int(x = x, times = times), do.call(rep.int, eval(same(x, times))))
  expect_equal(rep.int(x = x, times = times), do.call(rep.int, eval(same(times, x))))
})

test_that("as_same create 'argument = argument, ...'", {
  x <- "a"
  times <- 10
  expect_equal("x = x, times = times", as_same(x, times))
})
