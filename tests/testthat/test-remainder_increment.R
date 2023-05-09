test_that("", {
  expect_equal(remainder_increment(10, 10), rep(0, 10))
})

test_that("", {
  expect_equal(remainder_increment(20, 5), rep(0, 5))
})

test_that("", {
  expect_equal(remainder_increment(21, 5), c(rep(0, 4), 1))
})

test_that("", {
  expect_equal(remainder_increment(22, 5), c(rep(0, 3), 1, 2))
})

test_that("", {
  expect_error(remainder_increment(0, 0))
})

test_that("", {
  expect_error(remainder_increment(100, 101))
})
