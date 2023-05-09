test_that("", {
  expect_error(paa_indices(0, 0))
})

test_that("", {
  expect_error(paa_indices(100, 120))
})

test_that("", {
  expect_no_warning(paa_indices(100, 100))
})

test_that("", {
  expect_equal(paa_indices(10, 1), seq(0, 10, 10 / 1))
})

test_that("", {
  expect_equal(paa_indices(10, 2), seq(0, 10, 10 / 2))
})

test_that("", {
  expect_equal(paa_indices(10, 3), c(0, 3, 6, 10))
})

test_that("", {
  expect_equal(paa_indices(10, 4), c(0, 2, 4, 7, 10))
})

test_that("", {
  expect_equal(paa_indices(10, 5), seq(0, 10, 10 / 5))
})

test_that("", {
  expect_equal(paa_indices(10, 6), c(0, 1, 2, 4, 6, 8, 10))
})

test_that("", {
  expect_equal(paa_indices(10, 7), c(0, 1, 2, 3, 4, 6, 8, 10))
})

test_that("", {
  expect_equal(paa_indices(10, 10), seq(0, 10))
})
