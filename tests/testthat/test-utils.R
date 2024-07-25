library(testthat)

test_that("test is.scalar", {
  expect_true(is.scalar(1))
  expect_false(is.scalar(c(1, 2)))
  expect_true(is.scalar("test"))
  expect_true(is.scalar(c("test")))
  expect_false(is.scalar(c("test", "more")))
  expect_false(is.scalar(list()))
  expect_false(is.scalar(list(a = "test")))
})
