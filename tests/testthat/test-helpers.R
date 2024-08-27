library(testthat)

test_that("Test verify numeric join", {
  expect_true(verify_numeric_join(1, 1))
  expect_true(verify_numeric_join(NA, 1))
  expect_true(verify_numeric_join(1, NA))
  expect_false(verify_numeric_join(1, 2))
})

test_that("Test verify character join", {
  expect_true(verify_character_join("a", "a"))
  expect_true(verify_character_join(NULL, "a"))
  expect_true(verify_character_join("a", NULL))
  expect_false(verify_character_join("a", "b"))
})

test_that("Test get join value", {
  expect_equal(get_join_value(1, 1), 1)
  expect_equal(get_join_value(NA, 2), 2)
  expect_equal(get_join_value("a", "b"), NULL)
})

test_that("Test remove empty lists", {
  expect_equal(remove_empty_lists(list(1, 2, list())), list(1, 2))
  expect_equal(remove_empty_lists(list(1, 2, list(1, 2))), list(1, 2, list(1, 2)))
})

test_that("Test is.str.number function", {
  expect_true(is.str.number("1"))
  expect_false(is.str.number("a"))
})

test_that("Test is.scalar", {
  expect_true(is.scalar(1))
  expect_true(is.scalar(NA))
  expect_false(is.scalar(NULL))
  expect_false(is.scalar(c(1, 2)))
})

test_that("Test verbose cat", {
  expect_output(verbose_cat("a", "b"), "ab")
  expect_null(verbose_cat("a", "b", verbose = F))
})

test_that("Test clamp function", {
  expect_equal(clamp(c(1, 0, 2), lower = 1), c(1, 1, 2))
  expect_equal(clamp(c(1, -1, NA), lower = 0), c(1, 0, NA))
  expect_equal(clamp(c(1, 2.2, 3), upper = 2), c(1, 2, 2))
  expect_equal(clamp(c(2, 10, NA), upper = 2), c(2, 2, NA))
})
