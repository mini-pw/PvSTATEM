library(testthat)

test_that("Empty file triggers an error", {
  path <- "incorrect_filepath.csv"
  expect_error(read_layout_data(path, separator = ","))
})

test_that("Test reading layout from CSV file", {
  path <- system.file("extdata", "CovidOISExPONTENT_layout.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_data <- read_layout_data(path, separator = ",")
  expect_is(layout_data, "matrix")
  expect_equal(layout_data["A", 1], "B") # BLANK
})

test_that("Test reading layout from a excel file", {
  path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  layout_data <- read_layout_data(path, separator = ",")
  expect_is(layout_data, "matrix")
  expect_equal(layout_data["A", 1], "B") # BLANK
})
