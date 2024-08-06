library(testthat)

test_that("Fully Parse CovidOISExPONTENT.csv plate data", {
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT"))
  expect_warning(plate <- plate$blank_adjustment(in_place = FALSE))
  expect_equal(plate$blank_adjusted, TRUE)
})

test_that("Fully Parse CovidOISExPONTENT_CO.csv plate data with layout", {
  path <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path))
})
