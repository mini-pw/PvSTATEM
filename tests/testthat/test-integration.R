library(testthat)

test_that("Fully Parse CovidOISExPONTENT.csv plate data", {
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", verbose = FALSE))
  expect_warning(plate <- plate$blank_adjustment(in_place = FALSE))
  expect_equal(plate$blank_adjusted, TRUE)

  expect_equal(plate$sample_names[10:13], c("S", "S", "S", "Unknown2"))
  expect_equal(plate$sample_types[10:13], c("STANDARD CURVE", "STANDARD CURVE", "STANDARD CURVE", "TEST"))
  expect_equal(plate$sample_types[1], "BLANK")
  expect_true(all(is.na(plate$dilutions)))

})

test_that("Fully Parse CovidOISExPONTENT_CO.csv plate data with layout", {
  path <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))


  expect_equal(plate$dilutions[1:4], c(NA, "1/50", "1/100", "1/200"))
})
