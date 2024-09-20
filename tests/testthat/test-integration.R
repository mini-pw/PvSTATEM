library(testthat)

test_that("Fully Parse CovidOISExPONTENT.csv plate data", {
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", verbose = FALSE))

  expect_no_error(p <- plot_mfi_for_analyte(plate, "S2"))
  expect_true(inherits(p, "ggplot"))

  plate <- plate$blank_adjustment(in_place = FALSE)
  expect_equal(plate$blank_adjusted, TRUE)

  expect_equal(plate$sample_names[10:13], c("S", "S", "S", "Unknown2"))
  expect_equal(plate$sample_types[10:13], c("STANDARD CURVE", "STANDARD CURVE", "STANDARD CURVE", "TEST"))
  expect_equal(plate$sample_types[1], "BLANK")
  expect_true(all(is.na(plate$dilutions)))

  expect_equal(plate$plate_name, "CovidOISExPONTENT")
})

test_that("Validation works", {
  path <- system.file("extdata", "random_no_standard_curve.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "random_layout_no_standard_curve.xlsx", package = "PvSTATEM", mustWork = TRUE)

  expect_error(read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))
})

test_that("Fully Parse CovidOISExPONTENT_CO.csv plate data with layout", {
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))

  expect_equal(plate$dilutions[1:4], c(NA, "1/50", "1/100", "1/200"))

  expect_equal(plate$plate_name, "CovidOISExPONTENT")

  expect_no_error(print(plate))
  expect_no_error(summary(plate))

  tmp_dir <- tempdir(check = TRUE)
  test_output_path <- file.path(tmp_dir, "output.csv")

  expect_no_error(
    process_plate(plate, output_path = test_output_path)
  )
  expect_true(file.exists(test_output_path))
  expect_no_error(dilutions <- read.csv(test_output_path))
  file.remove(test_output_path)
})
