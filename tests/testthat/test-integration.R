library(testthat)

test_that("Fully Parse CovidOISExPONTENT.csv plate data", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", verbose = FALSE))
  # Check basic properties
  expect_equal(plate$plate_name, "CovidOISExPONTENT")
  expect_equal(plate$sample_names[10:13], c("S", "S", "S", "Unknown2"))
  expect_equal(plate$sample_types[10:13], c("STANDARD CURVE", "STANDARD CURVE", "STANDARD CURVE", "TEST"))
  expect_equal(plate$sample_types[1], "BLANK")
  expect_true(all(is.na(plate$dilutions)))
  # Check plotting mfi for analyte
  expect_no_error(p <- plot_mfi_for_analyte(plate, "S2"))
  expect_true(inherits(p, "ggplot"))
  expect_no_error(p <- plot_mfi_for_analyte(plate, "S2", plot_type = "boxplot", plot_outliers = TRUE))
  expect_true(inherits(p, "ggplot"))
  # Check blank adjustment
  plate <- plate$blank_adjustment(in_place = FALSE)
  expect_equal(plate$blank_adjusted, TRUE)
})

test_that("Test error catching without the layout file", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)

  # sample names
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", verbose = FALSE, use_layout_sample_names = FALSE))

  expect_equal(plate$sample_names[10:13], c("S", "S", "S", "Unknown2"))

  # sample types
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", verbose = FALSE, use_layout_types = FALSE))

  expect_equal(plate$sample_types[10:13], c("STANDARD CURVE", "STANDARD CURVE", "STANDARD CURVE", "TEST"))
  expect_true(all(is.na(plate$dilutions)))

  # dilution values
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", verbose = FALSE, use_layout_dilutions = FALSE))
  expect_true(all(is.na(plate$dilutions)))
})

test_that("Test reading with layout", {
  path <- system.file("extdata", "random_no_standard_curve.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "random_layout_no_standard_curve.xlsx", package = "PvSTATEM", mustWork = TRUE)

  expect_error(read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))
})

test_that("Fully Parse CovidOISExPONTENT_CO.csv plate data with layout", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))
  # Check basic properties
  expect_equal(plate$plate_name, "CovidOISExPONTENT")
  expect_equal(plate$dilutions[1:4], c(NA, "1/50", "1/100", "1/200"))
  # Check S3 functions
  expect_output(print(plate))
  expect_output(summary(plate))
  # Test processing of a plate
  tmp_dir <- tempdir(check = TRUE)
  test_output_path <- file.path(tmp_dir, "output.csv")
  expect_no_error(
    capture.output(
      process_plate(plate, output_dir = tmp_dir, filename = "output.csv", verbose = FALSE),
      file = NULL
    )
  )
  expect_true(file.exists(test_output_path))
  expect_no_error(dilutions <- read.csv(test_output_path))
  file.remove(test_output_path)
})

test_that("Test parsing CovidOISExPONTENT_CO.csv with one function", {
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(
    capture.output(
      process_file(path, layout_path, verbose = FALSE)
    )
  )
})

test_that("Read a INTELLIFLEX mock file", {
  path <- system.file("extdata", "random_intelliflex.csv", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(output <- read_intelliflex_format(path, verbose = verbose))
  expect_no_error(postprocess_intelliflex(output, verbose = verbose))
})

test_that("Test xponent file with holes in the layout", {
  path <- system.file("extdata", "CovidOISExPONTENT_CO_with_holes.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_CO_with_holes_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE)


  expect_equal(sum(plate$sample_types == "STANDARD CURVE"), 11)
  expect_equal(sum(plate$sample_types == "BLANK"), 1)

  expect_equal(plate$layout[96], "1/102400")
  expect_equal(plate$sample_locations[1], "H1")

  expect_equal(sum(!is.na(plate$dilutions)), 11)
  expect_equal(length(plate$sample_locations), 44)

  expect_equal(sum(is.na(plate$sample_names)), 0)
  expect_equal(sum(is.na(plate$sample_types)), 0)
})

test_that("Test ignoring datatypes with missing rows", {
  path <- system.file("extdata", "CovidOISExPONTENT_missing_rows.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  expect_warning(
    plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE)
  )

  expect_equal(sum(plate$sample_types == "STANDARD CURVE"), 11)
  expect_true("Median" %in% names(plate$data))
  expect_false("Net MFI" %in% names(plate$data))
  expect_equal(length(plate$sample_types), 96)
})
