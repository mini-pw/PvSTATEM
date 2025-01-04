library(testthat)

test_that("Test processing of a plate", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))

  # Test processing of a plate
  tmp_dir <- tempdir(check = TRUE)
  test_output_path <- file.path(tmp_dir, "output.csv")
  expect_no_error(
    process_plate(plate, output_dir = tmp_dir, filename = "output.csv")
  )
  expect_true(file.exists(test_output_path))
  expect_no_error(dilutions <- read.csv(test_output_path))

  # test a warning in case the file already exists
  expect_warning(
    process_plate(plate, output_dir = tmp_dir, filename = "output.csv")
  )


  # Test additional parameters
  expect_error(
    process_plate(plate, output_dir = tmp_dir, data_type = "incorrect")
  )

  expect_error(
    process_plate(plate, output_dir = tmp_dir, normalisation_type = "incorrect")
  )
  unlink(tmp_dir, recursive = TRUE)
})

test_that("Processing plate with nMFI", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))

  # Test processing of a plate
  tmp_dir <- tempdir(check = TRUE)
  test_output_path <- file.path(tmp_dir, "output.csv")
  expect_no_error(
    process_plate(plate, output_dir = tmp_dir, filename = "output.csv", normalisation_type = "nMFI")
  )
  file.remove(test_output_path)
  # Test processing of a plate with reference dilution specified
  expect_no_error(
    process_plate(plate, output_dir = tmp_dir, filename = "output.csv", normalisation_type = "nMFI", reference_dilution = "1/50")
  )
  expect_true(file.exists(test_output_path))
  expect_no_error(dilutions <- read.csv(test_output_path))
  file.remove(test_output_path)

  # Test additional parameters
  expect_error(
    process_plate(plate, output_dir = tmp_dir, data_type = "incorrect", normalisation_type = "nMFI")
  )

  expect_error(
    process_plate(plate, output_dir = tmp_dir, reference_dilution = "400", normalisation_type = "nMFI")
  )

  expect_error(
    process_plate(plate, output_dir = tmp_dir, reference_dilution = "1/401", normalisation_type = "nMFI")
  )
  unlink(tmp_dir, recursive = TRUE)
})


test_that("raw MFI in dataframe", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))

  # Test processing of a plate, without raw MFI
  tmp_dir <- tempdir(check = TRUE)
  test_output_path <- file.path(tmp_dir, "output.csv")
  expect_no_error(
    output_df <- process_plate(plate, output_dir = tmp_dir, filename = "output.csv", normalisation_type = "nMFI", include_raw_mfi = FALSE)
  )
  stopifnot(all(colnames(output_df) == plate$analyte_names))

  file.remove(test_output_path)

  # Test processing of a plate, with raw MFI
  expect_no_error(
    output_df <- process_plate(plate, output_dir = tmp_dir, filename = "output.csv", normalisation_type = "nMFI", include_raw_mfi = TRUE)
  )
  stopifnot(all(colnames(output_df) == c(plate$analyte_names, paste0(plate$analyte_names, "_raw"))))

  unlink(tmp_dir, recursive = TRUE)
})
