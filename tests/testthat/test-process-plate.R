library(testthat)

test_that("Test processing of a plate", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "SerolyzeR", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "SerolyzeR", mustWork = TRUE)
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

  # Test outputing MFI vaules to CSV file through the process_plate function
  expect_no_error(
    process_plate(plate, output_dir = tmp_dir, filename = "output.csv", output_type = "MFI")
  )


  # Test additional parameters
  expect_error(
    process_plate(plate, output_dir = tmp_dir, data_type = "incorrect")
  )

  expect_error(
    process_plate(plate, output_dir = tmp_dir, output_type = "incorrect")
  )
  unlink(tmp_dir, recursive = TRUE)
})

test_that("Processing plate with nMFI", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "SerolyzeR", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "SerolyzeR", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))

  # Test processing of a plate
  tmp_dir <- tempdir(check = TRUE)
  test_output_path <- file.path(tmp_dir, "output.csv")
  expect_no_error(
    process_plate(plate, output_dir = tmp_dir, filename = "output.csv", output_type = "nMFI")
  )
  file.remove(test_output_path)
  # Test processing of a plate with reference dilution specified
  expect_no_error(
    process_plate(plate, output_dir = tmp_dir, filename = "output.csv", output_type = "nMFI", reference_dilution = "1/50")
  )
  expect_true(file.exists(test_output_path))
  expect_no_error(dilutions <- read.csv(test_output_path))
  file.remove(test_output_path)

  # Test additional parameters
  expect_error(
    process_plate(plate, output_dir = tmp_dir, data_type = "incorrect", output_type = "nMFI")
  )

  expect_error(
    process_plate(plate, output_dir = tmp_dir, reference_dilution = "400", output_type = "nMFI")
  )

  expect_error(
    process_plate(plate, output_dir = tmp_dir, reference_dilution = "1/401", output_type = "nMFI")
  )
  unlink(tmp_dir, recursive = TRUE)
})


test_that("Test rows and columns in the output dataframes", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "SerolyzeR", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "SerolyzeR", mustWork = TRUE)
  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))

  # Test processing of a plate, without raw MFI
  tmp_dir <- tempdir(check = TRUE)
  test_output_path <- file.path(tmp_dir, "output.csv")
  expect_no_error(
    output_df <- process_plate(plate, output_dir = tmp_dir, filename = "output.csv", output_type = "nMFI")
  )
  stopifnot(all(colnames(output_df) == plate$analyte_names))
  stopifnot(all(rownames(output_df) == plate$sample_names[plate$sample_types == "TEST"]))

  file.remove(test_output_path)
  unlink(tmp_dir, recursive = TRUE)
})
