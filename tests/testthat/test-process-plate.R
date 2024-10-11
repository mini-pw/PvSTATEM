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
    process_plate(plate, output_path = test_output_path)
  )
  expect_true(file.exists(test_output_path))
  expect_no_error(dilutions <- read.csv(test_output_path))
  file.remove(test_output_path)


  # Test additional parameters
  expect_error(
    process_plate(plate, output_path = test_output_path, data_type = "incorrect")
  )

  expect_error(
    process_plate(plate, output_path = test_output_path, normalisation_type = "incorrect")
  )
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
        process_plate(plate, output_path = test_output_path, normalisation_type = "nMFI")
    )
    # Test processing of a plate with reference dilution specified
    expect_no_error(
        process_plate(plate, output_path = test_output_path, normalisation_type = "nMFI", reference_dilution = "1/50")
    )
    expect_true(file.exists(test_output_path))
    expect_no_error(dilutions <- read.csv(test_output_path))
    file.remove(test_output_path)

    # Test additional parameters
    expect_error(
        process_plate(plate, output_path = test_output_path, data_type = "incorrect", normalisation_type = "nMFI")
    )

    expect_error(
        process_plate(plate, output_path = test_output_path, reference_dilution = "400", normalisation_type = "nMFI")
    )

    expect_error(
        process_plate(plate, output_path = test_output_path, reference_dilution = "1/401", normalisation_type = "nMFI")
    )

})