library(testthat)

test_that("Test finding layout file", {
  plate_filepath <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  random_plate_filepath <- system.file("extdata", "random_intelliflex.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  layout_filepath <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  output <- find_layout_file(plate_filepath)
  expect_true(check_path_equal(output, layout_filepath))
  expect_error(find_layout_file(random_plate_filepath))
})

test_that("Test checking for mbr file", {
  plate1_filepath <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  plate2_filepath <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  plate3_filepath <- system.file("extdata", "random_intelliflex.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  non_matched_filepath <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  layout_filepath <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  expect_true(is_mbr_data_file(plate1_filepath))
  expect_true(is_mbr_data_file(plate2_filepath))
  expect_true(is_mbr_data_file(plate3_filepath))
  expect_false(is_mbr_data_file(non_matched_filepath))
  expect_false(is_mbr_data_file(layout_filepath))
})

test_that("Test detecting format", {
  plate1_filepath <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  plate2_filepath <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  plate3_filepath <- system.file("extdata", "random_intelliflex.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset

  expect_equal(detect_mbr_format(plate1_filepath), "xPONENT")
  expect_equal(detect_mbr_format(plate2_filepath), "xPONENT")
  expect_equal(detect_mbr_format(plate3_filepath), "INTELLIFLEX")
})

test_that("Test obtaining an output directory", {
  plate_filepath <- system.file("extdata", "multiplate_mock", "other", "random2_xponent.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  input_dir <- fs::path_dir(plate_filepath)
  input_dir_parent <- fs::path_dir(input_dir)
  specified_output_dir <- fs::path(tempdir(check = TRUE))
  specified_output_dir_plus <- fs::path_join(c(specified_output_dir, fs::path("other")))

  expect_equal(get_output_dir(plate_filepath, input_dir), input_dir)
  expect_equal(get_output_dir(
    plate_filepath, input_dir_parent,
    flatten_output = FALSE
  ), input_dir)
  expect_equal(get_output_dir(
    plate_filepath, input_dir_parent,
    flatten_output = TRUE
  ), input_dir_parent)
  expect_equal(get_output_dir(
    plate_filepath, input_dir_parent,
    output_dir = specified_output_dir
  ), specified_output_dir_plus)
  expect_equal(get_output_dir(
    plate_filepath, input_dir_parent,
    output_dir = specified_output_dir,
    flatten_output = TRUE
  ), specified_output_dir)
})


test_that("Test processing a mock directory", {
  dir <- system.file("extdata", "multiplate_mock", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  expect_no_error(capture.output(
    process_dir(dir, dry_run = T, recurse = T, flatten_output = T)
  ))
  expect_no_error(capture.output(
    process_dir(dir, dry_run = T, recurse = F, flatten_output = T)
  ))
  expect_no_error(capture.output(
    process_dir(dir, dry_run = T, recurse = T, flatten_output = F)
  ))
  expect_no_error(capture.output(
    process_dir(dir, dry_run = T, recurse = T, flatten_output = F, output_dir = tempdir(check = TRUE))
  ))
})

test_that("Test processing a directory with a single plate", {
  dir <- system.file("extdata", "multiplate_lite", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset
  output_dir <- tempdir(check = TRUE)
  plates <- process_dir(dir, return_plates = T, output_dir = output_dir)
  expect_length(plates, 1)
})
