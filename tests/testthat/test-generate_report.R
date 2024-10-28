library(testthat)

get_test_plate <- function() {
  names <- c("B", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "TEST")
  locations <- c(
    "A1", "A2", "A3", "A4", "A5", "A6",
    "B1", "B2", "B3", "B4", "B5", "B6"
  )
  types <- ifelse(names == "B", "BLANK", "STANDARD CURVE")
  types[names == "TEST"] <- "TEST"
  values <- c(19, 11713, 8387, 5711, 3238.5, 2044, 1078, 571, 262, 138, 81, 4000)
  dilutions <- c("1/2", "1/50", "1/100", "1/200", "1/400", "1/800", "1/1600", "1/3200", "1/6400", "1/12800", "1/25600", "1/102400")
  dilution_values <- convert_dilutions_to_numeric(dilutions)

  Plate$new(
    plate_name = "plate",
    sample_names = names,
    sample_types = types,
    sample_locations = locations,
    analyte_names = c("Spike_6P_IPP"),
    dilutions = dilutions,
    dilution_values = dilution_values,
    data = list(Median = data.frame(Spike_6P_IPP = values), Count = data.frame(Spike_6P_IPP = values))
  )
}

test_that("Test generate_plate_report function", {
  plate <- get_test_plate()

  tmp_dir <- tempdir(check = TRUE)

  expect_error(generate_plate_report())
  expect_no_error(generate_plate_report(plate, output_dir = tmp_dir))
  expect_no_error(generate_plate_report(plate, output_dir = tmp_dir, filename = "test_report.html"))
  file.remove(file.path(tmp_dir, "test_report.html"))
  expect_no_error(generate_plate_report(plate, output_dir = tmp_dir, filename = "test_report.html", counts_lower_threshold = 40, counts_higher_threshold = 80))
  file.remove(file.path(tmp_dir, "test_report.html"))
  expect_no_error(generate_plate_report(plate, output_dir = tmp_dir, filename = "test_report.html", additional_notes = "This is a test report."))
  file.remove(file.path(tmp_dir, "test_report.html"))


  multiline_note <- "This is a test report.\nThis is a test report."
  expect_no_error(generate_plate_report(plate, output_dir = tmp_dir, filename = "test_report.html", additional_notes = multiline_note))
  file.remove(file.path(tmp_dir, "test_report.html"))
})

test_that("Test generate_levey_jennings_report function", {
  plate <- get_test_plate()

  tmp_dir <- tempdir(check = TRUE)

  expect_error(generate_levey_jennings_report())
  expect_no_error(generate_levey_jennings_report(list(plate), output_dir = tmp_dir))
  expect_no_error(generate_levey_jennings_report(list(plate), output_dir = tmp_dir, filename = "test_report.html"))
})
