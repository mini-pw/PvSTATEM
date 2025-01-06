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
  dilutions <- c(NA, "1/50", "1/100", "1/200", "1/400", "1/800", "1/1600", "1/3200", "1/6400", "1/12800", "1/25600", "1/102400", NA)
  dilution_values <- convert_dilutions_to_numeric(dilutions)
  plate_datetime <- as.POSIXct("2020-01-01 12:00:00", tz = "UTC")

  Plate$new(
    plate_name = "plate",
    sample_names = names,
    sample_types = types,
    sample_locations = locations,
    analyte_names = c("Spike_6P_IPP"),
    dilutions = dilutions,
    dilution_values = dilution_values,
    data = list(Median = data.frame(Spike_6P_IPP = values)),
    plate_datetime = plate_datetime
  )
}

get_test_list_of_plates <- function() {
  list(
    test = get_test_plate()
  )
}

get_list_of_plates <- function() {
  dir <- system.file("extdata", "multiplate_reallife_reduced",
    package = "PvSTATEM", mustWork = TRUE
  )

  output_dir <- tempdir(check = TRUE)
  dir.create(output_dir)
  real_list_of_plates <- process_dir(dir,
    return_plates = TRUE,
    format = "xPONENT",
    output_dir = output_dir
  )
  unlink(output_dir, recursive = TRUE)
  real_list_of_plates
}

test_that("Plot Levey-Jennings chart", {
  list_of_plates <- get_test_list_of_plates()
  expect_no_error(plot_levey_jennings(list_of_plates, "Spike_6P_IPP"))
})

test_that("Plot Levey-Jennings chart with not a list", {
  expect_error(plot_levey_jennings("not_list", "Spike_6P_IPP"))
})

test_that("Plot Levey-Jennings chart with empty list", {
  expect_error(plot_levey_jennings(list(), "Spike_6P_IPP"))
})

test_that("Plot Levey-Jennings chart with less than 10 plates", {
  list_of_plates <- get_test_list_of_plates()
  expect_warning(plot_levey_jennings(list_of_plates, "Spike_6P_IPP"))
})

test_that("Plot Levey-Jennings chart with not a Plate object", {
  list_of_plates <- list("not_plate")
  expect_error(plot_levey_jennings(list_of_plates, "Spike_6P_IPP"))
})

test_that("Plot Levey-Jennings chart with not existing analyte", {
  list_of_plates <- get_test_list_of_plates()
  expect_error(plot_levey_jennings(list_of_plates, "not_existing"))
})


test_that("Plot Levey-Jennings chart with not existing dilution", {
  list_of_plates <- get_test_list_of_plates()
  expect_error(plot_levey_jennings(list_of_plates, "Spike_6P_IPP", "1/2"))
})

test_that("Plot Levey-Jennings chart with incorrect format of dilution", {
  list_of_plates <- get_test_list_of_plates()
  expect_error(plot_levey_jennings(list_of_plates, "Spike_6P_IPP", 1 / 400))
})


test_that("Plot Levey-Jennings chart with invalid horizontal lines parameter", {
  list_of_plates <- get_test_list_of_plates()
  expect_error(plot_levey_jennings(list_of_plates, "Spike_6P_IPP", "1/400", "not_numeric"))
})

test_that("Plot Levey-Jennings chart with not a string analyte", {
  list_of_plates <- get_test_list_of_plates()
  expect_error(plot_levey_jennings(list_of_plates, 1, "1/400"))
})
