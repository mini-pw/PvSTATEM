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

  Plate$new(
    plate_name = "plate",
    sample_names = names,
    sample_types = types,
    sample_locations = locations,
    analyte_names = c("Spike_6P_IPP"),
    dilutions = dilutions,
    dilution_values = dilution_values,
    data = list(Median = data.frame(Spike_6P_IPP = values))
  )
}

get_test_list_of_plates <- function() {
  list(
    test = get_test_plate()
  )
}

get_list_of_plates <- function() {
  dir <- system.file("extdata", "multiplate_reallife_reduced", package = "PvSTATEM", mustWork = TRUE)

  output_dir <- tempdir(check = TRUE)
  dir.create(output_dir)
  real_list_of_plates <- process_dir(dir, return_plates = TRUE, format = "xPONENT", output_dir = output_dir)
  unlink(output_dir, recursive = TRUE)
  real_list_of_plates
}

test_that("Test plotting the standard curve samples from a plate object", {
  plate <- get_test_plate()
  expect_no_error(plot_standard_curve_analyte(plate, "Spike_6P_IPP"))
})

test_that("Test plotting the standard curve samples with not a plate object", {
  expect_error(plot_standard_curve_analyte("not_plate", "Spike_6P_IPP"))
})

test_that("Test plotting the standard curve samples with not existing analyte", {
  plate <- get_test_plate()
  expect_error(plot_standard_curve_analyte(plate, "not_existing"))
})

test_that("Test creating analyte model from a plate object", {
  plate <- get_test_plate()
  expect_no_error(create_standard_curve_model_analyte(plate, "Spike_6P_IPP"))
})

test_that("Test plotting the full plot", {
  plate <- get_test_plate()
  model <- create_standard_curve_model_analyte(plate, "Spike_6P_IPP")
  expect_no_error(plot_standard_curve_analyte_with_model(plate, model))
})

test_that("Test over max extrapolation", {
  plate <- get_test_plate()
  model <- create_standard_curve_model_analyte(plate, "Spike_6P_IPP")
  expect_warning(plot_standard_curve_analyte_with_model(
    plate, model,
    over_max_extrapolation = Inf
  ))
})

test_that("Plot Stacked Standard Curve", {
  list_of_plates <- get_test_list_of_plates()
  expect_no_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP"))
})


test_that("Plot Stacked Standard Curve with not existing analyte", {
  list_of_plates <- get_test_list_of_plates()
  expect_error(plot_standard_curve_stacked(list_of_plates, "not_existing"))
})

test_that("Plot Stacked Standard Curve with empty list", {
  list_of_plates <- list()
  expect_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP"))
})

test_that("Plot Stacked Standard Curve with invalids object in list", {
  list_of_plates <- list("not_plate", get_test_plate(), "not_other_plate")
  expect_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP"))
})

test_that("Plot Stacked Standard Curve with invalid log scale", {
  list_of_plates <- get_test_list_of_plates()
  expect_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP", log_scale = "not_existing"))
})

test_that("Plot Stacked Standard Curve with string instead of list", {
  list_of_plates <- "not_list"
  expect_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP"))
})

test_that("Plot Stacked Standard Curve with number instead of list", {
  list_of_plates <- 123.45
  expect_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP"))
})

test_that("Plot Stacked Standard Curve invalid date legend type", {
  list_of_plates <- get_test_list_of_plates()
  expect_no_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP", legend_type = "date"))
})

test_that("Plot Stacked Standard Curve invalid plate_name legend type", {
  list_of_plates <- get_test_list_of_plates()
  expect_no_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP", legend_type = "plate_name"))
})

test_that("Plot Stacked Standard Curve invalid legend type", {
  list_of_plates <- get_test_list_of_plates()
  expect_error(plot_standard_curve_stacked(list_of_plates, "Spike_6P_IPP", legend_type = "not_existing"))
})

test_that("Plot Stacked Standard Curve with real data", {
  list_of_plates <- get_list_of_plates()
  expect_no_error(plot_standard_curve_stacked(list_of_plates, "ME"))
  expect_no_error(plot_standard_curve_stacked(list_of_plates, "ME", log_scale = "dilutions"))
  expect_no_error(plot_standard_curve_stacked(list_of_plates, "ME", log_scale = "MFI"))
  expect_error(plot_standard_curve_stacked(list_of_plates, "not_existing"))
})
