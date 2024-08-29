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

test_that("Test printing the plate and misc functions", {
  plate <- get_test_plate()
  expect_no_error(print(plate))
  expect_no_error(summary(plate))
  expect_no_error(summary(plate, include_names = TRUE))
})

test_that("Blank adjustments", {
  plate <- get_test_plate()

  plate$data$Median[[12, "Spike_6P_IPP"]] <- 10

  expect_no_error(plate_adj <- plate$blank_adjustment(in_place = FALSE))

  expect_equal(plate_adj$data$Median[[12, "Spike_6P_IPP"]], plate_adj$data$Median[[1, "Spike_6P_IPP"]])


  expect_error(plate_adj$blank_adjustment())

  expect_error(plate$blank_adjustment(threshold = "wrong method"))

  expect_no_error(plate$blank_adjustment(threshold = "median"))

  empty_plate <- Plate$new(
    plate_name = "empty_plate",
    sample_names = c("S"),
    analyte_names = "analyte",
    dilutions = c("1/1")
  )

  expect_error(empty_plate$blank_adjustment())
})

test_that("Getters in plate object", {
  plate <- get_test_plate()
  empty_plate <- Plate$new(
    plate_name = "empty_plate",
    sample_names = c("S"),
    analyte_names = "analyte"
  )

  # get_dilutions
  expect_error(plate$get_dilution("incorrect sample type"))
  expect_no_error(plate$get_dilution("STANDARD CURVE"))
  expect_error(empty_plate$get_dilution("ALL"))

  expect_error(plate$get_dilution("incorrect analyte"))
  expect_no_error(plate$get_dilution("STANDARD CURVE"))
  expect_error(empty_plate$get_dilution("ALL"))

  # get_data
  expect_no_error(plate$get_data("Spike_6P_IPP", data_type = "Median"))

  expect_error(plate$get_data("incorrect sample type"))
  expect_error(plate$get_data(NULL))
  expect_error(plate$get_data("Spike_6P_IPP", sample_type = "incorrect sample"))
  expect_error(plate$get_data("Spike_6P_IPP", data_type = "average"))
})
