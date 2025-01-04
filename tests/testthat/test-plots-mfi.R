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

test_that("Test plotting the mfi plot ", {
  plate <- get_test_plate()
  expect_no_error(plot_mfi_for_analyte(plate, "Spike_6P_IPP"))
})

test_that("Edge incorrect parameters", {
  plate <- get_test_plate()
  expect_error(plot_mfi_for_analyte(plate, "Spike_6P_IPP", plot_type = "barplot"))
  expect_error(plot_mfi_for_analyte(plate, "Spike_6P_IPP", data_type = "Average"))
  expect_error(plot_mfi_for_analyte(plate, "incorrect analyte"))
})
