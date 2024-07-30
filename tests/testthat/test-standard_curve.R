library(testthat)

get_test_plate <- function() {
  names <- c("B", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S")
  types <- ifelse(names == "B", "BLANK", "STANDARD CURVE")
  values <- c(19, 11713, 8387, 5711, 3238.5, 2044, 1078, 571, 262, 138, 81, 33)
  dilutions <- c(NA, "1/50", "1/100", "1/200", "1/400", "1/800", "1/1600", "1/3200", "1/6400", "1/12800", "1/25600", "1/102400")
  dilution_values <- convert_dilutions_to_numeric(dilutions)

  PvSTATEM:::Plate$new(
    plate_name = "plate",
    sample_names = names,
    analyte_names = c("Spike_6P_IPP"),
    sample_types = types,
    dilutions = dilutions,
    dilution_values = dilution_values,
    data = list(Median = data.frame(Spike_6P_IPP = values))
  )
}

test_that("Test plotting the standard curve samples from a plate object", {
  plate <- get_test_plate()
  expect_no_error(plot_standard_curve_analyte(plate, "Spike_6P_IPP"))
})

test_that("Test creating analyte model from a plate object", {
  plate <- get_test_plate()
  expect_no_error(create_standard_curve_model_analyte(plate, "Spike_6P_IPP"))
})
