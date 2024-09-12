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

get_real_plate <- function() {
  plate_filepath <- system.file("extdata", "CovidOISExPONTENT_CO.csv",
    package = "PvSTATEM"
  )
  layout_filepath <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx",
    package = "PvSTATEM"
  )

  read_luminex_data(plate_filepath, layout_filepath)
}

test_that("Test plotting the mfi plot ", {
  plate <- get_real_plate()
  expect_no_error(plot_layout(plate))
  expect_no_error(p <- plot_counts(plate, "OC43_NP_NA", plot_counts = TRUE, plot_legend = TRUE))
  print(p)
})


test_that("Misc errors of plate plots", {
  plate <- get_test_plate()
  expect_error(plot_counts(plate, "Spike_6P_IPP"))
})

test_that("higher threshold lower than lower", {
  plate <- get_real_plate()
  expect_error(p <- plot_counts(plate, "OC43_NP_NA", plot_counts = FALSE, plot_legend = FALSE, lower_threshold = 100, higher_threshold = 50))
})


test_that("properly specified thresholds for counts", {
  plate <- get_real_plate()
  expect_no_error(p <- plot_counts(plate, "OC43_NP_NA", plot_counts = FALSE, plot_legend = TRUE, lower_threshold = 50, higher_threshold = 150))
})
