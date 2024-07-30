library(testthat)

test_that("plot_standard_curve_antibody", {
  plate <- PvSTATEM:::Plate$new(
    plate_name = "plate",
    sample_names = c("sample1", "sample2", "sample3"),
    analyte_names = c("AMA"),
    sample_types = c("STANDARD CURVE", "STANDARD CURVE", "TEST"),
    dilutions = c("1/5", "1/100", NA),
    dilution_values = c(0.2, 0.01, NA),
    data = list(Median = data.frame(row.names = c("sample1", "sample2", "sample3"), AMA = c(0.1, 0.2, 0.3)))
  )
  expect_no_error(plot_standard_curve_antibody(plate, "AMA"))
})
