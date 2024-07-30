test_that("Test convert dilutions to numeric", {
  dilutions <- c(NA, "1/50", "1/100")
  dilution_values <- convert_dilutions_to_numeric(dilutions)
  expect_equal(dilution_values, c(NA, 0.02, 0.01))
})
