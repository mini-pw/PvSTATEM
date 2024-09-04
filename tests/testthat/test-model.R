library(testthat)




test_that("Artificial model with insufficient number of analytes", {
  expect_no_error(
    model <- Model$new(
      analyte = "Spike_6P_IPP",
      dilutions = c(0.1, 0.2),
      mfi = c(100, 50)
    )
  )

  expect_no_error(print(model))

  expect_equal(model$top_asymptote, 100)
  expect_equal(model$bottom_asymptote, 50)
})
