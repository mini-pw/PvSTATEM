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

test_that("Test high dose hook detection and handling", {
  dilutions <- c(
    1 / 50, 1 / 100, 1 / 200, 1 / 400, 1 / 800, 1 / 1600, 1 / 4000, 1 / 16000
  )
  # Normal case
  mfi <- c(2000, 1000, 500, 300, 200, 100, 50, 25)
  expect_true(all(handle_high_dose_hook(mfi, dilutions)))

  # High dose hook
  mfi <- c(2000, 500, 1000, 300, 200, 100, 50, 200)
  expect_warning(p <- handle_high_dose_hook(mfi, dilutions))
  expect_equal(p, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))

  # High dose hook insufficient samples
  # That would remove all but 3 samples which is less than the minimum required
  expect_warning(p <- handle_high_dose_hook(mfi, dilutions, high_dose_threshold = 1 / 800))
  expect_true(all(p))
})
