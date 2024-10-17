library(testthat)

test_that("get_nmfi works on our data with multiple parameters", {
  # Read plate
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))

  get_nmfi(plate, reference_dilution = 1 / 400)
  get_nmfi(plate, reference_dilution = "1/50")

  get_nmfi(plate, reference_dilution = 1 / 400, data_type = "Mean")
})

test_that("get_nmfi with incorrect params", {
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path, verbose = FALSE))

  expect_error(get_nmfi(plate, reference_dilution = 1 / 401))

  expect_error(get_nmfi(plate, reference_dilution = "1/401"))

  expect_error(get_nmfi(plate, reference_dilution = 1 / 400, data_type = "incorrect"))
})


test_that("get_nmfi on artificial plate", {
  path <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "random_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  expect_no_error(plate <- read_luminex_data(path, format = "xPONENT", verbose = FALSE))

  nmfi <- get_nmfi(plate, reference_dilution = 1 / 50)

  reference_dilution_index <- which(plate$dilution_values == 1 / 50)

  reference_dilution_values <- plate$data[["Median"]][reference_dilution_index, ]

  mfi_values <- plate$data[["Median"]][plate$sample_types == "TEST", ]

  for (i in 1:ncol(mfi_values)) {
    expect_equal(nmfi[, i], mfi_values[, i] / reference_dilution_values[[i]])
  }
})
