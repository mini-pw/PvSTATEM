require(testthat)

test_that("blank adjustment", {
  plate_file <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE)

  plate <- read_data(plate_file, verbose = FALSE)

  initial_mfi_value <- plate$samples[[2]]$data["Median", "Etramp5_ag1"]
  expect_equal(initial_mfi_value, 1292)

  plate$blank_adjustment(inplace = T, method = "avg")

  recalculated_mfi_value <- plate$samples[[2]]$data["Median", "Etramp5_ag1"]
  expect_equal(recalculated_mfi_value, 1272)
})

test_that("plotting standard curves work", {
  plate_file1 <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE)
  plate_file2 <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE)

  plate1 <- read_data(plate_file1, verbose = FALSE)
  plate2 <- read_data(plate_file2, verbose = FALSE)

  plate1$blank_adjustment()
  plate2$blank_adjustment()

  plates <- list(plate1, plate2)

  expect_error(plot_standard_curve_antibody(plates, antibody_name = "Etramp5_ag1"), NA)
})

test_that("plotting standard curves fail - different dilutions", {
  plate_file1 <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE)
  plate_file2 <- system.file("extdata", "random2.csv", package = "PvSTATEM", mustWork = TRUE)

  plate1 <- read_data(plate_file1, verbose = FALSE)
  plate2 <- read_data(plate_file2, verbose = FALSE)

  plate1$blank_adjustment()
  plate2$blank_adjustment()

  plates <- list(plate1, plate2)

  expect_error(plot_standard_curve_antibody(plates, antibody_name = "Etramp5_ag1"))
})
