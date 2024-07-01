test_that("blank adjustment works", {
  kenya_filepath <- system.file("extdata", "kenya.csv", package = "PvSTATEM", mustWork = TRUE)

  plate <- read_data(kenya_filepath, verbose=FALSE)

  initial_mfi_value = plate$samples[[1]]$data["Net MFI", "Etramp5_ag1"]
  expect_equal(initial_mfi_value, 5595.5)

  plate$blank_adjustment(inplace = T, method = "avg")

  recalculated_mfi_value = plate$samples[[1]]$data["Net MFI", "Etramp5_ag1"]
  expect_equal(recalculated_mfi_value, 5545.3333)
})

test_that("plotting standard curves work", {

  kenya_filepath1 <- system.file("extdata", "kenya.csv", package = "PvSTATEM", mustWork = TRUE)
  kenya_filepath6 <- system.file("extdata", "kenya_P6.csv", package = "PvSTATEM", mustWork = TRUE)
  kenya_filepath4 <- system.file("extdata", "kenya_P4.csv", package = "PvSTATEM", mustWork = TRUE)

  plate1 <- read_data(kenya_filepath1, verbose=FALSE)
  plate6 <- read_data(kenya_filepath6, verbose=FALSE)
  plate4 <- read_data(kenya_filepath4, verbose=FALSE)

  plate1$blank_adjustment()
  plate4$blank_adjustment()
  plate6$blank_adjustment()



  plates <- list(plate1, plate4, plate6)


  expect_error(plot_standard_curve_antibody(plates, antibody_name = "Etramp5_ag1"), NA)
})
