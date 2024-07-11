test_that("reading file", {
  plate_filepath <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE)

  plate <- read_data(plate_filepath, verbose = FALSE)
  expect_equal(length(plate$samples), 8)
})

test_that("reading file with layout", {
  plate_filepath <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_filepath <- system.file("extdata", "random_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  plate <- read_data(plate_filepath, layout_file_path = layout_filepath, check_plate = FALSE, verbose = FALSE)
  expect_equal(length(plate$samples), 8)
})

test_that("incorrect format detection", { # add better handling of this error
  kenya_filepath <- system.file("extdata", "random_broken_colB.csv", package = "PvSTATEM", mustWork = TRUE)

  expect_error(read_data(kenya_filepath, verbose = FALSE))
})
