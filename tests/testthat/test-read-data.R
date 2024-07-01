test_that("reading file works", {
  kenya_filepath <- system.file("extdata", "kenya.csv", package = "PvSTATEM", mustWork = TRUE)

  plate <- read_data(kenya_filepath, verbose=FALSE)
  expect_equal(length(plate$samples), 96)
})

test_that("reading file works", {
  oise_filepath <- system.file("extdata", "OISE.csv", package = "PvSTATEM", mustWork = TRUE)
  oise_layout_filepath <- system.file("extdata", "OISE_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)

  plate <- read_data(oise_filepath, layout_file_path = oise_layout_filepath, check_plate = FALSE, verbose=FALSE)
  expect_equal(length(plate$samples), 96)

})

test_that("incorrect format detection", { # add better handling of this error
  kenya_filepath <- system.file("extdata", "kenya_P3_shifted.csv", package = "PvSTATEM", mustWork = TRUE)

  expect_warning(
    expect_error(read_data(kenya_filepath, verbose=FALSE))
    )
})
