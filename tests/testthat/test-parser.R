test_that("Fully Parse CovidOISExPONTENT.csv plate data", {
  path <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(read_luminex_data(path, format = "xPONENT"))
})

test_that("Fully Parse CovidOISExPONTENT_CO.csv plate data with layout", {
  path <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM", mustWork = TRUE)
  layout_path <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
  expect_no_error(read_luminex_data(path, format = "xPONENT", layout_filepath = layout_path))
})
