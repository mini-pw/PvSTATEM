library(testthat)

test_that("Test verify numeric join", {
  expect_true(verify_numeric_join(1, 1))
  expect_true(verify_numeric_join(NA, 1))
  expect_true(verify_numeric_join(1, NA))
  expect_false(verify_numeric_join(1, 2))
})

test_that("Test verify character join", {
  expect_true(verify_character_join("a", "a"))
  expect_true(verify_character_join(NULL, "a"))
  expect_true(verify_character_join("a", NULL))
  expect_false(verify_character_join("a", "b"))
})

test_that("Test get join value", {
  expect_equal(get_join_value(1, 1), 1)
  expect_equal(get_join_value(NA, 2), 2)
  expect_equal(get_join_value("a", "b"), NULL)
})

test_that("Test remove empty lists", {
  expect_equal(remove_empty_lists(list(1, 2, list())), list(1, 2))
  expect_equal(remove_empty_lists(list(1, 2, list(1, 2))), list(1, 2, list(1, 2)))
})

test_that("Test is.str.number function", {
  expect_true(is.str.number("1"))
  expect_false(is.str.number("a"))
})

test_that("Test is.scalar", {
  expect_true(is.scalar(1))
  expect_true(is.scalar(NA))
  expect_false(is.scalar(NULL))
  expect_false(is.scalar(c(1, 2)))
})

test_that("Test verbose cat", {
  expect_output(verbose_cat("a", "b"), "ab")
  expect_null(verbose_cat("a", "b", verbose = FALSE))
})

test_that("Test clamp function", {
  expect_equal(clamp(c(1, 0, 2), lower = 1), c(1, 1, 2))
  expect_equal(clamp(c(1, -1, NA), lower = 0), c(1, 0, NA))
  expect_equal(clamp(c(1, 2.2, 3), upper = 2), c(1, 2, 2))
  expect_equal(clamp(c(2, 10, NA), upper = 2), c(2, 2, NA))
})

test_that("Test format dilution function standard case", {
  dilutions <- c("1/2", "1/3", "1/4")
  dilution_values <- c(0.5, 0.33, 0.25)
  sample_types <- c("STANDARD CURVE", "STANDARD CURVE", "STANDARD CURVE")

  expect_equal(format_dilutions(dilutions, dilution_values, sample_types), "1/2, 1/3, 1/4")
})

test_that("Test format dilution function with sample types", {
  dilutions <- c("1/2", "1/3", "1/4")
  dilution_values <- c(0.5, 0.33, 0.25)
  sample_types <- c("STANDARD CURVE", "STANDARD CURVE", "SAMPLE")

  expect_equal(format_dilutions(dilutions, dilution_values, sample_types), "1/2, 1/3")
})

test_that("Test format dilution function with multiple duplicates", {
  dilutions <- c("1/2", "1/3", "1/4", "1/4")
  dilution_values <- c(0.5, 0.33, 0.25, 0.25)
  sample_types <- c("STANDARD CURVE", "STANDARD CURVE", "STANDARD CURVE", "STANDARD CURVE")

  expect_equal(format_dilutions(dilutions, dilution_values, sample_types), "1/2, 1/3, 2x1/4")
})

test_that("Test format dilution function with shuffled dilutions", {
  dilutions <- c("1/4", "1/2", "1/3")
  dilution_values <- c(0.25, 0.5, 0.33)
  sample_types <- c("STANDARD CURVE", "STANDARD CURVE", "STANDARD CURVE")

  expect_equal(format_dilutions(dilutions, dilution_values, sample_types), "1/2, 1/3, 1/4")
})

test_that("Test format dilution function with dilutions equal null", {
  dilutions <- NULL
  dilution_values <- c(0.25, 0.5, 0.33)
  sample_types <- c("STANDARD CURVE", "STANDARD CURVE", "SAMPLE")

  expect_equal(format_dilutions(dilutions, dilution_values, sample_types), NULL)
})


test_that("Test is.decreasing function", {
  expect_true(is.decreasing(NULL))
  expect_true(is.decreasing(c()))
  expect_true(is.decreasing(c(2)))
  expect_true(is.decreasing(c(3, 2, 1)))
  expect_false(is.decreasing(c(1, 2, 3)))
  expect_false(is.decreasing(c(1, 2, 2)))
  expect_error(is.decreasing(c(1, 2, NA)))
  expect_error(is.decreasing("wrong"))
})

test_that("Test validate_filepath_and_output_dir function", {
  tmp_dir <- tempdir(check = TRUE)
  # base case
  expect_equal(
    validate_filepath_and_output_dir("test", tmp_dir, "plate_name", "report", "html"),
    file.path(tmp_dir, "test.html")
  )

  # extension handling
  expect_equal(
    validate_filepath_and_output_dir("test", tmp_dir, "plate_name", "report", "html.html"),
    file.path(tmp_dir, "test.html.html")
  )


  expect_equal(
    validate_filepath_and_output_dir("test.html", tmp_dir, "plate_name", "report", "html"),
    file.path(tmp_dir, "test.html")
  )

  # trailing full stop in extension
  expect_error(validate_filepath_and_output_dir("test", tmp_dir, "plate_name", "report", ".html"))

  # default filename creation
  expect_equal(
    validate_filepath_and_output_dir(NULL, tmp_dir, "plate_name", "report", "html"),
    file.path(tmp_dir, "plate_name_report.html")
  )


  # filename with no output_dir
  expect_warning(
    validate_filepath_and_output_dir(file.path(tmp_dir, "test.html"), tmp_dir, "plate_name", "report", "html")
  )

  expect_no_warning(
    validate_filepath_and_output_dir(file.path(tmp_dir, "test.html"), NULL, "plate_name", "report", "html")
  )

  file.create(file.path(tmp_dir, "test.html"))
  # overwrite existing file
  expect_warning(
    validate_filepath_and_output_dir(file.path(tmp_dir, "test.html"), tmp_dir, "plate_name", "report", "html")
  )

  # create output directory
  expect_no_error(
    validate_filepath_and_output_dir("test.html", file.path(tmp_dir, "output"), "plate_name", "report", "html")
  )

  expect_true(dir.exists(file.path(tmp_dir, "output")))
})
