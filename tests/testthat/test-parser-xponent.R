library(testthat)

## Unit tests

### Tests for is blank line

test_that("Test checking if a line is blank", {
  expect_true(is_line_blank(NA))
  expect_true(is_line_blank(""))
  expect_true(is_line_blank(",,"))
  expect_true(is_line_blank('""'))
  expect_false(is_line_blank("text"))
})


### Test for vectorize_csv_line

test_that("Test vectorizing a CSV line", {
  expect_equal(vectorize_csv_line("key,value", ","), c("key", "value"))
  expect_equal(vectorize_csv_line("key,value,", ","), c("key", "value"))
})


### Tests for is_the_end_of_csv_section

test_that("Test checking if the line is the end of a CSV section", {
  expect_false(is_the_end_of_csv_section("key,value", ","))
  expect_true(is_the_end_of_csv_section("", ","))
  expect_true(is_the_end_of_csv_section(",,", ","))
  expect_true(is_the_end_of_csv_section("DataType:", ","))
  expect_true(is_the_end_of_csv_section("Samples:", ","))
  expect_true(is_the_end_of_csv_section("-- CRC --", ","))
})


### Tests for skip_blanks_parser

test_that("Test skipping lines containing no information", {
  lines <- c("", ",,,", '""', "text")
  out <- skip_blanks_parser(1, lines)
  expect_equal(out[[2]], 4)
})


### Tests for check_key_parser

test_that("Test asserting the content of a line and skipping that line", {
  lines <- c("Samples:", "Sample1")
  out <- check_key_parser("Samples:", ",")(1, lines)
  expect_equal(out[[2]], 2)

  lines <- c("Batch1", "Samples")
  out <- check_key_parser("Batch\\d+", ",")(1, lines)
  expect_equal(out[[2]], 2)
})

test_that("Test failure in asserting the content of a line", {
  lines <- c("Samples:,test", "Sample1")
  expect_error(check_key_parser("Sample:", ",")(1, lines))
})


### Test for key_value_parser

test_that("Test parsing key-value pairs", {
  lines <- c("key,value", "key2,value2,")
  parser <- key_value_parser("key\\d*", ",")

  out1 <- parser(1, lines)
  expect_equal(out1[[1]], list(key = "value"))
  expect_equal(out1[[2]], 2)

  out2 <- parser(2, lines)
  expect_equal(out2[[1]], list(key2 = "value2"))
  expect_equal(out2[[2]], 3)
})


### Tests for named_key_value_pairs_parser

test_that("Test parsing line with multiple key-value pairs with a name in the front", {
  lines <- c("KVs,key1,value1,key2,value2,", "")
  parser <- named_key_value_pairs_parser("KVs", ",")

  out1 <- parser(1, lines)
  expect_equal(out1[[1]], list(KVs = list(key1 = "value1", key2 = "value2")))
  expect_equal(out1[[2]], 2)
})


### Tests for key_value_pairs_parser

test_that("Test parsing multiple key-value pairs", {
  lines <- c("key1,value1,key2,value2,", "")
  parser <- key_value_pairs_parser(",")

  out1 <- parser(1, lines)
  expect_equal(out1[[1]], list(key1 = "value1", key2 = "value2"))
  expect_equal(out1[[2]], 2)
})


### Tests for csv_table_parser

test_that("Test parsing a CSV section (Simple)", {
  lines <- c("h1,h2", "r1,r2,", "")
  out <- csv_table_parser("test", ",", remove_na_rows = TRUE)(1, lines)
  test_df <- out[[1]]$test
  expect_true(!is.null(test_df))
  expect_length(test_df, 1)
  expect_equal(out[[2]], 3)
})

test_that("Test parsing a CSV section (with row limit)", {
  lines <- c("h1,h2", "r1,r2,", "h3,h4", "r3,r4")
  out <- csv_table_parser("test", ",", max_rows = 2)(1, lines)
  test_df <- out[[1]]$test
  expect_true(!is.null(test_df))
  expect_length(test_df, 1)
})


### Tests for join_parsers

test_that("Test joining multiple parsers", {
  lines <- c("key1,value", "", "key2;value2;", "")
  parser1 <- key_value_parser("key1", ",")
  parser2 <- key_value_parser("key2", ";")
  parser <- join_parsers(parser1, skip_blanks_parser, parser2)
  out <- parser(1, lines)
  expect_equal(out[[1]], list(key1 = "value", key2 = "value2"))
})


### Tests for make_optional

test_that("Test making a parser optional", {
  lines <- c("key,value", "key2,value2")
  parser <- make_optional(key_value_parser("opt", ","))
  out <- parser(1, lines)
  expect_equal(out[[1]], NULL)
})


### Tests for match_any_parser

test_that("Test matching any of the parsers", {
  lines <- c("key,value", "key2,value2")
  parser1 <- key_value_parser("key1", ",")
  parser2 <- key_value_parser("key", ",")
  parser <- match_any_parser(parser1, parser2)
  out <- parser(1, lines)
  expect_equal(out[[1]], list(key = "value"))
  expect_equal(out[[2]], 2)
})


### Tests for repeat_parser

test_that("Test repeating a parser", {
  lines <- c("key1,value2", "key2,value2,", "key3,value3,,,,,")
  parser <- repeat_parser(key_value_parser("key\\d+", ","))
  out <- parser(1, lines)
  expect_equal(out[[1]], list(key1 = "value2", key2 = "value2", key3 = "value3"))
})


## Functionality tests

test_that("Parse the random plate data", {
  plate_file <- system.file("extdata", "random.csv", package = "PvSTATEM", mustWork = TRUE)
  expect_error(read_xponent_format(plate_file), NA)
})

test_that("Parse the random plate 2 data", {
  plate_file <- system.file("extdata", "random2.csv", package = "PvSTATEM", mustWork = TRUE)
  expect_error(read_xponent_format(plate_file), NA)
})

file_parse_success <- function(file) {
  print(paste0("Parsing file", file))
  expect_error_free <- TRUE
  tryCatch(
    {
      read_xponent_format(file)
    },
    error = function(e) {
      print(sprintf("Error on file: %s\n", file))
      print(e)
      expect_error_free <<- FALSE
    }
  )
  if (expect_error_free) {
    print("Parsing successful")
  }
  expect_error_free
}

test_that("Parse external plates", {
  external_data_dir <- system.file("extdata", "external", package = "PvSTATEM", mustWork = TRUE)
  for (file in list.files(external_data_dir, full.names = TRUE)) {
    expect_true(file_parse_success(file))
  }
})
