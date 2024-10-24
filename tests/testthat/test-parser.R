library(testthat)

test_that("Test handle_datetime with seen date formats", {
  # Default xPONENT datetime format MM/DD/YYYY HH:MM AM/PM
  expect_equal(
    handle_datetime("05/11/2022 4:45 PM", "xPONENT"),
    as.POSIXct("2022-05-11 16:45:00", tz = "")
  )

  # Automatic recovery with xPONENT datetime format DD/MM/YYYY HH:MM
  expect_message(dt <- handle_datetime("26/02/2014 16:07", "xPONENT"))
  expect_equal(dt, as.POSIXct("2014-02-26 16:07:00", tz = ""))

  # Default INTELLIFLEX datetime format YYYY-MM-DD HH:MM:SS AM/PM
  expect_equal(
    handle_datetime("2024-10-07 12:00:00 PM", "INTELLIFLEX"),
    as.POSIXct("2024-10-07 12:00:00", tz = "")
  )
})
