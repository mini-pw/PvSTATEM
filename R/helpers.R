#' Verify Numeric Join
#'
#' Checks if two numeric values are equal. If either value is `NA`, returns `TRUE`.
#'
#' @param x A numeric value to be compared.
#' @param y A numeric value to be compared.
#' @return `TRUE` if the values are equal or if either value is `NA`. Returns `FALSE` otherwise.
#' @keywords internal
verify_numeric_join <- function(x, y) {
  # check if two numeric values are equal
  if (is.na(x) || is.na(y)) {
    return(TRUE)
  }
  return(x == y)
}


#' Verify Character Join
#'
#' Checks if two character values are equal. If either value is `NULL`, returns `TRUE`.
#'
#' @param x A character value to be compared.
#' @param y A character value to be compared.
#' @return `TRUE` if the values are equal or if either value is `NULL`. Returns `FALSE` otherwise.
#' @keywords internal
verify_character_join <- function(x, y) {
  # check if two character values are equal
  if (is.null(x) || is.null(y)) {
    return(TRUE)
  }
  return(x == y)
}


#' Determine the Join Value
#'
#' Returns a non-`NA`/non-`NULL` value based on the inputs. If either value is `NA` or `NULL`, it returns the non-`NA`/non-`NULL` value. If both values are equal, it returns that value.
#'
#' @param x A value to be compared.
#' @param y A value to be compared.
#' @return A non-`NA`/non-`NULL` value or the common value if `x` equals `y`. Returns `NULL` if the values differ and neither is `NA` or `NULL`.
#' @keywords internal
get_join_value <- function(x, y) {
  if (is.na(x) || is.null(x)) {
    return(y)
  }
  if (is.na(y) || is.null(y)) {
    return(x)
  }

  if (x == y) {
    return(x)
  }
}

#' Remove Empty Lists from a List
#'
#' This internal function filters out elements from a list that are empty lists.
#'
#' @param lst A list to be processed.
#' @return A list with empty lists removed.
#' @keywords internal
remove_empty_lists <- function(lst) {
  # Filter out elements that are empty lists
  result <- lst[!sapply(lst, function(x) is.list(x) && length(x) == 0)]
  return(result)
}


#' Check if a string is a number
#'
#' @param x A string to be checked.
#' @return `TRUE` if the string is a number, `FALSE` otherwise.
#' @keywords internal
is.str.number <- function(x) {
  stopifnot(is.character(x))
  all(sapply(x, function(x) grepl("^[0-9]+$", x)))
}


#' Check if a value is a scalar
#'
#' This will return FALSE for NULL and vectors of length bigger than 2.
#'
#' @param x Object to be checked.
#' @return `TRUE` if the object is a scalar, `FALSE` otherwise.
#' @keywords internal
is.scalar <- function(x) {
  is.atomic(x) && length(x) == 1L
}

#' Verbose Cat
#'
#' This function prints the input to the console if the `verbose` argument is `TRUE`.
#'
#' @param ... The input to be printed.
#' @param verbose A logical value indicating whether the input should be printed.
#' @keywords internal
verbose_cat <- function(..., verbose = TRUE) {
  if (verbose) {
    cat(..., sep = "")
  }
}


#
# colors for WARNING, NOTE, DEFAULT
#
color_codes <- list(
  yellow_start = "\033[33m",
  yellow_end = "\033[39m",
  red_start = "\033[31m",
  red_end = "\033[39m",
  green_start = "\033[32m",
  green_end = "\033[39m"
)

#' Check if a value is an outlier
#'
#' @param x Vector of numeric values from which the outliers are to be detected.
#'
#' @return A logical vector indicating whether each value is an outlier.
#'
#' @importFrom stats IQR quantile
#' @keywords internal
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#' Clamp a value to a range
#'
#' @param x (`numeric()`) A numeric value to be clamped.
#' @param lower ('numeric(1)') The lower bound of the range.
#' @param upper (`numeric(1)`) The upper bound of the range.
#'
#' @return A numeric value clamped to the range \[`lower`, `upper`\].
#'
#' @keywords internal
clamp <- function(x, lower = -Inf, upper = Inf) {
  stopifnot(is.numeric(x), is.numeric(lower), is.numeric(upper))
  x[x < lower] <- lower
  x[x > upper] <- upper
  x
}
