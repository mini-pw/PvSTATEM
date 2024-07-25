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
#' @examples
#' my_list <- list(a = 1, b = list(), c = 3, d = list(e = 4, f = list()))
#' remove_empty_lists(my_list)
#' # Returns: $a: 1, $c: 3, $d: list(e = 4, f = list())
remove_empty_lists <- function(lst) {
  # Filter out elements that are empty lists
  result <- lst[!sapply(lst, function(x) is.list(x) && length(x) == 0)]
  return(result)
}