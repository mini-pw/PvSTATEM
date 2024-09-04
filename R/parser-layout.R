#'
#'

read_location_data_csv <- function(location_file_path, separator = ",", ...) {
  location_data <- utils::read.csv(
    location_file_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = 1,
    sep = separator
  )
  stopifnot(all(rownames(location_data) %in% LETTERS))
  as.matrix(location_data)
}


read_location_data_xlsx <- function(location_file_path, ...) {
  location_data <- readxl::read_xlsx(location_file_path, .name_repair = "unique_quiet")
  location_matrix <- as.matrix(location_data)
  row_names <- location_matrix[, 1]
  stopifnot(all(row_names %in% LETTERS))
  location_matrix <- location_matrix[, -1]
  rownames(location_matrix) <- row_names
  location_matrix
}


#' Read layout data from a file
#'
#' @param layout_file_path Path to the layout file
#' @param ... Additional arguments to pass to the underlying read function
#'
#' @return A matrix with the layout data.
#' The row names are supposed to be letters A,B,C, etc.
#' The column names are supposed to be numbers 1,2,3, etc.
#'
#' @import readxl
#' @import utils
read_layout_data <- function(layout_file_path, ...) {
  if (!file.exists(layout_file_path)) stop("Layout file '", layout_file_path, "' not found")

  ext <- tools::file_ext(layout_file_path)
  stopifnot(ext %in% c("csv", "xlsx"))

  switch(ext,
    csv = read_location_data_csv(layout_file_path, ...),
    xlsx = read_location_data_xlsx(layout_file_path, ...)
  )
}
