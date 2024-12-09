#' @title
#' Process a file to generate normalized data and reports
#'
#' @description
#' Perform `process_plate` and `generate_plate_report` for a given plate file.
#' In more detail, this function reads the plate file and calls the `process_plate`
#' on the processed plate objects across all the normalization types including the raw MFI vales.
#' If the user has specifed the `generate_report` flag, it will also call the `generate_plate_report` function
#' generating the quality control report.
#'
#' @param plate_filepath (`character(1)`) The path to the plate file.
#' @param layout_filepath (`character(1)`) The path to the layout file.
#' @param output_dir (`character(1)`) The directory where the output files should be saved. The default is `"normalized_data"`.
#' @param generate_report (`logical(1)`) If `TRUE`, generate a quality control report. The default is `FALSE`.
#' @param normalization_types (`character()`) A vector of normalization types to use. The default is `c("RAU", "nMFI")`.
#' @param verbose (`logical(1)`) Print additional information. The default is `TRUE`.
#'

process_file <- function(
    plate_filepath, layout_filepath,
    output_dir = "normalized_data",
    generate_report = FALSE,
    normalization_types = c("RAU", "nMFI"),
    verbose = TRUE,
    ...) {
  plate <- read_luminex_data(plate_filepath, layout_filepath, ...) # read the data

  verbose_cat("Processing plate '", plate$plate_name, "'\n", verbose = verbose)

  for (normalization_type in normalization_types) {
    process_plate(
      plate,
      normalisation_type = normalization_type, output_dir = output_dir,
      include_raw_mfi = TRUE, adjust_blanks = TRUE, verbose = verbose
    )
  }

  if (generate_report) {
    generate_plate_report(plate, output_dir = output_dir, ...)
  }
}
