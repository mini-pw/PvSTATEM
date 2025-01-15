#' @title
#' Process a file to generate normalised data and reports
#'
#' @description
#' Perform `process_plate` and `generate_plate_report` for a given plate file.
#' In more detail, this function reads the plate file and calls the `process_plate`
#' on the processed plate objects across all the normalisation types, including the raw MFI values.
#' If the user has specified the `generate_report` flag, it will also call the `generate_plate_report` function

#' generating the quality control report.
#'
#' @param plate_filepath (`character(1)`) The path to the plate file.
#' @param layout_filepath (`character(1)`) The path to the layout file.
#' @param output_dir (`character(1)`) The directory where the output files should be saved. The default is `"normalised_data"`.
#' @param format (`character(1)`) The format of the Luminex data. The default is `"xPONENT"`. Available options are `"xPONENT"` and `"INTELLIFLEX"`.
#' @param generate_report (`logical(1)`) If `TRUE`, generate a quality control report. The default is `FALSE`.
#' @param process_plate (`logical(1)`) If `TRUE`, process the plate. The default is `TRUE`.
#' If the value is set to `FALSE` the function will only read the plate file and return the plate object.
#' @param normalisation_types (`character()`) A vector of normalisation types to use. The default is `c("RAU", "nMFI")`.
#' @param verbose (`logical(1)`) Print additional information. The default is `TRUE`.
#' @param ... Additional arguments to for the `read_luminex_data` function.
#'
#' @examples
#'
#' # Select an input csv file for processing and corresponding layout file
#' plate_file <- system.file("extdata", "CovidOISExPONTENT_CO_reduced.csv", package = "PvSTATEM")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM")
#'
#' example_dir <- tempdir(check = TRUE) # a temporary directory
#' # create and save dataframe with computed dilutions for all suported noramlization types
#' process_file(plate_file, layout_file, output_dir = example_dir)
#'
#' example_dir2 <- tempdir(check = TRUE) # a temporary directory
#' # process the plate for a specific normalization type
#' process_file(plate_file, layout_file, output_dir = example_dir2, normalisation_types = c("RAU"))
#'
#' @importFrom fs file_exists
#'
#' @export
process_file <- function(
    plate_filepath, layout_filepath,
    output_dir = "normalised_data",
    format = "xPONENT",
    generate_report = FALSE,
    process_plate = TRUE,
    normalisation_types = c("RAU", "nMFI"),
    verbose = TRUE,
    ...) {
  if (is.null(plate_filepath)) {
    stop("Plate filepath is required.")
  }
  stopifnot(fs::file_exists(plate_filepath))

  plate <- read_luminex_data(plate_filepath, layout_filepath, format = format)

  verbose_cat("Processing plate '", plate$plate_name, "'\n", verbose = verbose)

  if (process_plate) {
    for (normalisation_type in normalisation_types) {
      process_plate(
        plate,
        normalisation_type = normalisation_type, output_dir = output_dir,
        include_raw_mfi = TRUE, adjust_blanks = TRUE, verbose = verbose
      )
    }
  }

  if (generate_report) {
    generate_plate_report(plate, output_dir = output_dir, ...)
  }

  return(plate)
}
