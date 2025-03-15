#' @title
#' Process a File to Generate Normalised Data and Reports
#'
#' @description
#' This function reads a Luminex plate file by calling [read_luminex_data()] and then processes it by calling [process_plate()]. It optionally generates also a quality control report using [generate_plate_report()].
#' It reads the specified plate file, processes the plate object using all specified normalisation types (including raw MFI values), and saves the results.
#' If `generate_report = TRUE`, a quality control report is also generated.
#'
#' ## Workflow
#' 1. Read the plate file and layout file.
#' 2. Process the plate data to generate a specific output types (`nMFI`, `RAU`), a simple output (`MFI`), or a report (`report`).
#' 3. Save the processed data to CSV files in the specified `output_dir`. The files are named as `{plate_name}_{output_type}.csv`.
#' 4. Optionally, generate a quality control report. The report is saved as an HTML file in the `output_dir`, under the name `{plate_name}_report.html`.
#'
#' @param plate_filepath (`character(1)`) Path to the Luminex plate file.
#' @param layout_filepath (`character(1)`) Path to the corresponding layout file.
#' @param output_dir (`character(1)`, default = `'output_data'`)
#'   - Directory where the output files will be saved.
#'   - If it does not exist, it will be created.
#' @param format (`character(1)`, default = `'xPONENT'`)
#'   - Format of the Luminex data.
#'   - Available options: `'xPONENT'`, `'INTELLIFLEX'`.
#' @param generate_report (`logical(1)`, default = `FALSE`)
#'   - If `TRUE`, generates a quality control report using [generate_plate_report()].
#' @param process_plate (`logical(1)`, default = `TRUE`)
#'   - If `TRUE`, processes the plate data using [process_plate()].
#'   - If `FALSE`, only reads the plate file and returns the plate object without processing.
#' @param output_types (`character()`, default = `c("MFI", "RAU", "nMFI")`)
#'   - List of output types to generate
#'   - Supported values: `c("MFI", "RAU", "nMFI", "report")`.
#' @param blank_adjustment (`logical(1)`, default = `FALSE`)
#'   - If `TRUE`, performs blank adjustment before processing.
#' @param verbose (`logical(1)`, default = `TRUE`)
#'   - If `TRUE`, prints additional information during execution.
#' @param ... Additional arguments passed to [read_luminex_data()] and [generate_plate_report()].
#'
#' @return A [`Plate`] object containing the processed data.
#'
#' @examples
#' # Example 1: Process a plate file with default settings (all normalisation types)
#' plate_file <- system.file("extdata", "CovidOISExPONTENT_CO_reduced.csv", package = "SerolyzeR")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "SerolyzeR")
#' example_dir <- tempdir(check = TRUE)
#' process_file(plate_file, layout_file, output_dir = example_dir)
#'
#' # Example 2: Process the plate for only RAU normalisation
#' process_file(plate_file, layout_file, output_dir = example_dir, output_types = c("RAU"))
#'
#' # Example 3: Process the plate and generate a quality control report
#' process_file(plate_file, layout_file, output_dir = example_dir, generate_report = TRUE)
#'
#' @importFrom fs file_exists
#'
#' @export
process_file <- function(
    plate_filepath, layout_filepath,
    output_dir = "output_data",
    format = "xPONENT",
    process_plate = TRUE,
    output_types = c("MFI", "RAU", "nMFI"),
    blank_adjustment = FALSE,
    verbose = TRUE,
    ...) {
  if (is.null(plate_filepath)) {
    stop("Plate filepath is required.")
  }
  stopifnot(fs::file_exists(plate_filepath))
  plate_filepath <- fs::path_abs(plate_filepath)

  plate <- read_luminex_data(plate_filepath, layout_filepath, format = format)

  verbose_cat("Processing plate '", plate$plate_name, "'\n", verbose = verbose)

  if (process_plate) {
    for (output_type in output_types) {
      if (output_type == "report") {
        generate_plate_report(plate, output_dir = output_dir, ...)
      } else {
        process_plate(
          plate,
          output_type = output_type, output_dir = output_dir,
          blank_adjustment = blank_adjustment, verbose = verbose
        )
      }
    }
  }

  return(plate)
}
