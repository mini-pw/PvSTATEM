#' Generate a report for a plate.
#'
#' This function generates a report for a plate. The report is generated using
#' the `plate_report_template.Rmd` template.
#'
#' @param plate A plate object.
#' @param use_model (`logical(1)`) A logical value indicating whether the model should be used in the report.
#' @param filename (`character(1)`) The name of the output file. If not provided, the filename will be created
#' based on the plate name with the suffix '_report.html'.
#' @param output_dir (`character(1)`) The directory where the report should be saved. Default is 'reports'.
#'
#'
#' @return A report.
#' @export
generate_plate_report <- function(plate, use_model = TRUE, filename = NULL, output_dir = "reports") {
  message("Generating report... This will take approximately 30 seconds.")
  output_file <- if (is.null(filename)) {
    paste0(plate$plate_name, "_report.html")
  } else {
    filename
  }
  rmarkdown::render(
    "R/plate_report_template.Rmd",
    params = list(plate = plate, use_model = use_model),
    output_file = output_file,
    output_dir = output_dir,
    quiet=TRUE
  )
  message(paste0("Report successfully generated, saving to: ", output_dir, "/", output_file))
}

