#' Generate a report for a plate.
#'
#' This function generates a report for a plate. The report is generated using
#' the `plate_report_template.Rmd` template.
#'
#' @param plate A plate object.
#' @param use_model (`logical(1)`) A logical value indicating whether the model should be used in the report.
#' @param filename (`character(1)`) The name of the output file. If not provided,the filename will be created based on the plate name with the suffix '_report.html'.
#' @param output_dir (`character(1)`) The directory where the report should be saved. Default is 'reports'.
#' @param counts_lower_threshold (`numeric(1)`) The lower threshold for the counts plots (works for each analyte). Default is 50.
#' @param counts_higher_threshold (`numeric(1)`) The higher threshold for the counts plots (works for each analyte). Default is 70.
#'
#'
#' @return A report.
#' @export
generate_plate_report <- function(plate, use_model = TRUE, filename = NULL, output_dir = "reports", counts_lower_threshold = 50, counts_higher_threshold = 70) {
  message("Generating report... This will take approximately 30 seconds.")
  output_file <- if (is.null(filename)) {
    paste0(plate$plate_name, "_report.html")
  } else {
    filename
  }


  template_path <- system.file("R", "plate_report_template.Rmd", package = "PvSTATEM")
  # Check if the file exists before attempting to render
  if (!file.exists(template_path)) {
    stop(paste("The template file does not exist at:", template_path))
  }

  rmarkdown::render(
    template_path,
    params = list(plate = plate, use_model = use_model, counts_lower_threshold = counts_lower_threshold, counts_higher_threshold = counts_higher_threshold),
    output_file = output_file,
    output_dir = output_dir,
    quiet = TRUE
  )
  message(paste0("Report successfully generated, saving to: ", output_dir, "/", output_file))
}


#' Generate a report with Levy-Jennings plots.
#'
#' This function generates a report with Levy-Jennings plots.
#' The report is generated using the `levy_jennings_report_template.Rmd` template.
#'
#' @param list_of_plates A list of plate objects.
#' @param filename (`character(1)`) The name of the output file. If not
#' provided, the filename will be created based on the plate name
#' with the suffix '_report.html'.
#' @param output_dir (`character(1)`) The directory where the report
#' should be saved. Default is 'reports'.
#'
#'
#' @return A report.
#' @export
generate_levy_jennings_report <- function(list_of_plates, filename = NULL, output_dir = "reports") {
  message("Generating report... This will take approximately 30 seconds.")
  output_file <- if (is.null(filename)) {
    paste0("levy_jennings_report.html") #### change this part
  } else {
    filename
  }

  template_path <- system.file("R", "levy_jennings_report_template.Rmd", package = "PvSTATEM")
  # Check if the file exists before attempting to render
  if (!file.exists(template_path)) {
    stop(paste("The template file does not exist at:", template_path))
  }

  rmarkdown::render(
    template_path,
    params = list(list_of_plates = list_of_plates),
    output_file = output_file,
    output_dir = output_dir,
    quiet = TRUE
  )
  message(paste0("Report successfully generated, saving to: ", output_dir, "/", output_file))
}
