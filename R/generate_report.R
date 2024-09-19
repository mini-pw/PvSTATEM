#' Generate a report for a plate.
#'
#' This function generates a report for a plate. The report is generated using
#' the `html_report_template.Rmd` template.
#'
#' @param plate A plate object.
#' @param use_model A logical value indicating whether the model should be used in the report.
#' @return A report.
#' @export
generate_report <- function(plate, use_model = TRUE) {
  message("Generating report... This will take approximately 30 seconds.")
  rmarkdown::render("R/html_report_template.Rmd", params = list(plate = plate, use_model = use_model))
}