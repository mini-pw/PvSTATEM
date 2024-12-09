#' Generate a report for a plate.
#'
#' @description
#' This function generates a report for a plate. The report contains all the necessary information about the plate, from the general plate parameters, such as examination date, to the breakdown of the analytes' plots.
#' The report is generated using the `plate_report_template.Rmd` template.
#'
#' @param plate A plate object.
#' @param use_model (`logical(1)`) A logical value indicating whether the model should be used in the report.
#'
#' @param filename (`character(1)`) The name of the output HTML report file.
#' If not provided or equals to `NULL`, the output filename will be based on the plate name, precisely: `{plate_name}_report.html`.
#' By default the `plate_name` is the filename of the input file that contains the plate data.
#' For more details please refer to \link[PvSTATEM]{Plate}.
#'
#' If the passed filename does not contain `.html` extension, the default extension `.html` will be added.
#' Filename can also be a path to a file, e.g. `path/to/file.html`. In this case, the `output_dir` and `filename` will be joined together.
#' However, if the passed filepath is an absolute path and the `output_dir` parameter is also provided, the `output_dir` parameter will be ignored.
#' If a file already exists under a specified filepath, the function will overwrite it.
#'
#' @param output_dir (`character(1)`) The directory where the output CSV file should be saved.
#' Please note that any directory path provided will create all necessary directories (including parent directories) if they do not exist.
#' If it equals to `NULL` the current working directory will be used. Default is 'reports'.
#' @param counts_lower_threshold (`numeric(1)`) The lower threshold for the counts plots (works for each analyte). Default is 50.
#' @param counts_higher_threshold (`numeric(1)`) The higher threshold for the counts plots (works for each analyte). Default is 70.
#' @param additional_notes (`character(1)`) Additional notes to be included in the report. Contents of this fields are left to the user's discretion. If not provided, the field will not be included in the report.
#'
#'
#' @return A report.
#'
#' @import svglite
#'
#' @examples
#'
#' plate_file <- system.file("extdata", "CovidOISExPONTENT_CO_reduced.csv", package = "PvSTATEM")
#' # a plate file with reduced number of analytes to speed up the computation
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM")
#' note <- "This is a test report.\n**Author**: Jane Doe \n**Tester**: John Doe"
#'
#' plate <- read_luminex_data(plate_file, layout_file, verbose = FALSE)
#' example_dir <- tempdir(check = TRUE) # a temporary directory
#' generate_plate_report(plate,
#'   output_dir = example_dir,
#'   counts_lower_threshold = 40,
#'   counts_higher_threshold = 50,
#'   additional_notes = note
#' )
#' @export
generate_plate_report <-
  function(plate,
           use_model = TRUE,
           filename = NULL,
           output_dir = "reports",
           counts_lower_threshold = 50,
           counts_higher_threshold = 70,
           additional_notes = NULL) {
    message("Generating report...This will take approximately 30 seconds.")

    output_path <- validate_filepath_and_output_dir(filename, output_dir, plate$plate_name, "report", "html")

    output_dir <- dirname(output_path)
    filename <- basename(output_path)

    template_path <-
      system.file(
        "templates",
        "plate_report_template.Rmd",
        package = "PvSTATEM",
        mustWork = TRUE
      )

    # markdown does not support single line breaks, so we need to replace them with two spaces and a line break
    if (!is.null(additional_notes)) {
      additional_notes <-
        gsub(
          pattern = "\n",
          replacement = "  \n",
          x = additional_notes
        )
    }

    rmarkdown::render(
      template_path,
      params = list(
        plate = plate,
        use_model = use_model,
        counts_lower_threshold = counts_lower_threshold,
        counts_higher_threshold = counts_higher_threshold,
        additional_notes = additional_notes
      ),
      output_file = filename,
      output_dir = output_dir,
      knit_root_dir = output_dir,
      intermediates_dir = output_dir,
      quiet = TRUE
    )

    message(paste0(
      "Report successfully generated, saving to: ",
      output_dir
    ))
  }


#' Generate a report with Levey-Jennings plots.
#'
#' This function generates a report with Levey-Jennings plots.
#' The report is generated using the `levey_jennings_report_template.Rmd` template.
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
#' @keywords internal
generate_levey_jennings_report <-
  function(list_of_plates,
           filename = NULL,
           output_dir = "reports") {
    message("Generating report... This will take approximately 30 seconds.")

    plate <- list_of_plates[[1]]
    output_path <- validate_filepath_and_output_dir(filename, output_dir, plate$plate_name, "levey_jennings", "html")

    filename <- basename(output_path)
    output_dir <- dirname(output_path)


    template_path <-
      system.file(
        "templates",
        "levey_jennings_report_template.Rmd",
        package = "PvSTATEM",
        mustWork = TRUE
      )

    rmarkdown::render(
      template_path,
      params = list(list_of_plates = list_of_plates),
      output_file = filename,
      output_dir = output_dir,
      quiet = TRUE
    )
    message(paste0(
      "Report successfully generated, saving to: ",
      output_path
    ))
  }
