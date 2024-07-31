read_layout_data <- function(layout_file_path,
                             results_plate,
                             check_plate = TRUE,
                             replace_names = TRUE,
                             ...,
                             verbose = TRUE) {
  # function modifies the results_plate object by adding the location information from the layout file

  ext <- tools::file_ext(layout_file_path)

  stopifnot(ext %in% c("csv", "xlsx"))

  location_data <- switch(ext,
    csv = read_location_data_csv(layout_file_path),
    xlsx = read_location_data_xlsx(layout_file_path)
  )

  for (sample in results_plate$samples) {
    row <- sample$sample_location$row
    col <- sample$sample_location$col
    # first col should contain the row letter
    sample_name_loc <- location_data[[row, col + 1]]
    sample_name <- sample$sample_name
    sample$sample_type <-
      SampleType$parse_sample_type(sample_name, sample_name_loc = sample_name_loc)
    if (replace_names) {
      sample$sample_name <- sample_name_loc
    }
  }

  return(results_plate)
}
