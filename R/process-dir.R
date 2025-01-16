#' @title
#' Find a layout file given plate filepath
#'
#' @import fs
#' @importFrom stringr str_split
#'
#' @keywords internal
#'
find_layout_file <- function(plate_filepath, layout_filepath = NULL) {
  if (!is.null(layout_filepath)) {
    stopifnot(fs::file_exists(layout_filepath))
    return(layout_filepath)
  }

  stopifnot(fs::is_absolute_path(plate_filepath))

  file_dir <- fs::path_dir(plate_filepath)
  filename <- fs::path_file(plate_filepath)
  filename_splitted <- stringr::str_split(filename, "\\.")
  filename_basename <- filename_splitted[[1]][1]

  supported_layout_exts <- c("xlsx", "csv")
  layout_file_glob <- paste0(
    filename_basename, "_layout",
    "\\.(", paste(supported_layout_exts, collapse = "|"), ")$"
  )
  possible_files <- list.files(file_dir, pattern = layout_file_glob)
  if (length(possible_files) == 0) {
    stop(
      paste0("Layout file for a file ", plate_filepath, " could not be found.")
    )
  }

  possible_layout_filename <- possible_files[1]
  possible_layout_path <- fs::path_join(c(file_dir, possible_layout_filename))
  if (fs::file_exists(possible_layout_path)) {
    return(possible_layout_path)
  } else {
    stop(
      paste0("Layout file for a file ", plate_filepath, " could not be found.")
    )
  }
}

#' @title
#' Identify if a file is a MBA data file
#'
#' @param filepath (`character(1)`) The path to the file.
#' @param check_format (`logical(1)`) If `TRUE`, the function will check if the file name contains a supported format. The default is `TRUE`.
#'
#' @return `TRUE` if the file is a MBA data file, `FALSE` otherwise.
#'
#' @import fs
#' @importFrom stringr str_split
#'
#' @keywords internal
#'
is_mba_data_file <- function(filepath, check_format = TRUE) {
  format_pattern <- PvSTATEM.env$mba_pattern
  extension_pattern <- "\\.([xX][lL][sS][xX]|[cC][sS][vV])$"
  output_pattern <- "RAU|nMFI"
  layout_pattern <- "_layout"

  stopifnot(fs::file_exists(filepath))
  filename <- fs::path_file(filepath)
  filename_splitted <- stringr::str_split(filename, "\\.")
  basename <- filename_splitted[[1]][1]

  # plate filename has to contain supported format
  if (check_format) {
    if (!grepl(format_pattern, filename, ignore.case = TRUE)) {
      return(FALSE)
    }
  }

  # plate filename extensions have to be supported
  if (!grepl(extension_pattern, filename)) {
    return(FALSE)
  }

  # plate filename has not to contain layout pattern
  if (grepl(layout_pattern, basename, fixed = TRUE)) {
    return(FALSE)
  }

  # plate filename has not to contain supported output format
  # as not to mix it up with output files
  if (grepl(output_pattern, basename, ignore.case = TRUE)) {
    return(FALSE)
  }

  return(TRUE)
}

#' @title
#' Try to detect the format of a file
#'
#' @import fs
#' @importFrom stringr str_split
#'
#' @keywords internal
#'
detect_mba_format <- function(filepath, format = NULL) {
  if (!is.null(format)) {
    stopifnot(is_mba_format(format, allow_nullable = FALSE))
    return(format)
  }

  stopifnot(fs::file_exists(filepath))
  filename <- fs::path_file(filepath)
  filename_splitted <- stringr::str_split(filename, "\\.")
  basename <- filename_splitted[[1]][1]

  if (grepl(PvSTATEM.env$xponent_pattern, basename, ignore.case = TRUE)) {
    return("xPONENT")
  } else if (grepl(PvSTATEM.env$intelliflex_pattern, basename, ignore.case = TRUE)) {
    return("INTELLIFLEX")
  } else {
    stop("The format of the file could not be detected.")
  }
}

#' @title
#' Get output directory for a given input file
#'
#' @import fs
#'
#' @keywords internal
#'
get_output_dir <- function(
    input_file,
    input_dir,
    output_dir = NULL,
    flatten_output_dir = FALSE) {
  output_root <- ifelse(is.null(output_dir), input_dir, output_dir)
  if (!fs::dir_exists(output_root)) {
    stop("Output directory does not exist.")
  }
  if (flatten_output_dir) {
    current_output_dir <- output_root
  } else {
    input_file_rel_path <- fs::path_rel(input_file, input_dir)
    current_output_dir <- fs::path_dir(
      fs::path_join(c(output_root, input_file_rel_path))
    )
  }
  return(fs::path(current_output_dir))
}

#' @title
#' Process a dir of files to generate normalised data and reports
#'
#' @description
#' The output files will be created alongside their corresponding input files, preserving
#' the directory structure of the input directory unless the `flatten_output_dir` parameter is set to `TRUE`.
#'
#' @param input_dir (`character(1)`) The directory containing the input files. It may be nested.
#' @param output_dir (`character(1)`) Optional overwrite directory where the output files should be saved. The default is `NULL`.
#' By default, the output directory is the same as the input directory.
#' @param recurse (`logical(1)`) If `TRUE`, the function will search for files recursively in the input directory. The default is `FALSE`.
#' @param flatten_output_dir (`logical(1)`) If `TRUE`, the output files will be saved in the output directory directly. The default is `FALSE`.
#' @param format (`character(1)`) The format of the Luminex data. The default is `NULL`, and the format will have to
#' be determined automatically based on the file name. Available options are `xPONENT` and `INTELLIFLEX`.
#' @param layout_filepath (`character(1)`) The path to the layout file. The default is `NULL`, and the layout file will have to
#' be determined automatically based on the file name.
#' @param normalisation_types (`character()`) A vector of normalisation types to use. The default is `c("RAU", "nMFI")`.
#' @param generate_reports (`logical(1)`) If `TRUE`, generate quality control reports for each file. The default is `FALSE`.
#' @param merge_outputs (`logical(1)`) If `TRUE`, merge the outputs of all plates into a single CSV file for each normalisation type.
#' The resulting file will be saved in the output directory with the name `merged_{normalisation_type}_{timestamp}.csv`.
#' Example: `merged_nMFI_20250115_230735.csv`.
#' @param column_collision_strategy (`character(1)`) A method for handling missing or additional columns when merging outputs.
#' Possible options are `union` and `intersection`. The default is `intersection`.
#' @param return_plates (`logical(1)`) If `TRUE`, return a list of processed plates. The default is `FALSE`.
#' @param dry_run (`logical(1)`) If `TRUE`, the function will not process any files
#' but will print the information about the files that would be processed. The default is `FALSE`.
#' @param verbose (`logical(1)`) Print additional information. The default is `TRUE`.
#' @param ... Additional arguments to for the `process_file` function.
#'
#' @return If the `return_plates` parameter is set to `TRUE` the function returns a list of plates
#' sorted by the `plate_datetime` (The time of the experiment noted in the csv file) in increasing order (oldest plates first).
#' If the `return_plates` parameters is set to `FALSE` the function returns `NULL`.
#'
#' @examples
#' # Select input directory to process
#' dir <- system.file("extdata", "multiplate_lite", package = "PvSTATEM", mustWork = TRUE)
#'
#' # Select output directory
#' output_dir <- tempdir(check = TRUE)
#'
#' # Process input directory and return plates
#' plates <- process_dir(dir, return_plates = TRUE, output_dir = output_dir)
#'
#' @import fs
#'
#' @export
process_dir <- function(
    input_dir,
    output_dir = NULL,
    recurse = FALSE,
    flatten_output_dir = FALSE,
    layout_filepath = NULL,
    format = NULL,
    normalisation_types = c("RAU", "nMFI"),
    generate_reports = FALSE,
    merge_outputs = FALSE,
    column_collision_strategy = "intersection",
    return_plates = FALSE,
    dry_run = FALSE,
    verbose = TRUE,
    ...) {
  stopifnot(fs::dir_exists(input_dir))
  stopifnot(is.null(output_dir) || fs::dir_exists(output_dir))
  stopifnot(is.null(layout_filepath) || fs::file_exists(layout_filepath))
  stopifnot(is_mba_format(format, allow_nullable = TRUE))

  input_files <- c()
  for (input_file in fs::dir_ls(input_dir, recurse = recurse)) {
    if (is_mba_data_file(input_file, check_format = is.null(format))) {
      input_files <- c(input_files, input_file)
    }
  }

  if (dry_run) {
    cat("Dry run mode enabled.\n")
    cat("Input directory: ", input_dir, "\n")
    if (!is.null(format)) {
      cat("MBA format: static (", format, ") \n")
    } else {
      cat("MBA format: dynamic \n")
    }
    if (!is.null(layout_filepath)) {
      cat("Layout file: static (", layout_filepath, ") \n")
    } else {
      cat("Layout file: dynamic \n")
    }
  }

  if (length(input_files) == 0) {
    cat("No files found in the input directory.\n")
    cat("Check if files inside the input directory are named correctly. ")
    cat("If files are not named according to the convention, ")
    cat("one should provide a global MBA format and layout file.\n")
    return(NULL)
  }

  formats <- rep(NA, length(input_files))
  for (i in seq_along(input_files)) {
    formats[i] <- detect_mba_format(input_files[i], format = format)
  }
  stopifnot(all(!is.na(formats)))

  layouts <- rep(NA, length(input_files))
  for (i in seq_along(input_files)) {
    layouts[i] <- find_layout_file(
      input_files[i],
      layout_filepath = layout_filepath
    )
  }
  stopifnot(all(!is.na(layouts)))

  if (dry_run) {
    cat("The following files will be processed:\n")
    for (i in seq_along(input_files)) {
      current_output_dir <- get_output_dir(input_files[i], input_dir,
        output_dir = output_dir, flatten_output_dir = flatten_output_dir
      )
      cat(
        "\n",
        "File: ", input_files[i], "\n",
        "Layout: ", layouts[i], "\n",
        "Format: ", formats[i], "\n",
        "Output:", current_output_dir, "\n"
      )
    }
    return(NULL)
  }

  plates <- list()
  for (i in seq_along(input_files)) {
    current_output_dir <- get_output_dir(input_files[i], input_dir,
      output_dir = output_dir, flatten_output_dir = flatten_output_dir
    )
    plate <- process_file(
      input_files[i],
      layout_filepath = ifelse(is.na(layouts[i]), NULL, layouts[i]),
      output_dir = current_output_dir,
      format = formats[i],
      process_plate = !merge_outputs,
      normalisation_types = normalisation_types,
      generate_report = generate_reports,
      verbose = verbose,
      ...
    )

    plates[[plate$plate_name]] <- plate
  }

  plates <- sort_list_by(
    plates,
    value_f = function(p) p$plate_datetime,
    decreasing = FALSE
  )

  file_ending <- format(now(), "%Y%m%d_%H%M%S")
  if (merge_outputs) {
    for (normalisation_type in normalisation_types) {
      dataframes <- list()
      for (plate in plates) {
        output_df <- process_plate(plate,
          normalisation_type = normalisation_type, write_output = FALSE,
          include_raw_mfi = TRUE, adjust_blanks = TRUE, verbose = verbose
        )
        df_header_columns <- data.frame(
          plate_name = plate$plate_name,
          sample_name = rownames(output_df)
        )
        rownames(output_df) <- NULL
        modifed_output_df <- cbind(df_header_columns, output_df)
        dataframes[[plate$plate_name]] <- modifed_output_df
      }

      main_output_df <- merge_dataframes(
        dataframes,
        column_collision_strategy = column_collision_strategy,
        fill_value = NA
      )

      file_name <- paste0(
        "merged_", normalisation_type, "_", file_ending, ".csv"
      )
      output_path <- fs::path_join(c(output_dir, file_name))
      write.csv(main_output_df, output_path, row.names = FALSE)
      verbose_cat("Merged output saved to: ", output_path, "\n", verbose = verbose)
    }
  }

  if (return_plates) {
    return(plates)
  }
}
