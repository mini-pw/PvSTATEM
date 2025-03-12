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
    if (fs::file_exists(layout_filepath)) {
      return(layout_filepath)
    }

    stop(
      paste0("The specified common layout file ", layout_filepath, " does not exist.")
    )
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
    warning(
      paste0("Layout file for a file ", plate_filepath, " could not be found. Won't use the layout file for the given plate.")
    )
    return(NULL)
  }

  possible_layout_filename <- possible_files[1]
  possible_layout_path <- fs::path_join(c(file_dir, possible_layout_filename))
  if (fs::file_exists(possible_layout_path)) {
    return(possible_layout_path)
  } else {
    warning(
      paste0("Layout file for a file ", plate_filepath, " could not be found. Won't use the layout file for the given plate.")
    )
    return(NULL)
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
  format_pattern <- SerolyzeR.env$mba_pattern
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

  if (grepl(SerolyzeR.env$xponent_pattern, basename, ignore.case = TRUE)) {
    return("xPONENT")
  } else if (grepl(SerolyzeR.env$intelliflex_pattern, basename, ignore.case = TRUE)) {
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
#' Process a Directory of Luminex Data Files
#'
#' @description
#' This function processes all Luminex plate files within a specified directory.
#' Each plate file is processed using [process_file()], and the resulting normalised data is saved.
#' Optionally, quality control reports can be generated, and results from multiple plates can be merged into a single file.
#'
#' ## Workflow
#' 1. Identify all Luminex plate files in the `input_dir`, applying recursive search if `recurse = TRUE`.
#' 2. Detect the format of each file based on the `format` parameter or the filename.
#' 3. Locate the corresponding layout file using the filename or use the common layout passed with the `layout_filepath` parameter.
#' 4. Determine the appropriate output directory using [get_output_dir()].
#' 5. Process each plate file using [process_file()].
#' 6. If `merge_outputs = TRUE`, merge normalised data from multiple plates into a single CSV file.
#'
#' ## Naming Conventions for Input Files
#' - **If `format` is specified:**
#'   - Each plate file should be named as `{plate_name}.csv`.
#'   - The corresponding layout file should be named as `{plate_name}_layout.csv` or `{plate_name}_layout.xlsx`.
#'   - Alternatively, if `layout_filepath` is provided, it serves as a unified layout file for all plates.
#'
#' - **If `format` is not specified (automatic detection):**
#'   - Each plate file should be named as `{plate_name}_{format}.csv`, where `{format}` is either `xPONENT` or `INTELLIFLEX`.
#'   - The corresponding layout file should be named using the same convention as above.
#'
#' ## Output File Structure
#' - The `output_dir` parameter specifies where the processed files are saved.
#' - If `output_dir` is `NULL`, output files are saved in the same directory as the input files.
#' - By default, the output directory structure follows the input directory, unless `flatten_output_dir = TRUE`, which saves all outputs directly into `output_dir`.
#' - Output filenames follow the convention used in [process_file()].
#'   - For a plate named `{plate_name}`, the normalised output files are named as:
#'     - `{plate_name}_RAU.csv` for RAU normalisation.
#'     - `{plate_name}_nMFI.csv` for nMFI normalisation.
#'     - `{plate_name}_MFI.csv` for MFI normalisation.
#'     - If `generate_reports = TRUE`, a quality control report is saved as `{plate_name}_report.pdf`.
#'   - If `merge_outputs = TRUE`, merged normalised files are named as:
#'     - `merged_RAU_{timestamp}.csv`
#'     - `merged_nMFI_{timestamp}.csv`
#'     - `merged_MFI_{timestamp}.csv`
#'
#' @param input_dir (`character(1)`) Path to the directory containing plate files. Can contain subdirectories if `recurse = TRUE`.
#' @param output_dir (`character(1)`, optional) Path to the directory where output files will be saved. Defaults to `NULL` (same as input directory).
#' @param recurse (`logical(1)`, default = `FALSE`)
#'   - If `TRUE`, searches for plate files in subdirectories as well.
#' @param flatten_output_dir (`logical(1)`, default = `FALSE`)
#'   - If `TRUE`, saves output files directly in `output_dir`, ignoring the input directory structure.
#' @param format (`character(1)`, optional) Luminex data format. If `NULL`, it is automatically detected. Options: `'xPONENT'`, `'INTELLIFLEX'`.
#' @param layout_filepath (`character(1)`, optional) Path to a layout file. If `NULL`, the function attempts to detect it automatically.
#' @param normalisation_types (`character()`, default = `c("MFI", "RAU", "nMFI")`)
#'   - The normalisation types to apply. Supported values: `"MFI"`, `"RAU"`, `"nMFI"`.
#' @param generate_reports (`logical(1)`, default = `FALSE`)
#'   - If `TRUE`, generates quality control reports for each processed plate file.
#' @param merge_outputs (`logical(1)`, default = `FALSE`)
#'   - If `TRUE`, merges all normalised data into a single CSV file per normalisation type.
#'   - The merged file is named `merged_{normalisation_type}_{timestamp}.csv`.
#' @param column_collision_strategy (`character(1)`, default = `'intersection'`)
#'   - Determines how to handle missing or extra columns when merging outputs.
#'   - Options: `'union'` (include all columns), `'intersection'` (include only common columns).
#' @param return_plates (`logical(1)`, default = `FALSE`)
#'   - If `TRUE`, returns a list of processed plates sorted by experiment date.
#' @param dry_run (`logical(1)`, default = `FALSE`)
#'   - If `TRUE`, prints file details without processing them.
#' @param verbose (`logical(1)`, default = `TRUE`)
#'   - If `TRUE`, prints detailed processing information.
#' @param ... Additional arguments passed to [process_file()].
#'
#' @return If `return_plates = TRUE`, returns a sorted list of [`Plate`] objects. Otherwise, returns `NULL`.
#'
#' @examples
#' # Process all plate files in a directory
#' input_dir <- system.file("extdata", "multiplate_lite", package = "SerolyzeR", mustWork = TRUE)
#' output_dir <- tempdir(check = TRUE)
#' plates <- process_dir(input_dir, return_plates = TRUE, output_dir = output_dir)
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
    normalisation_types = c("MFI", "RAU", "nMFI"),
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
  input_dir <- fs::path_abs(input_dir)

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
          blank_adjustment = TRUE, verbose = verbose
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
