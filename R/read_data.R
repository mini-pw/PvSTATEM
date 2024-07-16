#' Read data stored in the csv file - output of examination
#'
#' file structure based on the https://genome.med.harvard.edu/documents/luminex/IS2.3_SW_110_eng_us.pdf page 97
#' and Kenya screening csv files
#'
#'
#' @param file_path path to the Luminex csv file
#' @param layout_file_path path to the layout file
#' @param check_plate if TRUE veryfies the plate - checks the consistency etc.
#' @param verbose if TRUE, print out the progress of the function
#' @param colorize if TRUE, colorize the output
#' @param ... additional arguments passed down
#'
#' @examples
#' plate_file <- system.file("extdata", "random.csv", package = "PvSTATEM")
#' plate <- read_data(plate_file)
#'
#' @export
read_data <- function(file_path,
                      layout_file_path = NULL,
                      check_plate = TRUE,
                      verbose = TRUE,
                      colorize = !isTRUE(getOption("knitr.in.progress")),
                      ...) {
  # firstly we read the raw csv file as a dataframe
  # data <- read.csv(filepath, header = FALSE, sep = ",", stringsAsFactors = FALSE)

  if (!colorize) {
    color_codes <- list(
      yellow_start = "",
      yellow_end = "",
      red_start = "",
      red_end = "",
      green_start = "",
      green_end = ""
    )
  }

  verbose_cat(
    color_codes$green_start,
    "Reading MBA plate csv file...\n\n",
    color_codes$green_end,
    verbose = verbose
  )

  blocks <- extract_blocks(file_path)

  # divide the header and the results sections
  divided_blocks <- divide_blocks(blocks)
  header_blocks <- divided_blocks$header_blocks
  results_blocks <- divided_blocks$results_blocks


  results_plate <-
    parse_results_blocks(results_blocks, verbose = verbose)

  results_plate <-
    parse_header_blocks(results_plate, header_blocks, check_consistency = check_plate, verbose = verbose)

  # merging the layout file
  if (!is.null(layout_file_path)) {
    verbose_cat(
      "", color_codes$green_start,
      "reading layout file...\n\n",
      color_codes$green_end,
      verbose = verbose
    )
    results_plate <-
      read_layout_data(layout_file_path, results_plate, check_plate = check_plate, verbose = verbose)
  }
  verbose_cat(
    color_codes$green_start,
    "New plate object has been created!\n",
    color_codes$green_end, "\n",
    verbose = verbose
  )

  # consistency and validation checks
  if (check_plate) {
    verbose_cat(
      "Running consistency checks...\n",
      verbose = verbose
    )
    # if (!results_plate$check_analyte_consistency()) {
    #  verbose_cat(
    #    "(",
    #    color_codes$red_start,
    #    "WARNING",
    #    color_codes$red_end,
    #    ")",
    #    "\nInconsistent analytes in the plate - there are data of analytes undefined in the file\n",
    #    verbose = verbose
    #  )
    #}

    if (!results_plate$check_beads_number()) {
      verbose_cat(
        "(",
        color_codes$red_start,
        "WARNING",
        color_codes$red_end,
        ")",
        "\nPlate contains at least one region that did not reach the specified bead count - ", results_plate$min_events_per_bead, "\n",
        verbose = verbose
      )
    }
  }

  # add name from filepath
  filename_without_extension <- sub("\\.[^.]*$", "", basename(file_path))

  results_plate$plate_name <- filename_without_extension

  return(results_plate)
}

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

read_location_data_csv <- function(location_file_path, ...,
                                   verbose = TRUE) {
  # function reads the location data from the csv file
  verbose_cat("not tested implementation location csv file\n", verbose = verbose)
  location_data <-
    read.csv(location_file_path,
      header = TRUE,
      stringsAsFactors = FALSE
    )

  return(location_data)
}

read_location_data_xlsx <- function(location_file_path, ...,
                                    verbose = TRUE) {
  # function reads the location data from the xlsx file
  location_data <- readxl::read_xlsx(location_file_path, .name_repair = "unique_quiet")

  return(location_data)
}



extract_blocks <- function(file_path) {
  # from the unstructured file extract the blocks of data - luminex data is divided into blocks separated by some blank rows
  # this function reads the file and returns a list of blocks which are vectors of strings

  lines <- readLines(file_path)

  # Initialize an empty list to store blocks
  blocks <- list()

  # Initialize an empty vector to store the current block
  current_block <- list()


  # parse lines
  lines <- parse_lines(lines)

  # Loop through each line
  for (line in lines) {
    # If the line is an empty vector save the current block and start the next one
    if (length(line) == 0) {
      blocks[[length(blocks) + 1]] <- current_block
      current_block <- list()
    } else {
      # If the line is not blank, add it to the current block
      current_block <- c(current_block, list(line))
    }
  }

  # Save the last block (in case the file doesn't end with a blank line)
  blocks[[length(blocks) + 1]] <- current_block

  # remove empty blocks
  blocks <- blocks[sapply(blocks, function(x) {
    length(x) > 0
  })]

  return(blocks)
}

parse_lines <- function(lines, csv_delim = ",") {
  # function parses unstructured lines saved as strings and returns a list of vectors
  replacement_delim <- ";"

  if (csv_delim == ";") {
    replacement_delim <- ","
  }

  # remove trailing delims
  leading_trailing_delim_regex <-
    paste0("^", csv_delim, "*|", csv_delim, "*$")
  lines <-
    lapply(lines, function(line) {
      gsub(leading_trailing_delim_regex, "", line)
    })

  replace_delim_in_brackets <-
    paste0("(?:\\G(?!^)|\\()[^)(", csv_delim, "]*\\K,(?=[^)()*])")
  lines <-
    lapply(lines, function(line) {
      gsub(replace_delim_in_brackets, replacement_delim, line, perl = TRUE)
    })

  delim_regex <-
    paste0(
      "(?:^|",
      csv_delim,
      ')(?=[^"]|(")?)"?((?(1)(?:[^"]|"")*|[^',
      csv_delim,
      '"]*))"?(?=,|$)'
    )

  matches <-
    regmatches(lines, gregexpr(delim_regex, lines, perl = TRUE))
  # remove trailing delims
  matches <-
    lapply(matches, function(line) {
      gsub(leading_trailing_delim_regex, "", line)
    })

  # remove empty strings
  matches <- lapply(matches, function(line) {
    line[nzchar(line)]
  })
  return(matches)
}

divide_blocks <- function(blocks) {
  # function divides the blocks into header and results blocks


  results_block_index <- -1

  for (i in seq_len(length(blocks))) {
    # Check if the block contains the keyword "results"
    if (any(grepl("^Results", blocks[[i]], ignore.case = TRUE))) {
      results_block_index <- i
    }
  }

  if (results_block_index == -1) {
    stop(
      "No results block found, couldn't extract the header blocks, check your data formatting"
    )
  }

  header_blocks <- blocks[1:(results_block_index - 1)]
  results_blocks <- blocks[(results_block_index + 1):length(blocks)]


  return(list(header_blocks = header_blocks, results_blocks = results_blocks))
}


parse_header_blocks <-
  function(results_plate,
           header_blocks,
           check_consistency = TRUE,
           verbose = TRUE) {
    # this function parses the header blocks and writes the metadata into the results plate

    if (length(header_blocks) < 5) {
      stop("Improper data formatting - there are no enough blocks in the header section")
    }

    batch_info <-
      parse_batch_info(header_blocks[[1]], header_blocks[[2]])

    calibration_info <-
      parse_calibration_info(header_blocks[[3]], header_blocks[[4]])

    results_plate$calibration_info <- calibration_info


    sample_info <- parse_sample_info(header_blocks[[5]])

    if (check_consistency) {
      if (results_plate$number_of_samples != sample_info$samples_count) {
        stop(
          "According to plate metadata there are ",
          sample_info$samples_count,
          " samples in the plate, but ",
          results_plate$number_of_samples,
          "found"
        )
      }

      # TODO what is min events ?
      batch_info$min_events_per_bead <-
        sample_info$min_events_per_bead
    }


    results_plate$batch_info <- batch_info

    return(results_plate)
  }


parse_header <- function(header_blocks) {
  # this function parses the header and returns the metadata collected from the header

  batch_info <-
    parse_batch_info(header_blocks[[1]], header_blocks[[2]])

  # for now we skip the optional information

  calibration_info <-
    parse_calibration_info(header_blocks[[3]], header_blocks[[4]])

  sample_info <- parse_sample_info(header_blocks[[5]])

  return(
    list(
      batch_info = batch_info,
      calibration_info = calibration_info,
      sample_info = sample_info
    )
  )
}

parse_date <- function(date) {
  # this function parses the date and returns the date as a list

  date_format <- "%m/%d/%Y %I:%M %p"

  date <- strptime(date, format = date_format)


  return(date)
}

parse_batch_info <- function(program_block, sn_block) {
  # this function parses the program info and returns the program info as a list

  # join both the lists
  batch_list <- c(program_block, sn_block)

  # extract the metadata
  batch_info <- list()
  for (i in seq_len(length(batch_list))) {
    fields <- batch_list[[i]]
    field_name <- fields[1]
    if (length(fields) > 2) {
      # TODO better solution
      field_value <- paste(fields[2:length(fields)], collapse = " ")
    } else {
      field_value <- fields[2]
    }
    batch_info[[field_name]] <- field_value
  }

  batch_info$examination_date <- parse_date(batch_info$Date)
  batch_info$batch_name <- batch_info$Batch


  # TODO add exceptions


  return(batch_info)
}


parse_calibration_info <- function(cal_block1, cal_block2) {
  # this function parses the block with calibration data and returns the calibration data as a list

  # TODO

  return(list())
}

parse_sample_info <- function(sample_block) {
  # this function parses the sample info and returns the sample info as a list

  sample_vector <- sample_block[[1]]
  sample_info <- list()
  if (sample_vector[1] != "Samples") {
    stop("error in formating, samples block misconfigured")
  }
  samples_count <- as.numeric(sample_vector[2])

  sample_info$samples_count <- samples_count
  sample_info$min_events_per_bead <- as.numeric(sample_vector[4])

  return(sample_info)
}




parse_results_blocks <- function(results_blocks, verbose = TRUE) {
  # this function parses the plate data and returns informations about the antigens - MFI and different measures


  plate <- Plate$new()

  for (i in 1:length(results_blocks)) {
    results_block <- results_blocks[[i]]

    parsed_list <-
      parse_single_results_block(results_block, verbose = verbose)

    # add parsed list to plate
    plate$add_results_block(parsed_list$data_type, parsed_list[[2]])
  }

  return(plate)
}



# function returns list with two fileds - first one is type of object stored in the list and the second it a list with the objects of given type
parse_single_results_block <-
  function(results_block, verbose = TRUE) {
    # function parses single results block data and returns list of samples

    vector_data_type <-
      results_block[[1]] # extract the first row that should contain the datatype of the block

    if (length(vector_data_type) == 1) {
      # probably reached CRC block

      if (vector_data_type[1] == "-- CRC --") {
        verbose_cat(
          "(",
          color_codes$yellow_start,
          "NOTE",
          color_codes$yellow_end,
          ")\n",
          "CRC block found, omiting it for now\n",
          verbose = verbose
        )
        return(list(data_type = "CRC", list()))
      }

      return(list(data_type = "CRC", list()))
    }

    data_type <- vector_data_type[2]

    block_header <- results_block[[2]] # fields and antigen names

    if (length(results_block) <= 2) {
      # there are no data stored
      return(list(data_type = data_type, list()))
    }

    results_df <-
      as.data.frame(do.call(rbind, results_block[3:length(results_block)]))
    names(results_df) <- block_header

    warning_datatypes <- c("Warnings/Errors", "Audit Logs")

    if (data_type %in% warning_datatypes) {
      warnings <- list()

      if (nrow(results_df) == 0) {
        return(list(data_type = data_type, list()))
      }


      for (row in 1:nrow(results_df)) {
        warning <- list()
        warning[[colnames(results_df)[1]]] <- results_df[row, 1]
        warning[[colnames(results_df)[2]]] <- results_df[row, 2]
        warning[[colnames(results_df)[3]]] <- results_df[row, 3]

        warnings[[results_df[row, 1]]] <- warning
      }

      return(list(data_type = data_type, warnings))
    }

    analysis_datatypes <- c("Alysis Types", "Analysis Types")
    if (data_type %in% analysis_datatypes) {
      rownames(results_df) <- results_df[1:nrow(results_df), 1]
      results_df <-
        results_df[, -1] # remove the first column - it should have the known format
      # replace values
      null_values <- c("None", "Alysis Types", "Analysis Types")
      for (val in null_values) {
        results_df[results_df == val] <- NA
      }

      if (any(is.na(results_df))) {
        verbose_cat(
          "(",
          color_codes$red_start,
          "WARNING",
          color_codes$red_end,
          ")",
          "\nThe datatype ",
          data_type,
          " contains NA values\n",
          verbose = verbose
        )
      }

      analyte_types <- list()
      for (col in 1:ncol(results_df)) {
        analyte_name <- colnames(results_df)[col]

        analysis_type <- results_df[1, col]

        if (is.na(analysis_type)) {
          analysis_type <- NULL
        }

        analyte_types[[analyte_name]] <- analysis_type
      }
      return(list(data_type = "analyte_types", analyte_types))
    }


    beads_datatypes <- c("Units", "Per Bead Count")

    if (data_type %in% beads_datatypes) {
      rownames(results_df) <- results_df[1:nrow(results_df), 1]
      results_df <-
        results_df[, -1] # remove the first column - it should have the known format
      # replace values
      null_values <- c("None", "Units", "Units:")
      for (val in null_values) {
        results_df[results_df == val] <- NA
      }


      if (any(is.na(results_df))) {
        verbose_cat(
          "(",
          color_codes$red_start,
          "WARNING",
          color_codes$red_end,
          ")\n",
          "The datatype ",
          data_type,
          " contains NA values\n",
          verbose = verbose
        )
      }

      analytes <- list()
      for (col in 1:ncol(results_df)) {
        analyte_name <- colnames(results_df)[col]
        id <- NA
        per_bead_count <- NA
        analyte_units <- NULL
        analysis_type <- NULL


        if (data_type == "Units") {
          analyte_units <- results_df[2, col]
        } else if (data_type == "Per Bead Count") {
          per_bead_count <- as.numeric(results_df[2, col])
        }

        id <- as.numeric(results_df[1, col])


        if (!is.null(analyte_units) && is.na(analyte_units)) {
          analyte_units <- NULL
        }

        analyte <-
          Analyte$new(
            id = id,
            analyte_name = analyte_name,
            units = analyte_units,
            bead_count = per_bead_count,
            analysis_type = analysis_type
          )

        analytes[[as.character(id)]] <- analyte
      }
      return(list(data_type = "analytes", analytes))
    }



    results_types <- c()


    # the results dataframe contains a data for each sample of certain datatype extracted as above

    # check if the dataframe contains a column with name or Location

    first_analyte_col_index <- 1
    if ("Location" %in% names(results_df)) {
      first_analyte_col_index <- first_analyte_col_index + 1
    }
    if ("Sample" %in% names(results_df)) {
      first_analyte_col_index <- first_analyte_col_index + 1
    }



    samples <- list()

    for (row in 1:nrow(results_df)) {
      id <- row # TODO better labeling

      if (is.null(results_df[row, "Sample"])) {
        stop(paste0(
          "No name specified for the sample of id: ",
          row,
          " - omiting it"
        ))
        next
      }

      name <- results_df[row, "Sample"]
      sample_location <- NULL
      dilution_factor <- NA
      sample_df <- data.frame()

      if ("Location" %in% names(results_df)) {
        sample_location <-
          SampleLocation$parse_sample_location(results_df[row, "Location"])
      }

      if (data_type == "Dilution Factor") {
        dilution_factor <- as.numeric(results_df[row, "Dilution Factor"])
      } else {
        sample_df <-
          results_df[row, first_analyte_col_index:length(results_df)]

        sample_df <-
          cbind(data_type = c(data_type), sample_df) # add row with the datatype to the dataframe

        rownames(sample_df) <- sample_df$data_type
        sample_df$data_type <- NULL
      }

      sample_type <-
        SampleType$parse_sample_type(results_df[row, "Sample"], dilution_factor = dilution_factor)

      sample <-
        Sample$new(
          id = id,
          sample_name = name,
          sample_type = sample_type,
          sample_location = sample_location,
          data = sample_df
        )
      samples[[row]] <- sample
    }

    return(list(data_type = "samples", samples))
  }




parse_crc <- function(results_block) {
  # function parses the CRC data

  # TODO

  return(list())
}

verbose_cat <- function(..., verbose = TRUE) {
  if (verbose) {
    cat(..., sep = "")
  }
}


#
# colors for WARNING, NOTE, DEFAULT
#
color_codes <-
  list(
    yellow_start = "\033[33m",
    yellow_end = "\033[39m",
    red_start = "\033[31m",
    red_end = "\033[39m",
    green_start = "\033[32m",
    green_end = "\033[39m"
  )
