# Some description of the module
#
#


### Utility functions

is_line_blank <- function(line) {
  if (!is.scalar(line)) {
    stop("Line must be a scalar character value")
  }
  if (is.na(line)) {
    return(TRUE)
  }
  stringr::str_detect(line, "^[;,\"]*$")
}

vectorize_csv_line <- function(line, separator) {
  line_stripped <- stringr::str_remove(line, "[\\s,;]*$")
  as.character(read.csv(
    text = line_stripped,
    header = FALSE,
    sep = separator,
    quote = '\"',
    allowEscapes = TRUE,
    stringsAsFactors = FALSE
  ))
}

parsing_error <- function(index, lines, parser_name, reason) {
  begin_index <- max(1, index - 5)
  named_lines <- names(lines)[begin_index:index]
  names(named_lines) <- begin_index:index
  named_lines_str <- capture.output(print(named_lines))

  paste0(
    "Parsing error occurred while parsing line: ", index, ".\n",
    "Parser: ", parser_name, ".\n",
    "Reason: ", reason, ".\n",
    "Lines parsed before: \n",
    paste0(named_lines_str, collapse = "\n")
  )
}


### Simple parsers

skip_blanks_parser <- function(index, lines) {
  while (is_line_blank(lines[index]) && (index <= length(lines))) {
    names(lines)[index] <- "BLANK"
    index <- index + 1
  }
  list(NULL, index, lines)
}

eof_parser <- function(index, lines) {
  if (index < length(lines)) {
    stop(parsing_error(
      index,
      lines,
      "EOF parser",
      "Expected end of file but found next line at index. File was not fully parsed."
    ))
  }
  list(NULL, index, lines)
}


### Combinators

join_parsers <- function(..., do_skip_blanks = FALSE) {
  function(index, lines) {
    outputs <- list()
    for (parser in list(...)) {
      output <- parser(index, lines)
      if (length(output) != 3) {
        stop("Internal error: Parser should return a list with 3 elements")
      }
      parsed_output <- output[[1]]
      outputs <- c(outputs, parsed_output)
      index <- output[[2]]
      lines <- output[[3]]

      if (do_skip_blanks) {
        sb_out <- skip_blanks_parser(index, lines)
        index <- sb_out[[2]]
        lines <- sb_out[[3]]
      }
    }
    list(outputs, index, lines)
  }
}

make_optional <- function(parser) {
  function(index, lines) {
    tryCatch(parser(index, lines), error = function(e) {
      list(NULL, index, lines)
    })
  }
}

match_any_parser <- function(...) {
  function(index, lines) {
    for (parser in list(...)) {
      opt_parser <- make_optional(parser)
      output <- opt_parser(index, lines)
      if (!is.null(output[[1]])) {
        return(output)
      }
    }
    stop(parsing_error(
      index, lines,
      "Match any parser",
      "No parser matched starting at this line."
    ))
  }
}

repeat_parser <- function(parser) {
  function(index, lines) {
    joined_outputs <- c()
    opt_parser <- make_optional(parser)
    parser_output <- opt_parser(index, lines)
    while (!is.null(parser_output[[1]])) {
      joined_outputs <- c(joined_outputs, parser_output[[1]])
      index <- parser_output[[2]]
      lines <- parser_output[[3]]
      parser_output <- opt_parser(index, lines)
    }
    list(joined_outputs, index, lines)
  }
}


### Output modifies

wrap_parser_output <- function(parser, name) {
  function(index, lines) {
    output <- parser(index, lines)
    output_list <- list()
    output_list[[name]] <- output[[1]]
    list(output_list, output[[2]], output[[3]])
  }
}

rename_parser_output <- function(parser, names) {
  function(index, lines) {
    output <- parser(index, lines)
    names(output[[1]]) <- names
    output
  }
}


### CSV parsers

check_key_parser <- function(regex, separator) {
  function(index, lines) {
    read_values <- vectorize_csv_line(lines[index], separator)
    match <- stringr::str_match(read_values[1], regex)
    if (is.na(match[1])) {
      stop(parsing_error(
        index, lines, "Check line content",
        paste0("Could not match the regex: `", regex, "`")
      ))
    }
    names(lines)[index] <- paste0("CH: ", regex)
    list(NULL, index + 1, lines)
  }
}

key_value_parser <- function(key_regex, separator, check_length = TRUE) {
  function(index, lines) {
    read_values <- vectorize_csv_line(lines[index], separator)
    if (check_length && length(read_values) > 2) {
      stop(parsing_error(
        index, lines, "Key,Value parser",
        paste0("Expected at most 2 values at line. Error occurred while trying to parse key: ", key_regex)
      ))
    }
    if (!stringr::str_detect(read_values[1], key_regex)) {
      stop(parsing_error(
        index, lines, "Key,Value parser",
        paste0("No key matching: `", key_regex, "` found.")
      ))
    }
    names(lines)[index] <- paste0("KV: ", read_values[1])
    output_list <- list()
    output_list[read_values[1]] <- read_values[2]
    list(output_list, index + 1, lines)
  }
}

named_key_value_pairs_parser <- function(line_key, separator) {
  function(index, lines) {
    read_values <- vectorize_csv_line(lines[index], separator)
    if (!stringr::str_detect(read_values[1], line_key)) {
      stop(parsing_error(
        index, lines, "Named,(Key,Value)* pairs parser",
        paste0("No key: ", line_key, " found")
      ))
    }
    if (length(read_values) < 3) {
      stop(parsing_error(
        index, lines, "Named,(Key,Value)* pairs parser",
        paste0("Expected at least 3 values")
      ))
    }
    keys_and_values <- read_values[-1]
    keys <- keys_and_values[seq(1, length(keys_and_values), 2)]
    values <- keys_and_values[seq(2, length(keys_and_values), 2)]
    if (length(keys) != length(values)) {
      stop(parsing_error(
        index, lines, "Named,(Key,Value)* pairs parser",
        paste0("Number of keys and values do not match")
      ))
    }
    names(values) <- keys

    names(lines)[index] <- paste0("NKVs: ", read_values[1])
    output_list <- list()
    output_list[[line_key]] <- as.list(values)
    list(output_list, index + 1, lines)
  }
}

key_value_pairs_parser <- function(separator) {
  function(index, lines) {
    read_values <- vectorize_csv_line(lines[index], separator)
    if (length(read_values) < 2) {
      stop(parsing_error(
        index, lines, "(Key,Value)* pairs parser",
        "Expected at least 2 values. Error occurred while trying to parse key-value pairs."
      ))
    }
    keys <- read_values[seq(1, length(read_values), 2)]
    values <- read_values[seq(2, length(read_values), 2)]
    values <- c(values, rep(NA, length(keys) - length(values)))
    if (length(keys) != length(values)) {
      stop(parsing_error(
        index, lines, "(Key,Value)* pairs parser",
        "Number of keys and values do not match."
      ))
    }

    names(lines)[index] <- paste0("KVs: ", paste(keys, collapse = ", "))
    names(values) <- keys
    list(as.list(values), index + 1, lines)
  }
}

read_until_parser <- function(stop_cond, separator, name = "Header") {
  function(index, lines) {
    current_index <- index

    while (current_index <= length(lines)) {
      if (!is_line_blank(lines[current_index])) {
        read_values <- vectorize_csv_line(lines[current_index], separator)
        if (stop_cond(read_values[1])) {
          break
        }
      }
      current_index <- current_index + 1
    }
    prev_index <- max(index, current_index - 1)
    names(lines)[index:prev_index] <- rep(name, prev_index - index + 1)


    output_list <- list()
    output_list[[name]] <- paste(lines[index:prev_index], collapse = "\n")

    list(output_list, current_index, lines)
  }
}

is_the_end_of_csv_section <- function(line, separator, empty_line_stop = TRUE) {
  if (is.na(line)) {
    return(TRUE)
  } else if (is_line_blank(line)) {
    return(empty_line_stop)
  } else {
    regex <- '^[\\s,;"\']*(DataType:|Samples|-- CRC --)'
    return(stringr::str_detect(line, regex))
  }
}


csv_table_parser <- function(name, separator, max_rows = Inf, remove_na_rows = FALSE, ...) {
  function(index, lines) {
    end_index <- index
    while (!is_the_end_of_csv_section(lines[end_index], separator, ...)) {
      end_index <- end_index + 1
    }
    end_index <- min(end_index, index + max_rows)

    mod_lines <- lines[index:(end_index - 1)]
    regex_check <- paste0("\"(\\d+\\(\\d+", separator, "\\w+\\d+\\))\"")
    regex <- paste0("(\\d+\\(\\d+", separator, "\\w+\\d+\\))")
    mod_lines <- as.character(sapply(
      mod_lines,
      function(line) {
        if (!stringr::str_detect(line, regex_check)) {
          replacement <- '\"$1\"'
          line <- stringi::stri_replace_all(line, regex = regex, replacement = replacement)
        }
        line
      }
    ))

    df <- read.csv(
      text = mod_lines,
      header = TRUE,
      sep = separator,
      na.strings = c("", "NA", "None", "<NA>")
    )
    df <- df[, colSums(is.na(df)) < nrow(df)]
    if ((any(nrow(df)) > 0) && remove_na_rows) {
      df <- df[rowSums(is.na(df)) < ncol(df), ]
    }

    names(lines)[index:(end_index - 1)] <- rep(paste0("CSV: ", name), end_index - index)
    output_list <- list()
    output_list[[name]] <- df
    list(output_list, end_index, lines)
  }
}


### Program metadata
program_build_data_parser <- function(separator) {
  function(index, lines) {
    read_values <- vectorize_csv_line(lines[index], separator)
    if (length(read_values) < 3) {
      stop(parsing_error(
        index, lines,
        "Parse build date",
        "Expected at least 3 values"
      ))
    }
    if (read_values[1] != "Date") {
      stop(parsing_error(
        index, lines,
        "Parse build date",
        "The line doesn't start with `Date`"
      ))
    }
    names(lines)[index] <- "Build Date"
    list(list(Date = read_values[2], Time = read_values[3]), index + 1, lines)
  }
}

program_metadata_parser <- function(separator) {
  function(index, lines) {
    output <- join_parsers(
      key_value_parser("Program", separator, check_length = FALSE),
      key_value_parser("Build", separator),
      program_build_data_parser(separator),
      skip_blanks_parser,
      key_value_parser("SN", separator),
      match_any_parser(
        key_value_parser("Batch", separator),
        key_value_parser("Session", separator)
      )
    )(index, lines)
    # Rename Session to Batch if it exists
    if ("Session" %in% names(output[[1]])) {
      output[[1]]$Batch <- output[[1]]$Session
    }
    list(list(ProgramMetadata = output[[1]]), output[[2]], output[[3]])
  }
}

### Batch metadata
batch_metadata_parser <- function(separator) {
  function(index, lines) {
    output <- join_parsers(
      make_optional(key_value_parser("Version", separator)),
      key_value_parser("Operator", separator),
      make_optional(
        match_any_parser(
          key_value_parser("Computerme", separator),
          key_value_parser("ComputerName", separator)
        )
      ),
      make_optional(key_value_parser("Country Code", separator)),
      match_any_parser(
        repeat_parser(key_value_parser("Protocol\\w+", separator)),
        repeat_parser(key_value_parser("Template\\w+", separator))
      ),
      make_optional(key_value_parser("PanelName", separator)),
      make_optional(key_value_parser("MaxSampleUptakeVolume", separator)),
      repeat_parser(key_value_parser("Sample\\w+", separator)),
      make_optional(key_value_parser("DDGate", separator)),
      make_optional(key_value_parser("SampleTimeout", separator)),
      skip_blanks_parser,
      repeat_parser(key_value_parser("Batch\\w+", separator)),
      make_optional(named_key_value_pairs_parser("ProtocolPlate", separator)),
      make_optional(named_key_value_pairs_parser("ProtocolMicrosphere", separator)),
      make_optional(
        named_key_value_pairs_parser("ProtocolAnalysis", separator)
      ),
      repeat_parser(key_value_parser("Protocol\\w+", separator)),
      make_optional(key_value_parser("NormBead", separator)),
      make_optional(match_any_parser(
        key_value_parser("ProtocolHeater", separator),
        named_key_value_pairs_parser("ProtocolHeater", separator)
      )),
      make_optional(key_value_parser("ProtocolOperatingMode", separator)),
      make_optional(key_value_parser("BeadType", separator)),
      make_optional(key_value_parser("PrePlateRoutine", separator)),
      make_optional(key_value_parser("PostPlateRoutine", separator)),
      make_optional(key_value_parser("PostWellRoutine", separator)),
      make_optional(key_value_parser("PlateReadDirection", separator))
    )(index, lines)
    list(list(BatchMetadata = output[[1]]), output[[2]], output[[3]])
  }
}

### Calibration metadata
calibration_metadata_parser <- function(separator) {
  function(index, lines) {
    output <- join_parsers(
      check_key_parser("^Most Recent Calibration", separator),
      repeat_parser(key_value_parser("Last\\s*\\w*\\s*Calibration", separator)),
      repeat_parser(key_value_parser("Last\\s*\\w*\\s*Verification", separator)),
      repeat_parser(key_value_parser("Last\\s*\\w*\\s*Test", separator)),
      skip_blanks_parser,
      check_key_parser("CALInfo:", separator),
      # HACK: This is not fully correct calibrator outputs are overwritten
      repeat_parser(join_parsers(
        check_key_parser("Calibrator", separator),
        csv_table_parser("Calibrator", separator, max_rows = 2)
      ))
    )(index, lines)
    list(list(CalibrationMetadata = output[[1]]), output[[2]], output[[3]])
  }
}

### Assay info block
assay_info_parser <- function(separator) {
  join_parsers(
    check_key_parser("^AssayInfo", separator),
    csv_table_parser("AssayInfo", separator),
    skip_blanks_parser,
    check_key_parser("^AssayInfo2", separator)
  )
}

### Results block
results_block_parser <- function(separator) {
  function(index, lines) {
    output_dfs <- c()
    while (!is.na(lines[index]) && stringr::str_detect(lines[index], "DataType:")) {
      second <- key_value_parser("DataType:", separator)(index, lines)
      index <- second[[2]]
      lines <- second[[3]]

      df_name <- second[[1]]$DataType
      third <- join_parsers(
        csv_table_parser(
          df_name, separator,
          empty_line_stop = FALSE, remove_na_rows = TRUE
        ),
        skip_blanks_parser
      )(index, lines)
      index <- third[[2]]
      lines <- third[[3]]

      output_dfs <- c(output_dfs, third[[1]])
    }

    list(list(Results = output_dfs), index, lines)
  }
}


### CRC32 block

crc32_parser <- function(separator) {
  parse_crc32_value <- function(index, lines) {
    read_values <- vectorize_csv_line(lines[index], separator)
    match <- stringr::str_match(read_values[1], "^CRC32:\\s*(.+?)(,|;|$)")
    if (is.na(match[1])) {
      stop(parsing_error(
        index, lines,
        "CRC32 value parser",
        "No CRC32 found"
      ))
    }
    names(lines)[index] <- paste0("CRC32: ", match[2])
    list(list(CRC32 = match[2]), index + 1, lines)
  }
  join_parsers(
    check_key_parser("^-- CRC --", separator),
    parse_crc32_value
  )
}


### Main parser

#' Read the xPONENT format data
#'
#' @param path Path to the xPONENT file
#' @param exact_parse Whether to parse the file exactly or not
#' Exact parsing means that the batch, calibration and assay metadata will be parsed as well
#' @param encoding Encoding of the file
#' @param separator Separator for the CSV values
#' @param verbose Whether to print the progress. Default is `TRUE`
#'
#' @import stringr
#' @import readr
#'
#' @export
read_xponent_format <- function(path, exact_parse = FALSE, encoding = "utf-8", separator = ",", verbose = TRUE) {
  lines <- readr::read_lines(
    path,
    locale = readr::locale(encoding = encoding),
  )

  names(lines) <- rep(NA, length(lines))

  sep <- separator # alias
  if (exact_parse) {
    header_parser <- wrap_parser_output(
      join_parsers(
        batch_metadata_parser(sep),
        make_optional(calibration_metadata_parser(sep)),
        make_optional(assay_info_parser(sep)),
        do_skip_blanks = TRUE
      ),
      "Header"
    )
  } else {
    header_parser <- wrap_parser_output(
      read_until_parser(function(value) stringr::str_detect(value, "^Samples"), sep),
      "Header"
    )
  }

  main_parser <- join_parsers(
    program_metadata_parser(sep),
    header_parser,
    key_value_pairs_parser(sep), # Samples
    check_key_parser("^Results", sep),
    results_block_parser(sep),
    make_optional(crc32_parser(sep)),
    eof_parser,
    do_skip_blanks = TRUE
  )

  out <- main_parser(1, lines)
  out[[1]]
}
