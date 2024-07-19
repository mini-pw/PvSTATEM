# Some description of the module
#
#


### Utility functions

is_line_blank <- function(line) {
  if (is.na(line)) {
    return(TRUE)
  }
  stringr::str_detect(line, "^[;,\"]*$")
}


global_sep <<- ","
vectorize_csv_line <- function(line) {
  line_stripped <- stringr::str_remove(line, "[\\s,;]*$")
  as.character(read.csv(
    text = line_stripped,
    header = FALSE,
    sep = global_sep,
    quote = '\"',
    allowEscapes = TRUE,
    stringsAsFactors = FALSE
  ))
}

is_the_end_of_csv_section <- function(line, empty_line_stop = TRUE) {
  if (is.na(line)) {
    return(TRUE)
  } else if (empty_line_stop && is_line_blank(line)) {
    return(TRUE)
  } else {
    return(
      stringr::str_detect(line, "^DataType:") ||
        stringr::str_detect(line, "^Samples") ||
        stringr::str_detect(line, "^-- CRC --")
    )
  }
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

skip_blanks <- function(index, lines) {
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


### Generic parsers

check_and_skip <- function(regex) {
  function(index, lines) {
    read_values <- vectorize_csv_line(lines[index])
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

key_value_parser <- function(key_regex, check_length = TRUE) {
  function(index, lines) {
    read_values <- vectorize_csv_line(lines[index])
    if (check_length && length(read_values) > 2) {
      stop(parsing_error(
        index, lines, "Key,Value parser",
        paste0("Expected at most 2 values at line. Error occured while trying to parse key: ", key_regex)
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

named_key_value_pairs_parser <- function(line_key) {
  function(index, lines) {
    read_values <- vectorize_csv_line(lines[index])
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

key_value_pairs_parser <- function(index, lines) {
  read_values <- vectorize_csv_line(lines[index])
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


# max_rows are counter with the header
parse_as_csv <- function(name, max_rows = Inf, remove_na_rows = FALSE, ...) {
  function(index, lines) {
    end_index <- index
    while (!is_the_end_of_csv_section(lines[end_index], ...)) {
      end_index <- end_index + 1
    }
    end_index <- min(end_index, index + max_rows)

    df <- read.csv(
      text = lines[index:(end_index - 1)],
      header = TRUE,
      sep = global_sep,
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
        sb_out <- skip_blanks(index, lines)
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


### Program metadata
parse_program_build_date <- function(index, lines) {
  read_values <- vectorize_csv_line(lines[index])
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

parse_program_metadata <- function(index, lines) {
  output <- join_parsers(
    key_value_parser("Program", check_length = FALSE),
    key_value_parser("Build"),
    parse_program_build_date
  )(index, lines)
  list(list(ProgramMetadata = output[[1]]), output[[2]], output[[3]])
}


### Batch metadata
parse_batch_metadata <- function(index, lines) {
  output <- join_parsers(
    key_value_parser("SN"),
    match_any_parser(
      key_value_parser("Batch"),
      key_value_parser("Session")
    ),
    make_optional(key_value_parser("Version")),
    key_value_parser("Operator"),
    make_optional(
      match_any_parser(
        key_value_parser("Computerme"),
        key_value_parser("ComputerName")
      )
    ),
    make_optional(key_value_parser("Country Code")),
    match_any_parser(
      repeat_parser(key_value_parser("Protocol\\w+")),
      repeat_parser(key_value_parser("Template\\w+"))
    ),
    make_optional(key_value_parser("PanelName")),
    make_optional(key_value_parser("MaxSampleUptakeVolume")),
    repeat_parser(key_value_parser("Sample\\w+")),
    make_optional(key_value_parser("DDGate")),
    make_optional(key_value_parser("SampleTimeout")),
    skip_blanks,
    repeat_parser(key_value_parser("Batch\\w+")),
    make_optional(named_key_value_pairs_parser("ProtocolPlate")),
    make_optional(named_key_value_pairs_parser("ProtocolMicrosphere")),
    make_optional(
      named_key_value_pairs_parser("ProtocolAnalysis")
    ),
    repeat_parser(key_value_parser("Protocol\\w+")),
    make_optional(key_value_parser("NormBead")),
    make_optional(match_any_parser(
      key_value_parser("ProtocolHeater"),
      named_key_value_pairs_parser("ProtocolHeater")
    )),
    make_optional(key_value_parser("ProtocolOperatingMode")),
    make_optional(key_value_parser("BeadType")),
    make_optional(key_value_parser("PrePlateRoutine")),
    make_optional(key_value_parser("PostPlateRoutine")),
    make_optional(key_value_parser("PostWellRoutine")),
    make_optional(key_value_parser("PlateReadDirection"))
  )(index, lines)
  list(list(BatchMetadata = output[[1]]), output[[2]], output[[3]])
}

### Calibration metadata
parse_calibration_metadata <- function(index, lines) {
  output <- join_parsers(
    check_and_skip("^Most Recent Calibration"),
    repeat_parser(key_value_parser("Last\\s*\\w*\\s*Calibration")),
    repeat_parser(key_value_parser("Last\\s*\\w*\\s*Verification")),
    repeat_parser(key_value_parser("Last\\s*\\w*\\s*Test")),
    skip_blanks,
    check_and_skip("CALInfo:"),
    # HACK: This is not fully correct calibrator outputs are overwritten
    repeat_parser(join_parsers(
      check_and_skip("Calibrator"),
      parse_as_csv("Calibrator", max_rows = 2)
    ))
  )(index, lines)
  list(list(CalibrationMetadata = output[[1]]), output[[2]], output[[3]])
}

### Assay info block
parse_assay_info <- join_parsers(
  check_and_skip("^AssayLotInfo"),
  parse_as_csv("AssayLotInfo"),
  skip_blanks,
  parse_as_csv("AssayLotInfo2")
)

### Results block
parse_results_block <- function(index, lines) {
  output_dfs <- c()
  while (!is.na(lines[index]) && stringr::str_detect(lines[index], "DataType:")) {
    second <- key_value_parser("DataType:")(index, lines)
    index <- second[[2]]
    lines <- second[[3]]

    df_name <- second[[1]]$DataType
    third <- join_parsers(
      parse_as_csv(df_name, empty_line_stop = FALSE, remove_na_rows = TRUE),
      skip_blanks
    )(index, lines)
    index <- third[[2]]
    lines <- third[[3]]

    output_dfs <- c(output_dfs, third[[1]])
  }

  list(list(Results = output_dfs), index, lines)
}


### CRC32 block

parse_crc32_value <- function(index, lines) {
  read_values <- vectorize_csv_line(lines[index])
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

parse_crc32_block <- function(index, lines) {
  output <- join_parsers(
    check_and_skip("^-- CRC --"),
    parse_crc32_value
  )(index, lines)
  list(output[[1]], output[[2]], output[[3]])
}


### Main parser

read_xponent_format <- function(path, encoding = "utf-8", sep = ",") {
  lines <- readr::read_lines(
    path,
    locale = readr::locale(encoding = encoding),
  )

  # HACK: There has to be a better way
  global_sep <<- sep

  names(lines) <- rep(NA, length(lines))

  main_parser <- join_parsers(
    parse_program_metadata,
    parse_batch_metadata,
    make_optional(parse_calibration_metadata),
    make_optional(parse_assay_info),
    key_value_pairs_parser,
    check_and_skip("^Results"),
    parse_results_block,
    make_optional(parse_crc32_block),
    eof_parser,
    do_skip_blanks = TRUE
  )

  out <- main_parser(1, lines)
  out[[1]]
}
