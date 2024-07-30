#' @title Plate
#' @description
#' A class to represent the luminex plate. It contains information about
#' the samples and analytes that were examined on the plate as well as
#' some additional metadata and batch info
#'
#'
#'
#' @examples
#'
#' plate_filepath <- system.file("extdata",
#'   "CovidOISExPONTENT_CO.csv",
#'   package = "PvSTATEM",
#'   mustWork = TRUE
#' ) # get the filepath of the csv dataset
#' layout_filepath <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx",
#'   package = "PvSTATEM", mustWork = TRUE
#' ) # get the filepath of the layout file
#'
#' plate <- read_data(plate_filepath, layout_filepath)
#'
#' plate$summary()
#'
#' @export
Plate <- R6::R6Class(
  "Plate",
  public = list(

    #' @field analytes list of all analytes measured during an examination
    analytes = list(),

    #' @field samples list of all samples examined on a plate
    samples = list(),

    #' @field batch_info a raw list containing all the metadata about
    #' the plate read from the Luminex file
    batch_info = list(),

    #' @field calibration_info a list containing calibration logs read from
    #' the Luminex file
    calibration_info = list(),

    #' @field audit_logs a list containing audit logs read from Luminex file
    audit_logs = list(),

    #' @field plate_name - plate name obtained from filename
    plate_name = "",


    #' @description
    #' creates a new `Plate` object
    #'
    #' @param analytes (`list`)
    #' list of all analytes measured during an examination

    #' @param samples (`list`)
    #' list of all samples examined on a plate

    #' @param batch_info (`list`) \cr
    #' a raw list containing all the metadata about the plate read from
    #' the Luminex file

    #' @param calibration_info (`list`) \cr
    #' a list containing calibration logs read from Luminex file

    #' @param audit_logs (`list`)
    #' a list containing audit logs read from Luminex file
    initialize = function(analytes = list(),
                          samples = list(),
                          batch_info = list(),
                          calibration_info = list(),
                          audit_logs = list(),
                          plate_name = "") {
      # check for valid input
      self$analytes <- analytes
      self$samples <- samples
      self$batch_info <- batch_info
      self$calibration_info <- calibration_info
      self$audit_logs <- audit_logs
      self$plate_name <- plate_name
    },

    #' @description
    #' Function prints the basic information about the plate
    #' such as the number of samples and analytes
    #' @return invisible object
    print = function(...) {
      cat(
        "Plate with",
        length(self$samples),
        "samples and",
        length(self$analytes),
        "analytes\n"
      )
      invisible(self)
    },


    #' @description
    #' Function outputs basic information about the plate, such as examination date,
    #' batch name, and sample types
    #'
    #' @param include_names If `include_names` parameter is `TRUE`, a
    #' part from count of control samples, provides also their names.
    #' By default `FALSE`
    summary = function(..., include_names = FALSE) {
      positive_control_samples_list <- self$get_sample_by_type("POSITIVE CONTROL")
      negative_control_samples_list <- self$get_sample_by_type("NEGATIVE CONTROL")
      standard_curve_samples_list <- self$get_sample_by_type("STANDARD CURVE")

      blank_samples_num <-
        length(self$get_sample_by_type("BLANK"))
      positive_control_num <- length(positive_control_samples_list)
      negative_control_num <- length(negative_control_samples_list)
      standard_curve_num <- length(standard_curve_samples_list)

      positive_control_names <- ""
      negative_control_names <- ""
      standard_curve_names <- ""

      if (include_names) {
        if (positive_control_num > 0) {
          positive_control_names <- paste(sapply(positive_control_samples_list, function(sample) paste0("'", sample$sample_name, "'")), collapse = ", ")
          positive_control_names <- paste0("\nSample names: ", positive_control_names)
        }

        if (negative_control_num > 0) {
          negative_control_names <- paste(sapply(negative_control_samples_list, function(sample) paste0("'", sample$sample_name, "'")), collapse = ", ")
          negative_control_names <- paste0("\nSample names: ", negative_control_names)
        }
        if (standard_curve_num > 0) {
          standard_curve_names <- paste(sapply(standard_curve_samples_list, function(sample) paste0("'", sample$sample_name, "'")), collapse = ", ")
          standard_curve_names <- paste0("\nSample names: ", standard_curve_names)
        }
      }

      cat(
        "Summary of the plate generated on ", as.character(self$examination_date),
        "\nwith name '", plate$plate_name, "':\n",
        "Total number of samples: ",
        self$number_of_samples,
        "\n",
        "Number of blank samples: ",
        blank_samples_num,
        "\n",
        "Number of standard curve samples: ",
        standard_curve_num,
        standard_curve_names, "\n",
        "Number of positive control samples: ",
        positive_control_num,
        positive_control_names, "\n",
        "Number of negative control samples: ",
        negative_control_num,
        negative_control_names, "\n",
        sep = ""
      )

      invisible(self)
    },

    #' @description
    #' function adds block of information to the current plate
    #'
    #'
    #' the analysis type should be added after the analysis,
    #' otherwise there is no enough information about samples
    add_results_block = function(data_type, parsed_block) {
      # verify the data type
      if (data_type == "CRC") {
        return(0)
      }
      if (data_type == "Audit Logs") {
        self$audit_logs <- parsed_block
        return(0)
      }
      if (data_type == "Warnings/Errors") {
        # warnings should be at the end of the file, thus we assume that there exists samples in given locations
        for (warning in parsed_block) {
          location <- SampleLocation$parse_sample_location(warning$Location)
          sample <- self$get_sample(location)
          if (warning$Status == "Warning") {
            sample$warnings <- c(sample$warnings, warning$Message)
          } else if (warning$Status == "Error") {
            sample$errors <- c(sample$errors, warning$Message)
          }
        }
      }
      if (data_type == "samples") {
        if (length(self$samples) == 0) {
          self$samples <- parsed_block
        } else {
          # join the lists of analytes
          new_samples <- parsed_block
          for (i in seq_along(new_samples)) {
            new_sample <- new_samples[[i]]
            if (new_sample$id %in% sapply(self$samples, function(x) {
              x$id
            })) {
              # add new data to the existing sample
              self$samples[[new_sample$id]]$join(new_sample)
            } else {
              self$samples[[new_sample$id]] <- new_sample
            }
          }
        }
        return(0)
      }
      if (data_type == "analytes") {
        if (length(self$analytes) == 0) {
          self$analytes <- parsed_block
        }
        # join the lists of analytes
        new_analytes <- parsed_block
        for (i in seq_along(new_analytes)) {
          new_analyte <- new_analytes[[i]]
          if (new_analyte$id %in% sapply(self$analytes, function(x) {
            x$id
          })) {
            # add new data to the existing sample
            self$analytes[[as.character(new_analyte$id)]]$join(new_analyte)
          } else {
            self$analytes[[as.character(new_analyte$id)]] <- new_analyte
          }
        }
        return(0)
      }
      if (data_type == "analyte_types") {
        parsed_block <- Filter(Negate(is.null), parsed_block)

        if (length(self$analytes) == 0 || length(parsed_block)) {
          return(0)
        } # nothing to add

        for (analyte_name in names(parsed_block)) {
          analyte_type <- parsed_block[[analyte_name]]

          analyte_id <- self$get_analyte_id(analyte_name)
          if (length(analyte_id) > 0) {
            self$analytes[[analyte_id]]$analysis_type <- analyte_type
          }
        }
        return(0)
      }
    },

    #' @description
    #' Function verifies if the batch info contain valid information
    check_batch_info = function() {
      # we will require only some of the fields

      if (is.null(self$examination_date)) {
        stop("No examination date provided or in bad format")
      }

      if (is.null(self$batch_name)) {
        stop("No batch name provided or is in bad format")
      }


      return(TRUE)
    },

    #' @description
    #' checks analyte consistency - verifies if all of the analytes contained within the samples are listed in the `analytes` list of the plate object
    check_analyte_consistency = function() {
      additional_column_names <- c("Total Events")

      is_consistent <- TRUE

      analytes_in_plate <- self$analyte_names

      analytes_in_plate <- c(analytes_in_plate, additional_column_names)

      # check if all analytes in self$samples are saved in self$analytes
      for (sample in self$samples) {
        analytes_in_sample <- colnames(sample$data)
        diff_elements <-
          setdiff(analytes_in_sample, analytes_in_plate)


        if (length(diff_elements) > 0) {
          # Print differing elements
          warning_message <- paste0(
            "Analytes in the sample ",
            sample$sample_name,
            " that are not in plate: ",
            diff_elements
          )
          sample$warnings <- append(sample$warnings, warning_message)

          # Raise an error
          is_consistent <- FALSE
          # stop("Error: There are analytes in the sample not defined in the plate.")
        }
      }

      return(is_consistent)
    },

    #' @description
    #' function performs validity check - verifies which samples and analytes saved in the plate have the bead count lower than `min_events_per_bead`
    #'
    #' @param min_events_per_bead lower bound of acceptable number of events. By default equals to `min_events_per_bead` parameter saved in the plate object
    #'
    #' @return returns `TRUE` if there are samples with bead count lower than `min_events_per_bead`, otherwise `FALSE`
    check_beads_number = function(min_events_per_bead = self$min_events_per_bead) {
      below_min_list <- list()

      below_min_flag <- FALSE

      for (sample in self$samples) {
        if ("Count" %in% row.names(sample$data)) { # TODO there should be option for lowercase
          below_min <- which(sample$data["Count", ] < min_events_per_bead, arr.ind = TRUE)
          if (length(below_min) > 0) {
            below_min_analytes <- names(sample$data["Count", ])[below_min[, "col"]]
            new_warnings <- paste0("An analyte ", below_min_analytes, " did not reach the specified count in the given sample")
            sample$warnings <- c(sample$warnings, new_warnings)
          }
        }
      }

      return(below_min_flag)
    },

    #'
    #' @param sample sample name or its id
    #' @return sample object of given sample name or id
    get_sample = function(sample) {
      # get the sample by its name, id or location
      if ("SampleLocation" %in% class(sample)) {
        sample <-
          which(sapply(self$samples, function(x) {
            x$sample_location$location_name
          }) == sample$location_name)
      } else if (is.numeric(sample)) {
        if (sample < 0 || sample > plate$number_of_samples) {
          stop("Sample ID out of range")
        }
      } else {
        sample <-
          which(sapply(self$samples, function(x) {
            x$sample_name
          }) == sample)

        sample_by_loc <-
          which(sapply(self$samples, function(x) {
            x$sample_location$location_name
          }) == sample)

        sample <- c(sample, sample_by_loc)

        if (length(sample) == 0) {
          stop("Sample of given name nor location not found")
        }
      }
      return(self$samples[[sample]])
    },

    #' @description
    #' Function returns list of samples filtered by the type
    #'
    #' @param sample_type type of the sample to be filtered. Possible values are:
    #' SampleType$valid_sample_types
    #'
    #'
    #' @param exclude If `FALSE` returns list of samples with given `sample_type`,
    #' otherwise returns all samples except for the specified `sample_type`
    #'
    get_sample_by_type = function(sample_type, exclude = FALSE) {
      stopifnot(sample_type %in% SampleType$valid_sample_types)

      samples_by_type <- list()
      for (sample in self$samples) {
        if (sample$sample_type$sample_type == sample_type && !exclude) {
          samples_by_type <- append(samples_by_type, sample)
        } else if (sample$sample_type$sample_type != sample_type && exclude) {
          samples_by_type <- append(samples_by_type, sample)
        }
      }

      return(samples_by_type)
    },

    #'
    #' @return  analyte id of given name
    get_analyte_id = function(analyte_name) {
      analyte_id <-
        which(sapply(self$analytes, function(x) {
          x$analyte_name
        }) == analyte_name)
    },

    #' @description
    #' Function returns data for a specific analyte and sample.
    #'
    #' @param analyte An analyte name or its id of which data we want to extract
    #'
    #' @param sample sample name or id
    #' @param data_type if `NULL` returns whole column of the dataframe containing information about the sample.
    #' Otherwise tries to select the exact `data_type` from the dataframe and returns a single value
    #'
    #' @return Data about a sample and analyte
    get = function(analyte, sample, data_type = NULL) {
      # get the data for a specific analyte and sample

      if (length(sample) != 1) {
        stop("Only one sample can be passed, for now.")
      }

      if (is.numeric(analyte)) {
        # if there is passed id of analyte
        analyte <- as.character(analyte)
        analyte_name <- self$analytes[[analyte]]$analyte_name
      } else {
        analyte_name <- analyte
      }
      analyte_id <- self$get_analyte_id(analyte_name)
      if (length(analyte_id) == 0) {
        stop("Analyte of given name not found")
      }

      sample <- self$get_sample(sample)

      if (is.null(data_type)) {
        return(sample$data[analyte_name])
      } else {
        if (!data_type %in% row.names(sample$data[analyte_name])) {
          stop(paste0("Incorrect value for `data_type`: ", data_type))
        }
        return(sample$data[data_type, analyte_name])
      }


      return(sample)
    },

    #' @description
    #' Function adjusts the values of test samples by subtracting average of BLANK samples
    #' purpose of this operation is to remove background light
    #' In short it subtracts the values from data in all samples, except from Blanks. It does not subtract values from
    #' `Count` values
    #'
    #' @param method How the values of different blanks should be aggregated. By default `avg`. For now it is the only available method
    #' @param inplace Whether the method should produce new plate with adjusted values or not, By default `TRUE` - operates on the current plate.
    blank_adjustment = function(method = "avg", inplace = "TRUE") {
      if (private$blank_already_adjusted) {
        stop("Blank values have been already adjusted in this plate, if you want to try doing it using different method consider reversing this process")
      }

      private$standard_curve_private <- NULL
      private$blank_already_adjusted <- TRUE
      available_methods <- c("avg")
      if (!method %in% available_methods) {
        stop(paste0(method, "not available for now, consider using one of the following: ", available_methods))
      }

      if (inplace == FALSE) {
        newplate <- self$copy()
      } else {
        newplate <- self
      }

      blank_samples <- self$get_sample_by_type("BLANK") # these values will be subtracted
      non_blank_samples <- self$get_sample_by_type("BLANK",
        exclude = TRUE
      ) # from these values

      # aggregate blank values

      if (method == "avg") {
        agg_dataframe <- NULL
        for (sample in blank_samples) {
          if (is.null(agg_dataframe)) {
            agg_dataframe <- sample$data
          } else {
            agg_dataframe <- agg_dataframe + sample$data
          }
        }
        if ("Count" %in% rownames(agg_dataframe)) {
          agg_dataframe["Count", ] <- 0 # count row is omitted
        }
        if ("Total Events" %in% colnames(agg_dataframe)) {
          agg_dataframe["Total Events"] <- 0
        }

        agg_dataframe <- agg_dataframe / length(blank_samples)
        # average the results


        for (sample in non_blank_samples) {
          sample$data <- sample$data - agg_dataframe
          # subtract the aggregated values
        }
      }
    },


    #' @description
    #' Function verifies if there are any MFI values below zero
    #' after blank removal
    check_MFI_after_adjustment = function() {
      if (!self$check_if_blanks_already_adjusted) {
        stop("Consider adjusting the blanks first")
      }

      below_zero_list <- list()

      below_zero_flag <- FALSE

      for (sample in self$get_sample_by_type("BLANK", exclude = TRUE)) {
        below_min <- which(sample$data < 0, arr.ind = TRUE)
        if (length(below_min) > 0) {
          below_min_analytes <- names(sample$data)[below_min[, "col"]]
          new_warnings <- paste0(
            "An analyte ", below_min_analytes,
            " has value below 0 after blank adjustment"
          )
          sample$warnings <- c(sample$warnings, new_warnings)
        }
      }

      return(below_zero_flag)
    },


    #' @description performs copy of the plate
    copy = function() {
      stop("Not implemented yet")
    }
  ),
  private = list(
    blank_already_adjusted = FALSE,
    verbose = TRUE,
    standard_curve_private = NULL
  ),
  active = list(
    #' @field number_of_samples number of samples stored in the current plate
    number_of_samples = function() {
      return(length(self$samples))
    },

    #' @field analyte_names list of all analyte names saved in the plate
    analyte_names = function() {
      analyte_names <- c()
      for (analyte in self$analytes) {
        analyte_names <- c(analyte_names, analyte$analyte_name)
      }
      return(analyte_names)
    },

    #' @field sample_names list of all sample names
    sample_names = function() {
      sample_names <- c()
      for (sample in self$samples) {
        sample_names <- c(sample_names, sample$sample_name)
      }
      return(sample_names)
    },

    #' @field examination_date Metadata: date of the examination
    examination_date = function() {
      return(self$batch_info$Date)
    },

    #' @field batch_name Metdata: batch name
    batch_name = function() {
      if (!is.null(self$batch_info$batch_name) && !is.na(self$batch_info$batch_name)) {
        return(self$batch_info$batch_name)
      }
      return("___")
    },

    #' @field min_events_per_bead minimal number of events that is valid for one bead - sample and analyte
    min_events_per_bead = function() {
      if (is.null(self$batch_info$min_events_per_bead)) {
        return(50)
      }
      return(self$batch_info$min_events_per_bead)
    },
    #' @field check_if_blanks_already_adjusted flag that specifies
    #' if the blanks were already adjusted and its MFI values subtracted from
    #' remaining samples
    check_if_blanks_already_adjusted = function() {
      return(private$blank_already_adjusted)
    },

    #' @field warnings list of lists of all warnings from all samples
    warnings = function() {
      warnings <- lapply(self$samples, function(sample) sample$warnings)
      remove_empty_lists(warnings)
    },

    #' @field errors list of lists of all errors from all samples
    errors = function() {
      errors <- lapply(self$samples, function(sample) sample$errors)
      remove_empty_lists(errors)
    },
    standard_curve = function() {
      if (!is.null(private$standard_curve_private)) {
        return(private$standard_curve_private)
      }

      if (!self$check_if_blanks_already_adjusted) {
        verbose_cat(
          "(",
          color_codes$red_start,
          "WARNING",
          color_codes$red_end,
          ")",
          "\nBlank values not adjusted -
          Consider adjusting the blank values using function
          `plate$blank_adjustment`\n",
          verbose = private$verbose
        )
      }

      standard_curves <- self$get_sample_by_type("STANDARD CURVE")
      if (length(standard_curves) == 0) {
        verbose_cat(
          "(",
          color_codes$red_start,
          "WARNING",
          color_codes$red_end,
          ")",
          "\nNo standard curve samples found in the plate\n
          Using positive control samples",
          verbose = private$verbose
        )
        standard_curves <- plate$get_sample_by_type("POSITIVE CONTROL")
      }


      dilutions <- sapply(standard_curves, function(sample) {
        sample$sample_type$character_dilution_factor
      })
      dilutions_numeric <- sapply(standard_curves, function(sample) {
        sample$sample_type$dilution_factor
      })
      # sort values according to dilutions
      sorted_order <- order(dilutions_numeric)

      # Sort the vectors according to the sorted order of the reference vector
      dilutions_numeric <- dilutions_numeric[sorted_order]
      dilutions <- dilutions[sorted_order]
      standard_curves <- standard_curves[sorted_order]
      private$standard_curve_private <- standard_curves
      return(standard_curves)
    }
  )
)
