#' @title Logistic regression model for the standard curve
#'
#' @description
#' The Model class is a wrapper around the `nplr` model. It allows to predict the RAU (Relative Antibody Unit) values
#' directly from the MFI values of a given sample.
#'
#' The `nplr` model is fitted using the formula:
#' \deqn{y = B + \frac{T - B}{(1 + 10^{b \cdot (x_{mid} - x)})^s},}{y = B + (T - B) / (1 + 10^(b * (x_mid - x)))^s,}
#'
#' where:
#' - \eqn{y} is the predicted value, MFI in our case,
#' - \eqn{x} is the independent variable, dilution in our case,
#' - \eqn{B} is the bottom plateau - the right horizontal asymptote,
#' - \eqn{T} is the top plateau - the left horizontal asymptote,
#' - \eqn{b} is the slope of the curve at the inflection point,
#' - \eqn{x_{mid}}{x_mid} is the x-coordinate at the inflection point,
#' - \eqn{s} is the asymmetric coefficient.
#'
#' This equation is referred to as the Richards' equation. More information about the model can be found in the `nplr` package documentation.
#'
#' After the model is fitted to the data, the RAU values can be predicted using the `predict` method.
#' The RAU value is simply a predicted dilution value (using the standard curve) for a given MFI
#' multiplied by 1,000 000 to have a more readable value.
#' For more information about the differences between dilution, RAU and MFI values, please see the
#' "Normalisation" section in the "Basic PvSTATEM functionalities" vignette.
#'
#'
#' @examples
#' plate_file <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_layout.csv", package = "PvSTATEM")
#' plate <- read_luminex_data(plate_file, layout_filepath = layout_file)
#' model <- create_standard_curve_model_analyte(plate, "S2", log_mfi = TRUE)
#' print(model)
#'
#' @import nplr
#' @import dplyr
#'
Model <- R6::R6Class(
  "Model",
  public = list(

    #' @field analyte (`character(1)`)\cr
    #' Name of the analyte for which the model was fitted
    analyte = NULL,
    #' @field dilutions (`numeric()`)\cr
    #'  Dilutions used to fit the model
    dilutions = NULL,

    #' @field mfi (`numeric()`)\cr
    #'  MFI values used to fit the model
    mfi = NULL,

    #' @field mfi_min (`numeric(1)`)\cr
    #' Minimum MFI used for scaling MFI values to the range \[0, 1\]
    mfi_min = NULL,

    #' @field mfi_max (`numeric(1)`)\cr
    #' Maximum MFI used for scaling MFI values to the range \[0, 1\]
    mfi_max = NULL,

    #' @field model (`nplr`)\cr
    #' Instance of the `nplr` model fitted to the data
    model = NULL,

    #' @field log_dilution (`logical()`)\cr
    #' Indicator should the dilutions be transformed using the `log10` function
    log_dilution = TRUE,

    #' @field log_mfi (`logical()`)\cr
    #' Indicator should the MFI values be transformed using the `log10` function
    log_mfi = TRUE,

    #' @field scale_mfi (`logical()`)\cr
    #' Indicator should the MFI values be scaled to the range \[0, 1\]
    scale_mfi = TRUE,

    #' @description
    #' Create a new instance of Model [R6][R6::R6Class] class
    #'
    #' @param analyte (`character(1)`)\cr
    #'  Name of the analyte for which the model was fitted.
    #' @param dilutions (`numeric()`)\cr
    #'   Dilutions used to fit the model
    #' @param mfi MFI (`numeric()`)\cr
    #'   values used to fit the model
    #' @param npars (`numeric(1)`)\cr
    #'   Number of parameters to use in the model
    #' @param verbose (`logical()`)\cr
    #'   If `TRUE` prints messages, `TRUE` by default
    #' @param log_dilution (`logical()`)\cr
    #'   If `TRUE` the dilutions are transformed using the `log10` function, `TRUE` by default
    #' @param log_mfi (`logical()`)\cr
    #'   If `TRUE` the MFI values are transformed using the `log10` function, `TRUE` by default
    #' @param scale_mfi (`logical()`)\cr
    #'   If `TRUE` the MFI values are scaled to the range \[0, 1\], `TRUE` by default
    #' @param mfi_min (`numeric(1)`)\cr
    #'   Enables to set the minimum MFI value used for scaling MFI values to the range \[0, 1\].
    #'   Use values before any transformations (e.g., before the `log10` transformation)
    #' @param mfi_max (`numeric(1)`)\cr
    #'   Enables to set the maximum MFI value used for scaling MFI values to the range \[0, 1\].
    #'   Use values before any transformations (e.g., before the `log10` transformation)
    #'
    initialize = function(analyte, dilutions, mfi, npars = 5, verbose = TRUE, log_dilution = TRUE, log_mfi = TRUE, scale_mfi = TRUE, mfi_min = NULL, mfi_max = NULL) {
      stopifnot(is.character(analyte) && !is.null(analyte) && nchar(analyte) > 0)
      stopifnot(length(dilutions) == length(mfi))
      stopifnot(all((dilutions > 0) & (dilutions < 1)))
      stopifnot(all(mfi > 0) & mfi_min >= 0)

      self$analyte <- analyte
      self$dilutions <- dilutions
      self$log_mfi <- log_mfi
      self$scale_mfi <- scale_mfi
      self$log_dilution <- log_dilution


      if (!is.null(mfi_min)) {
        self$mfi_min <- ifelse(self$log_mfi, log10(mfi_min), mfi_min)
      }
      if (!is.null(mfi_max)) {
        self$mfi_max <- ifelse(self$log_mfi, log10(mfi_max), mfi_max)
      }

      number_of_samples <- length(dilutions)
      if (number_of_samples < 5) {
        verbose_cat(
          "(", color_codes$red_start, "WARNING", color_codes$red_end, ")\n",
          "Using less than five samples to fit the logistic model. For now, using the basic nplr method to fit the logistic model - should be modified in the future",
          verbose = verbose
        )
        npars <- min(npars, number_of_samples)
      }

      mfi <- private$mfi_fit_transform(mfi)
      self$model <- nplr::nplr(
        x = dilutions,
        y = mfi,
        npars = npars,
        silent = !verbose,
        useLog = log_dilution
      )
    },

    #' @description
    #' Predict RAU values from the MFI values
    #'
    #' @param mfi (`numeric()`)\cr
    #' MFI values for which we want to predict the RAU values
    #'
    #' @param over_max_extrapolation (`numeric(1)`)\cr
    #' How much we can extrapolate the values above the maximum RAU value
    #' seen in standard curve samples \eqn{\text{RAU}_{max}}. Defaults to 0.
    #' If the value of the predicted RAU is above \eqn{RAU_{max} + \text{over\_max\_extrapolation}},
    #' the value is censored to the value of that sum.
    #' @param eps (`numeric(1)`)\cr
    #' A small value used to avoid numerical issues close to the asymptotes
    #'
    #' @return (`data.frame()`)\cr
    #' Dataframe with the predicted RAU values for given MFI values
    #' The columns are named as follows:
    #' - `RAU` - the Relative Antibody Units (RAU) value
    #' - `MFI` - the predicted MFI value
    #'
    predict = function(mfi, over_max_extrapolation = 0, eps = 1e-6) {
      private$assert_model_fitted()
      original_mfi <- mfi
      # Extrapolation maximum
      max_sc_rau <- max(dilution_to_rau(self$dilutions), na.rm = TRUE)
      rau_threshold <- max_sc_rau + over_max_extrapolation
      top_asymptote_transformed <- private$mfi_transform(self$top_asymptote - eps)
      rau_at_top_asymptote <- nplr::getEstimates(self$model,
        targets = top_asymptote_transformed
      )[1, "x"]
      if (rau_threshold >= rau_at_top_asymptote) {
        warning(
          "Extrapolation above the top asymptote is not allowed.
          Samples with MFI values above the top asymptote will be censored to the top asymptote.
          Consider using a smaller value for `over_max_extrapolation`."
        )
      }
      # Handle mfi outside asymptotes
      mfi <- clamp(mfi,
        lower = self$bottom_asymptote + eps,
        upper = self$top_asymptote - eps
      )
      mfi <- private$mfi_transform(mfi)
      # Example columns: y, x,

      # As we do not use the confidence intervals, we can
      # set the number of additional evaluations to 0
      # to speed up the computation by setting B = 0.
      # Same with `get_plot_data` method
      df <- nplr::getEstimates(self$model, mfi, B = 0)
      df <- df[, c("x", "y")]
      # nprl automatically scales the x to non log scale
      df[, "y"] <- original_mfi
      # Convert to RAU
      df[, "x"] <- dilution_to_rau(df[, "x"])
      # Censor values or extrapolate
      df[, "x"] <- ifelse(df[, "x"] > rau_threshold, rau_threshold, df[, "x"])
      # Rename columns before returning
      colnames(df) <- sub("x", "RAU", colnames(df))
      colnames(df) <- sub("y", "MFI", colnames(df))
      df
    },

    #' @description
    #' Data that can be used to plot the standard curve.
    #'
    #' @return (`data.frame()`)\cr
    #' Prediction dataframe for scaled MFI (or logMFI) values in the range \[0, 1\].
    #' Columns are named as in the `predict` method
    get_plot_data = function() {
      private$assert_model_fitted()
      upper_bound <- nplr::getPar(self$model)$params$top
      upper_bound <- (upper_bound + 1) / 2
      lower_bound <- private$mfi_transform(10) # Scaled MFI for MFI = 10

      uniform_targets <- seq(lower_bound, upper_bound, length.out = 100)
      df <- nplr::getEstimates(self$model, targets = uniform_targets, B = 0)
      df[, "y"] <- private$mfi_reverse_transform(df[, "y"])
      cols <- grepl("^x", colnames(df))
      df[, cols] <- dilution_to_rau(df[, cols])

      colnames(df) <- sub("^x", "RAU", colnames(df))
      colnames(df) <- sub("^y", "MFI", colnames(df))
      df
    },

    #' @description
    #' Function prints the basic information about the model
    #' such as the number of parameters or samples used
    print = function() {
      cat(
        "Instance of the Model class fitted for analyte '", self$analyte, "': \n",
        "- fitted with", nplr::getPar(self$model)$npar, "parameters\n",
        "- using", length(nplr::getX(self$model)), "samples\n",
        "- using log residuals (mfi): ", self$log_mfi, "\n",
        "- using log dilution: ", self$log_dilution, "\n",
        "- top asymptote:", self$top_asymptote, "\n",
        "- bottom asymptote:", self$bottom_asymptote, "\n",
        "- goodness of fit:", nplr::getGoodness(self$model)$gof, "\n",
        "- weighted goodness of fit:", nplr::getGoodness(self$model)$wgof, "\n"
      )
    }
  ),
  active = list(
    #' @field top_asymptote (`numeric(1)`)\cr
    #' The top asymptote of the logistic curve
    top_asymptote = function() {
      private$assert_model_fitted()
      asymptote <- nplr::getPar(self$model)$params$top
      private$mfi_reverse_transform(asymptote)
    },

    #' @field bottom_asymptote (`numeric(1)`)\cr
    #' The bottom asymptote of the logistic curve
    bottom_asymptote = function() {
      private$assert_model_fitted()
      asymptote <- nplr::getPar(self$model)$params$bottom
      private$mfi_reverse_transform(asymptote)
    }
  ),
  private = list(
    assert_model_fitted = function() {
      if (is.null(self$model)) {
        stop("Model class was not properly initialized. Missing nplr model")
      }
    },
    mfi_fit_transform = function(mfi) {
      if (self$log_mfi) {
        mfi <- log10(mfi)
      }
      if (self$scale_mfi) {
        if (is.null(self$mfi_min)) {
          self$mfi_min <- min(mfi)
        }
        if (is.null(self$mfi_max)) {
          self$mfi_max <- max(mfi)
        }
        stopifnot(self$mfi_max > self$mfi_min)

        mfi <- (mfi - self$mfi_min) / (self$mfi_max - self$mfi_min)
      }
      mfi
    },
    mfi_transform = function(mfi) {
      if (self$log_mfi) {
        mfi <- log10(mfi)
      }
      if (self$scale_mfi) {
        mfi <- (mfi - self$mfi_min) / (self$mfi_max - self$mfi_min)
      }
      mfi
    },
    mfi_reverse_transform = function(mfi) {
      if (self$scale_mfi) {
        mfi <- mfi * (self$mfi_max - self$mfi_min) + self$mfi_min
      }
      if (self$log_mfi) {
        mfi <- 10^mfi
      }
      mfi
    }
  )
)

#' Predict the RAU values from the MFI values
#' @description
#' More details can be found here: \link[PvSTATEM]{Model}
#'
#' @param object (`Model()`)
#'   Object of the Model class
#' @param mfi (`numeric()`)
#'   MFI values for which we want to predict the RAU values
#'   Should be in the same scale as the MFI values used to fit the model
#' @param ... Additional arguments passed to the method
#'
#' @importFrom stats predict
#'
#' @return (`data.frame()`)
#'
#' @export
predict.Model <- function(object, mfi, ...) {
  object$predict(mfi, ...)
}

#' Create a standard curve model for a certain analyte
#'
#' @param plate (`Plate()`)
#'   Object of the Plate class
#' @param analyte_name (`character(1)`)
#'   Name of the analyte for which we want to create the model
#' @param data_type (`character(1)`)
#'   Data type of the value we want to use to fit the model - the same datatype as in the plate file. By default, it equals to `Median`
#' @param source_mfi_range_from_all_analytes (`logical(1)`)
#'   If `TRUE`, the MFI range is calculated from all analytes; if `FALSE`, the MFI range is calculated only for the current analyte
#'   Defaults to `FALSE`
#' @param detect_high_dose_hook (`logical(1)`) If `TRUE`, the high dose hook effect is detected and handled.
#' For more information, please see the \link[PvSTATEM]{handle_high_dose_hook} function documentation.
#' @param ... Additional arguments passed to the model
#'
#' Standard curve samples should not contain `na` values in mfi values nor in dilutions.
#'
#' @return (`Model()`) Standard Curve model
#'
#' @export
create_standard_curve_model_analyte <- function(plate, analyte_name,
                                                data_type = "Median",
                                                source_mfi_range_from_all_analytes = FALSE,
                                                detect_high_dose_hook = TRUE,
                                                ...) {
  mfi <- plate$get_data(analyte_name, "STANDARD CURVE", data_type = data_type)
  dilutions_numeric <- plate$get_dilution_values("STANDARD CURVE")

  if (detect_high_dose_hook) {
    sample_selector <- handle_high_dose_hook(mfi, dilutions_numeric)
    mfi <- mfi[sample_selector]
    dilutions_numeric <- dilutions_numeric[sample_selector]
  }

  mfi_source <- ifelse(source_mfi_range_from_all_analytes, "ALL", analyte_name)
  mfi_min <- min(plate$get_data(mfi_source, "STANDARD CURVE", data_type = data_type), na.rm = TRUE)
  mfi_max <- max(plate$get_data(mfi_source, "STANDARD CURVE", data_type = data_type), na.rm = TRUE)

  Model$new(analyte_name, dilutions_numeric, mfi, mfi_min = mfi_min, mfi_max = mfi_max, ...)
}


#' @title Detect and handle the high dose hook effect
#'
#' @description
#' Typically, the MFI values associated with standard curve
#' samples should decrease as we dilute the samples. However,
#' sometimes in high dilutions, the MFI presents a non monotonic behavior.
#' In that case, MFI values associated with dilutions above (or equal to)
#' `high_dose_threshold` should be removed from the analysis.
#'
#' For more information about this effect please refer to:
#' Namburi, R. P. et. al. (2014) High-dose hook effect.
#  DOI: 10.4103/2277-8632.128412
#'
#' For the `nplr` model the recommended number of standard curve samples
#' is at least 4. If the high dose hook effect is detected but the number
#' of samples below the `high_dose_threshold` is lower than 4,
#' additional warning is printed and the samples are not removed.
#'
#' The function returns a logical vector that can be used to subset the MFI values.
#'
#' @param mfi (`numeric()`)
#' @param dilutions (`numeric()`)
#' @param high_dose_threshold (`numeric(1)`) MFI values associated
#' with dilutions above this threshold should be checked for the high dose hook effect
#'
#' @examples
#' plate_filepath <- system.file(
#'   "extdata", "CovidOISExPONTENT.csv",
#'   package = "PvSTATEM", mustWork = TRUE
#' ) # get the filepath of the csv dataset
#' layout_filepath <- system.file(
#'   "extdata", "CovidOISExPONTENT_layout.xlsx",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' plate <- read_luminex_data(plate_filepath, layout_filepath) # read the data
#'
#' # here we plot the data with observed high dose hook effect
#' plot_standard_curve_analyte(plate, "RBD_omicron")
#'
#' # here we create the model with the high dose hook effect handled
#' model <- create_standard_curve_model_analyte(plate, "RBD_omicron")
#'
#' @return sample selector (`logical()`)
handle_high_dose_hook <- function(mfi, dilutions, high_dose_threshold = 1 / 200) {
  total_samples <- length(mfi)
  correct_order <- order(dilutions, decreasing = TRUE)
  high_dose_hook_samples <- dilutions[correct_order] >= high_dose_threshold
  mfi <- mfi[correct_order]
  if (!is.decreasing(mfi[high_dose_hook_samples])) {
    # High dose hook detected
    if ((total_samples - length(mfi[high_dose_hook_samples])) < 4) {
      warning(
        "High dose hook detected but the number of samples
        below the high dose threshold is lower than 4.
        The samples will not be removed from the analysis."
      )
      return(rep(TRUE, total_samples))
    } else {
      warning(
        "High dose hook detected.
        Removing samples with dilutions above the high dose threshold."
      )
      return((!high_dose_hook_samples)[order(correct_order)]) # Return the initial order
    }
  } else {
    return(rep(TRUE, total_samples))
  }
}
