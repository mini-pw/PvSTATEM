#' @title Logistic regression model for the standard curve
#'
#' @description
#' This model uses the `nplr` package to fit the model. The model is fitted using the formula:
#'
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
    #' Predict the dilutions from the MFI values
    #'
    #' @param mfi (`numeric()`)\cr
    #' MFI values for which we want to predict the dilutions.
    #'
    #' @return (`data.frame()`)\cr
    #' Dataframe with the predicted dilutions, MFI values, and the 97.5% confidence intervals
    #' The columns are named as follows:
    #' - `dilution` - the dilution value
    #' - `dilution.025` - the lower bound of the confidence interval
    #' - `dilution.975` - the upper bound of the confidence interval
    #' - `MFI` - the predicted MFI value
    #'
    predict = function(mfi) {
      private$assert_model_fitted()
      original_mfi <- mfi
      mfi <- private$mfi_transform(mfi)

      # Example columns: y, x.025, x, x.975
      df <- nplr::getEstimates(self$model, mfi)
      # nprl automatically scales the x to non log scale
      df[, "y"] <- original_mfi

      colnames(df) <- sub("^x", "dilution", colnames(df))
      colnames(df) <- sub("^y", "MFI", colnames(df))
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
      lower_bound <- nplr::getPar(self$model)$params$bottom
      bound_width <- upper_bound - lower_bound
      upper_bound <- upper_bound - 0.05 * bound_width
      lower_bound <- private$mfi_transform(1) # Scaled MFI for MFI = 1

      uniform_targets <- seq(lower_bound, upper_bound, length.out = 100)
      df <- nplr::getEstimates(self$model, targets = uniform_targets)
      df[, "y"] <- private$mfi_reverse_transform(df[, "y"])
      colnames(df) <- sub("^x", "dilution", colnames(df))
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

#' Predict the dilutions from the MFI values
#' @description
#' More details can be found here: \link[PvSTATEM]{Model}
#'
#' @param object (`Model()`)
#'   Object of the Model class
#' @param mfi (`numeric()`)
#'   MFI values for which we want to predict the dilutions.
#'   Should be in the same scale as the MFI values used to fit the model
#' @param ... Additional arguments passed to the method
#'
#' @importFrom stats predict
#'
#' @return (`data.frame()`)
#'
#' @export
predict.Model <- function(object, mfi, ...) {
  object$predict(mfi)
}

#' Create a standard curve model for a certain analyte
#'
#' @param plate (`Plate()`)
#'   Object of the Plate class
#' @param analyte_name (`character(1)`)
#'   Name of the analyte for which we want to create the model
#' @param data_type (`character(1)`)
#'   Data type of the value we want to use to fit the model - the same datatype as in the plate file. By default, it equals to `Median`
#'
#' @param source_mfi_range_from_all_analytes (`logical(1)`)
#'   If `TRUE`, the MFI range is calculated from all analytes; if `FALSE`, the MFI range is calculated only for the current analyte
#'   Defaults to `FALSE`
#' @param ... Additional arguments passed to the model
#'
#' @return (`Model()`) Standard Curve model
#'
#' @export
create_standard_curve_model_analyte <- function(plate, analyte_name, data_type = "Median", source_mfi_range_from_all_analytes = FALSE, ...) {
  mfi <- plate$get_data(analyte_name, "STANDARD CURVE", data_type = data_type)
  dilutions_numeric <- plate$get_dilution_values("STANDARD CURVE")

  mfi_source <- ifelse(source_mfi_range_from_all_analytes, "ALL", analyte_name)


  mfi_min <- min(plate$get_data(mfi_source, "STANDARD CURVE", data_type = data_type), na.rm = TRUE)
  mfi_max <- max(plate$get_data(mfi_source, "STANDARD CURVE", data_type = data_type), na.rm = TRUE)

  Model$new(analyte_name, dilutions_numeric, mfi, mfi_min = mfi_min, mfi_max = mfi_max, ...)
}
