#' Logisic regresion model for the standard curve
#'
#' @param dilutions Diltuions used to fit the model
#' @param mfi MFI values used to fit the model
#' @param npars Number of parameters to use in the model
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#' @param log_dilution If `TRUE` the dilutions are transformed using the `log10` function, `TRUE` by default
#' @param log_mfi If `TRUE` the MFI values are transformed using the `log10` function, `TRUE` by default
#' @param scale_mfi If `TRUE` the MFI values are scaled to the range [0, 1], `TRUE` by default
#'
#' @description
#' This function uses the `nplr` package to fit the model. The model is fitted using the formula:
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
#' This equation is refereed as the Richards' equation. More information about the model can be found in the `nplr` package documentation.
#'
#' @import nplr
#' @import dplyr
#'
SCModel <- R6::R6Class(
  "SCModel",
  public = list(
    dilutions = NULL,
    mfi = NULL,
    mfi_min = NULL,
    mfi_max = NULL,
    model = NULL,
    log_dilution = TRUE,
    log_mfi = TRUE,
    scale_mfi = TRUE,
    # ------------
    initialize = function(dilutions, mfi, npars = 5, verbose = TRUE, log_dilution = TRUE, log_mfi = TRUE, scale_mfi = TRUE) {
      stopifnot(length(dilutions) == length(mfi))
      stopifnot(all((dilutions > 0) & (dilutions < 1)))
      stopifnot(all(mfi > 0))

      self$log_mfi <- log_mfi
      self$scale_mfi <- scale_mfi
      self$log_dilution <- log_dilution

      number_of_samples <- length(dilutions)
      if (number_of_samples < 5) {
        verbose_cat(
          "(", color_codes$red_start, "WARNING", color_codes$red_end, ")\n",
          "Using less than 5 samples to fit logistic model. For now using the basic nplr method to fit the logistic model - should be modified in the future",
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
    #'
    predict = function(mfi) {
      if (is.null(self$model)) {
        stop("SCModel class was not properly initialized. Missing model")
      }
      original_mfi <- mfi
      mfi <- private$mfi_transform(mfi)

      # Example columns: y, x.025, x, x.975
      df <- nplr::getEstimates(self$model, mfi)
      # nprl automatically scales the x to non log scale
      df[, "y"] <- original_mfi

      colnames(df) <- sub("^x", "dilution", colnames(df))
      colnames(df) <- sub("^y", "mfi", colnames(df))
      df
    },
    plot_data = function() {
      if (is.null(self$model)) {
        stop("SCModel class was not properly initialized. Missing model")
      }
      targets <- seq(.99, .01, by = -0.1)
      df <- nplr::getEstimates(self$model, targets)
      df[, "y"] <- private$mfi_reverse_transform(df[, "y"])
      colnames(df) <- sub("^x", "dilution", colnames(df))
      colnames(df) <- sub("^y", "mfi", colnames(df))
      df
    },

    #'
    top_asymptote = function() {
      if (is.null(self$model)) {
        stop("SCModel class was not properly initialized. Missing model")
      }
      asymptote <- nplr::getPar(self$model)$params$top
      private$mfi_reverse_transform(asymptote)
    },
    #'
    bottom_asymptote = function() {
      if (is.null(self$model)) {
        stop("SCModel class was not properly initialized. Missing model")
      }
      asymptote <- nplr::getPar(self$model)$params$bottom
      private$mfi_reverse_transform(asymptote)
    }
  ),
  private = list(
    mfi_fit_transform = function(mfi) {
      if (self$log_mfi) {
        mfi <- log(mfi, base = 10)
      }
      if (self$scale_mfi) {
        self$mfi_min <- min(mfi)
        self$mfi_max <- max(mfi)
        mfi <- (mfi - self$mfi_min) / (self$mfi_max - self$mfi_min)
      }
      mfi
    },
    mfi_transform = function(mfi) {
      if (self$log_mfi) {
        mfi <- log(mfi, base = 10)
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
    }
  )
)

#' Predict the dilutions from the MFI values
#'
#' @param mfi MFI values for which we want to predict the dilutions.
#' Should be in the same scale as the MFI values used to fit the model
#'
#' @export
predict.SCModel <- function(object, mfi) {
  object$predict(mfi)
}

#' Create standard curve model for a certain analyte
#'
#' @param plate Plate object
#' @param analyte_name Name of the analyte for which we want to create the model
#' @param data_type Data type of the value we want to use to fit the model - the same datatype as in the plate file. By default equals to `Median`
#' @param ... Additional arguments passed to the model
#'
#' @return Standard Curve model
#'
#' @export
create_standard_curve_model_analyte <- function(plate, analyte_name, data_type = "Median", ...) {
  # Create a dataframe with the data
  mfi <- plate$get_data(analyte_name, "STANDARD CURVE", data_type = data_type)
  dilutions_numeric <- plate$get_dilution_values("STANDARD CURVE")
  SCModel$new(dilutions_numeric, mfi, ...)
}
