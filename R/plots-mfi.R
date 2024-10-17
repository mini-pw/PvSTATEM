#' Plot MFI value distribution for a given analyte
#'
#' @param plate A plate object
#' @param analyte_name The analyte to plot
#' @param data_type The type of data to plot. Default is "Median"
#' @param plot_type The type of plot to generate. Default is "violin".
#' Available options are "boxplot" and "violin".
#' @param scale_y What kind of transformation of the scale to apply.
#' By default MFI is presented in a "log10" scale. Available options are
#' described in the documentation of \link[ggplot2]{scale_y_continuous}
#' under`transform` parameter.
#' @param plot_outliers When using "boxplot" type of a plot
#' one can set this parameter to TRUE and display the names of samples for
#' which MFI falls outside the 1.5 IQR interval
#'
#'
#' @return A ggplot object
#'
#' @import dplyr
#' @import ggplot2
#' @import ggrepel
#'
#' @export
plot_mfi_for_analyte <- function(plate, analyte_name,
                                 data_type = "Median", plot_type = "violin",
                                 scale_y = "log10", plot_outliers = FALSE) {
  if (!(analyte_name %in% plate$analyte_names)) {
    stop("Analyte ", analyte_name, " not found in the plate")
  }
  if (!is_valid_data_type(data_type)) {
    stop("Datatype not supported.")
  }

  main_geom <- switch(plot_type,
    "boxplot" = ggplot2::geom_boxplot,
    "violin" = ggplot2::geom_violin,
    {
      stop("Plot type ", plot_type, "  not supported. Use either 'boxplot' or 'violin'")
    }
  )

  df <- plate$data[[data_type]] %>%
    dplyr::select(analyte_name) %>%
    dplyr::rename("MFI" = analyte_name)

  df <- dplyr::mutate(df,
    SampleId = paste0("SampleId: ", seq_len(nrow(df))),
    SampleType = plate$sample_types,
  )

  blanks_df <- df %>% dplyr::filter(.data$SampleType == "BLANK")
  blank_mean <- mean(blanks_df$MFI)
  sc_df <- df %>% dplyr::filter(.data$SampleType == "STANDARD CURVE")
  max_sc <- max(sc_df$MFI)
  test_df <- df %>%
    dplyr::filter(.data$SampleType == "TEST") %>%
    dplyr::mutate(
      outlier = ifelse(is_outlier(.data$MFI), .data$SampleId, as.character(NA))
    )

  p <- test_df %>%
    ggplot2::ggplot(aes(x = .data$SampleType, y = .data$MFI)) +
    main_geom(color = "blue") +
    ggplot2::geom_hline(
      aes(yintercept = blank_mean, linetype = "BLANK MEAN"),
      color = "dark grey", linewidth = 1
    ) +
    ggplot2::geom_point(data = sc_df, size = 3, color = "red") +
    ggplot2::geom_hline(aes(yintercept = max_sc, linetype = "STANDARD CURVE\nMAX"), size = 0.5, color = "red") +
    ggplot2::scale_linetype_manual(
      name = "Boundaries", values = c("BLANK MEAN" = "dashed", "STANDARD CURVE\nMAX" = "dotted")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Sample Type")) +
    ggplot2::ggtitle(
      paste0(
        "MFI ", stringr::str_to_title(plot_type), " of test sample coverage\n for analyte: ", analyte_name
      )
    ) +
    ggplot2::xlab("Sample Type") +
    ggplot2::ylab(paste0("MFI (", data_type, ") ", scale_y, " scale")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5), # Center title
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_blank()
    ) +
    ggplot2::scale_y_continuous(transform = scale_y)

  if (plot_outliers) {
    if (plot_type == "boxplot") {
      hjust <- rep(NA, nrow(test_df))
      is_out <- !is.na(test_df$outlier)
      hjust[is_out] <- ifelse(seq_len(sum(is_out)) %% 2 == 0, -0.18, 1.18)

      p <- p + ggrepel::geom_text_repel(aes(label = .data$outlier), na.rm = TRUE, hjust = hjust, color = "grey", min.segment.length = 0.3)
    } else if (plot_type == "violin") {
      warning("Outliers are not supported for violin plots. Ignoring the plot_outliers argument.")
    }
  }

  p
}
