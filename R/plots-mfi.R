#' Plot MFI value distribution for a given analyte
#'
#' @param plate A plate object
#' @param analyte_name The analyte to plot
#' @param data_type The type of data to plot. Default is "Median"
#' @param plot_type The type of plot to generate. Default is "boxplot".
#' Available options are "boxplot" and "violin".
#'
#' @return A ggplot object
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
plot_mfi_for_analyte <- function(plate, analyte_name,
                                 data_type = "Median", plot_type = "boxplot") {
  if (!(analyte_name %in% plate$analyte_names)) {
    stop("Analyte ", analyte_name," not found in the plate")
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

  df <- plate$data[["Median"]] %>%
    dplyr::select(analyte_name) %>%
    dplyr::rename("MFI" = analyte_name) %>%
    dplyr::mutate(
      SampleId = paste0("SampleId: ", seq_len(nrow(.))),
      SampleType = plate$sample_types,
    )

  blanks_df <- df %>% dplyr::filter(SampleType == "BLANK")
  blank_mean <- mean(blanks_df$MFI)
  sc_df <- df %>% dplyr::filter(SampleType == "STANDARD CURVE")
  test_df <- df %>% dplyr::filter(SampleType == "TEST")

  p <- test_df %>%
    dplyr::mutate(
      outlier = ifelse(is_outlier(MFI), SampleId, as.character(NA))
    ) %>%
    ggplot2::ggplot(aes(x = SampleType, y = MFI)) +
    main_geom(color = "blue") +
    ggplot2::geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.1) +
    ggplot2::geom_hline(
      aes(yintercept = blank_mean, linetype = "BLANK MEAN"),
      color = "dark grey", linewidth = 1
    ) +
    ggplot2::geom_point(data = sc_df, size = 3, color = "red") +
    ggplot2::scale_linetype_manual(
      name = "Boundaries", values = c("BLANK MEAN" = "dashed")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Sample Type")) +
    ggplot2::ggtitle(
      paste0("MFI Boxplot of test sample coverage\n for analyte: ", analyte_name)
    ) +
    ggplot2::xlab("Sample Type") +
    ggplot2::ylab("MFI (Median)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5) # Center title
    )
  p
}
