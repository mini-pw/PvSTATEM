#' Plot MFI value distribution for a given analyte
#'
#' @param plate A plate object
#' @param analyte The analyte to plot
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
plot_mfi_for_analyte <- function(plate, analyte,
                                 data_type = "Median", plot_type = "boxplot") {
  if (!(analyte %in% plate$analyte_names)) {
    stop("Analyte not found in the plate")
  }
  if (!is_valid_data_type(data_type)) {
    stop("Datatype not supported.")
  }

  main_geom <- switch(plot_type,
    "boxplot" = ggplot2::geom_boxplot,
    "violin" = ggplot2::geom_violin,
    {
      stop("Plot type not supported. Use either 'boxplot' or 'violin'")
    }
  )

  df <- plate$data[[data_type]]
  df %>%
    dplyr::select(analyte) %>%
    dplyr::rename("MFI" = analyte) %>%
    dplyr::mutate(
      SampleId = paste0("SampleId: ", seq_len(nrow(.))),
      SampleTypes = plate$sample_types,
    ) %>%
    dplyr::group_by(SampleTypes) %>%
    dplyr::mutate(
      outlier = ifelse(is_outlier(MFI), SampleId, as.character(NA))
    ) %>%
    ggplot2::ggplot(aes(x = SampleTypes, y = MFI)) +
    main_geom() +
    ggplot2::geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.1) +
    ggplot2::xlab("Sample Types") +
    ggplot2::ylab("MFI (Median)") +
    ggplot2::ggtitle(
      paste0("Boxplot of MFI per sample type for analyte: ", analyte)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5), # Center title
      axis.title.x = element_text(margin = margin(t = 10)) # Adjust x-axis title
    )
}
