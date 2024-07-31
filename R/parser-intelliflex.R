require(dplyr)

#' Read the Intelliflex format data
#'
#' @param path Path to the INTELLIFLEX file
#'
#' @import dplyr
#'
#' @export
read_intelliflex_format <- function(path) {
  df <- read.csv(path)
  system_end_index <- which(colnames(df) == "PLATE.START")
  sample_end_index <- which(colnames(df) == "TOTAL.EVENTS")

  system_df <- df[1, 1:system_end_index]
  sample_df <- df[, (system_end_index + 1):(sample_end_index)]
  results_df <- df[, (sample_end_index + 1):length(colnames(df))]
  results_df <- results_df[, colSums(is.na(results_df)) < nrow(results_df)]

  analyte_names <- results_df %>%
    select(contains("ANALYTE.NAME")) %>%
    head(1)

  region_names <- stringr::str_extract(colnames(analyte_names), "(.*?).ANALYTE.NAME", group = 1)
  analyte_names <- as.character(analyte_names)
  names(analyte_names) <- region_names

  results_df <- results_df %>%
    select(!(contains("ANALYTE.NAME") | contains("REGION")))

  result_types <- unique(stringr::str_extract(
    colnames(results_df),
    "R\\d+\\.\\.RP\\d+\\.(.*)",
    group = 1
  ))

  result_df_list <- lapply(
    result_types,
    function(result_type) {
      x <- results_df %>%
        select(matches(paste0("R\\d+\\.\\.RP\\d+\\.", result_type)))
      regions <- stringr::str_extract(colnames(x), paste0("(.*?).", result_type), group = 1)
      colnames(x) <- analyte_names[regions]
      x <- cbind(location = sample_df[, "WELL.LOCATION"], x)
      x
    }
  )
  names(result_df_list) <- result_types

  list(
    SystemMetadata = unlist(system_df),
    SampleMetadata = sample_df,
    Results = result_df_list
  )
}
