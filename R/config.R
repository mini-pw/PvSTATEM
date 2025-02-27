SerolyzerR.env <- new.env(parent = emptyenv())

# MBA formats
SerolyzerR.env$mba_formats <- c("xPONENT", "INTELLIFLEX")

# String patterns for declared MBA formats
SerolyzerR.env$xponent_pattern <- "xpontent|xponent"
SerolyzerR.env$intelliflex_pattern <- "intelliflex"
SerolyzerR.env$mba_pattern <- paste(
  SerolyzerR.env$xponent_pattern,
  SerolyzerR.env$intelliflex_pattern,
  sep = "|"
)

# Normalisation types
SerolyzerR.env$normalisation_types <- c("RAU", "nMFI")

# String patterns for declared normalisation types
SerolyzerR.env$normalisation_pattern <- paste0(
  SerolyzerR.env$normalisation_types,
  collapse = "|"
)
