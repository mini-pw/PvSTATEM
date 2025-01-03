PvSTATEM.env <- new.env(parent = emptyenv())

# MBA formats
PvSTATEM.env$mba_formats <- c("xPONENT", "INTELLIFLEX")

# String patterns for declared MBA formats
PvSTATEM.env$xponent_pattern <- "xpontent|xponent"
PvSTATEM.env$intelliflex_pattern <- "intelliflex"
PvSTATEM.env$mba_pattern <- paste(
  PvSTATEM.env$xponent_pattern,
  PvSTATEM.env$intelliflex_pattern,
  sep = "|"
)

# Normalisation types
PvSTATEM.env$normalisation_types <- c("RAU", "nMFI")

# String patterns for declared normalisation types
PvSTATEM.env$normalisation_pattern <- paste0(
  PvSTATEM.env$normalisation_types,
  collapse = "|"
)
