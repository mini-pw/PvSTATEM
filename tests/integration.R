devtools::load_all()

data_dir <- "../PvSTATEM_resources/PvSTATEM data"
files <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

correctly_parsed <- 0
for (file in files) {
  print(paste0("Reading file: ", file))
  lines <- readr::read_lines(file)
  out <- parse_luminex_data(1, lines)
}

print(paste0("Parsed files: ", correctly_parsed, " out of ", length(files)))
