#' Plot a 96-well plate with colored wells
#'
#' It is a generic function to plot a 96-well plate with colored wells
#' used by other functions in this package, mainly to plot layout and counts.
#' The function uses a background image of a 96-well plate and
#' plots the colors in the wells using ggplot2.
#' This function is not intended to be used directly by the user.
#' Rather, it is used by other functions, specified in this file.
#'
#' @param colors A vector with 96 colors that will be used to color the wells, order is from left to right and top to bottom
#' @param plot_numbers Logical value indicating if the well numbers should be plotted, default is `FALSE`
#' @param numbers An optional vector with 96 numbers, plotted on the wells. Order is from left to right and top to bottom, must have the same length as colors. It could be used, for instance, to plot the bead count of each well. Must be provided in case the `plot_numbers` parameters is set to `TRUE`
#' @param plot_title The title of the plot (default is "Plate")
#' @param plot_legend Logical value indicating if the legend should be plotted, default is `FALSE`
#' @param legend_mapping A named vector with the colors mapping used to create the legend
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import grid
#' @import png
#' @importFrom grDevices dev.size
#'
#' @keywords internal
plot_plate <- function(colors, plot_numbers = FALSE, numbers = NULL, plot_title = "Plate",
                       plot_legend = FALSE, legend_mapping = NULL) {
  if (length(colors) != 96) {
    stop("The colors vector must have 96 elements")
  }

  if (plot_numbers && is.null(numbers)) {
    stop("The numbers vector must be provided if plot_numbers is TRUE")
  }

  if (plot_numbers && length(numbers) != 96) {
    stop("The numbers vector must have 96 elements")
  }

  if (is.null(legend_mapping)) {
    stop("The legend_mapping vector must always be provided")
  }

  if (length(legend_mapping) < length(unique(colors))) {
    stop("The legend_mapping vector must have at least the same length as the unique colors")
  }

  # Load the background image
  image_path <- system.file("img", "96_well_plate.png", package = "PvSTATEM", mustWork = TRUE)
  plate_img <- readPNG(image_path)

  # values obtained using trial and error
  well_positions <- expand.grid(
    x = seq(0.075, 0.927, length.out = 12),
    y = seq(0.904, 0.095, length.out = 8)
  )

  # Add colors to the well positions data frame
  well_positions$color <- colors
  well_positions$numbers <- numbers

  # Define the aspect ratio of the background image
  background_image_resolution <- c(dim(plate_img)[2], dim(plate_img)[1])
  aspect_ratio <- background_image_resolution[2] / background_image_resolution[1]
  # A size unit relative to the device width in mm
  runit <- 10 * dev.size("cm")[1] / 1000

  categories <- names(legend_mapping)
  well_positions$category <- factor(well_positions$color, levels = legend_mapping, labels = categories)

  # Plot the plate with colored wells
  p <- ggplot(well_positions, aes(x = .data$x, y = .data$y, fill = .data$category)) +
    geom_tile(key_glyph = "point") +
    annotation_custom(
      rasterGrob(plate_img, width = unit(1, "npc"), height = unit(1, "npc")),
      -Inf, Inf, -Inf, Inf
    ) +
    theme_void() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = legend_mapping) +
    ggtitle(plot_title) +
    theme(
      aspect.ratio = aspect_ratio,
      plot.title = element_text(hjust = 0.5, size = 100 * runit, vjust = -1),
      legend.title = element_text(size = 0),
      legend.text = element_text(size = 12, face = "bold"),
      legend.background = element_rect(fill = "white", linewidth = 0),
      legend.key = element_rect(fill = "white", color = "white"),
      legend.margin = margin(c(5, 5, 5, 5))
    ) +
    guides(fill = guide_legend(title = NULL, override.aes = list(size = 6, shape = 21, stroke = 1, color = "black")))

  if ((dev.size("px") / background_image_resolution)[1] < (dev.size("px") / background_image_resolution)[2]) {
    p <- p + theme(legend.position = "bottom")
  }

  if (plot_numbers) {
    p <- p + geom_text(
      aes(label = numbers),
      size = 15 * runit, color = "black", vjust = 0.5, hjust = 0.5, fontface = "bold"
    )
  }

  if (!plot_legend) {
    p <- p + theme(legend.position = "none")
  }

  p
}


#' Plot counts in a 96-well plate
#'
#' This is a function used to plot counts in a 96-well plate using a color to represent the count ranges.
#' There is possibility to plot exact counts in each well. \cr \cr
#' If plot window is resized, it's best to re-run the function to adjust the scaling.
#' Sometimes when legend is plotted, whole layout may be shifted, then it's best to stretch the window, and everything will be adjusted automatically.
#'
#' @param plate The plate object with the counts data
#' @param analyte_name The name of the analyte
#' @param plot_counts Logical indicating if the counts should be plotted
#' @param plot_legend Logical indicating if the legend should be plotted
#' @param lower_threshold The lower threshold for the counts, it separates green and yellow colors
#' @param higher_threshold The higher threshold for the counts, it separates yellow and red colors
#'
#' @return A ggplot object
#'
#' @examples
#' plate_filepath <- system.file("extdata", "CovidOISExPONTENT_CO.csv",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' layout_filepath <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' plate <- read_luminex_data(plate_filepath, layout_filepath)
#' plot_counts(
#'   plate = plate, analyte_name = "OC43_NP_NA",
#'   plot_counts = TRUE, plot_legend = FALSE
#' )
#'
#' @export
plot_counts <- function(plate, analyte_name, plot_counts = TRUE, plot_legend = FALSE, lower_threshold = 50, higher_threshold = 70) {
  if (is.null(plate)) {
    stop("The plate object must be provided")
  }

  if (is.null(plate$data)) {
    stop("The plate object must have data")
  }

  if (is.null(analyte_name)) {
    stop("The analyte_name must be provided")
  }

  if (lower_threshold > higher_threshold) {
    stop("lower threshold cannot be higher than higher threshold")
  }

  counts <- plate$get_data(analyte_name, data_type = "Count")
  location <- plate$sample_locations
  counts <- create_vector_without_holes(counts, location)

  if (length(counts) != 96) {
    stop("The counts vector must have 96 elements")
  }

  # Define the mapping
  color_map <- c(
    "TO LITTLE" = "#cc3232",
    "WARNING" = "#e5e50f",
    "CORRECT" = "#2dc937",
    " " = "white" # default for missing values,
    # it is a space because otherwise "missing" would be included in legend
  )

  # mapping function from counts to colors
  map_to_color <- function(count, lower_threshold, higher_threshold) {
    count <- as.integer(ifelse(count == " ", -1, count))
    if (count < 0) {
      return(color_map[" "])
    }

    if (count < lower_threshold) {
      return(color_map["TO LITTLE"])
    } else if (count >= lower_threshold && count <= higher_threshold) {
      return(color_map["WARNING"])
    } else {
      return(color_map["CORRECT"])
    }
  }


  # Apply the mapping function to the counts vector
  colors <- sapply(counts, map_to_color, lower_threshold = lower_threshold, higher_threshold = higher_threshold)
  title <- paste("Counts for", analyte_name)

  plot_plate(colors, plot_title = title, plot_numbers = plot_counts, numbers = counts, plot_legend = plot_legend, legend_mapping = color_map)
}


#' @title
#' Plot layout of a 96-well plate
#'
#' @description
#' This is a function used to plot the layout of a 96-well plate using a color to represent the sample types. \cr \cr
#' If plot window is resized, it's best to re-run the function to adjust the scaling.
#' Sometimes when legend is plotted, whole layout may be shifted, then it's best to stretch the window, and everything will be adjusted automatically.
#'
#'
#' @param plate The plate object with the layout information
#' @param plot_legend Logical indicating if the legend should be plotted
#'
#' @return A ggplot object
#'
#' @examples
#' plate_filepath <- system.file("extdata", "CovidOISExPONTENT_CO.csv",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' layout_filepath <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' plate <- read_luminex_data(plate_filepath, layout_filepath)
#' plot_layout(plate = plate, plot_legend = TRUE)
#'
#' @export
plot_layout <- function(plate, plot_legend = TRUE) {
  if (is.null(plate)) {
    stop("The plate object must be provided")
  }

  plate_name <- plate$plate_name
  sample_types <- plate$sample_types
  location <- plate$sample_locations
  sample_types <- create_vector_without_holes(sample_types, location)

  if (length(sample_types) != 96) {
    stop("The sample_types vector must have 96 elements")
  }

  # Define the mapping using a named vector
  color_map <- c(
    "BLANK" = "#B5CFB7",
    "POSITIVE CONTROL" = "#B1AFFF",
    "NEGATIVE CONTROL" = "#FFE9D0",
    "TEST" = "#BBE9FF",
    "STANDARD CURVE" = "#F7B5CA",
    " " = "white" # default for missing values,
    # it is a space because otherwise "missing" would be included in legend
  )

  # Mapping function using the named vector
  map_to_color <- function(sample_type) {
    if (!is.null(color_map[sample_type])) {
      return(color_map[sample_type])
    } else {
      return(color_map[" "])
    }
  }

  # Apply the mapping function to the sample_types vector
  colors <- sapply(sample_types, map_to_color)

  title <- paste("Layout of", plate_name)

  plot_plate(colors, plot_title = title, plot_numbers = FALSE, plot_legend = plot_legend, legend_mapping = color_map)
}

#' @title Remove holes from a vector
#' @description
#' Function selects the values from the vector at the given locations
#' and creates a vector "without holes". Works only for 96-plate.
#' @param vector A vector with values
#' @param locations A vector with locations where the values should be placed
#'
#' @return A vector with values at the given locations
#'
#' @keywords internal
create_vector_without_holes <- function(vector, locations) {
  # Create a vector with all the locations
  rows <- rep(LETTERS[1:8], each = 12)
  columns <- as.character(1:12)
  all_locations <- paste0(rows, columns)

  # Create a vector with all the locations and set the missing values
  without_holes <- rep(" ", length(all_locations))
  names(without_holes) <- all_locations

  # Update the present positions with the corresponding values
  without_holes[locations] <- vector

  # Output vector
  without_holes <- unname(without_holes)
  without_holes
}
