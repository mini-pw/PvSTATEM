require(ggplot2)
require(grid)
require(png)


#' Plot a 96-well plate with colored wells
#'
#' It is a generic function to plot a 96-well plate with colored wells
#' used by other functions in this package, mainly to plot layout and counts.
#' The function uses a background image of a 96-well plate and
#' plots the colors in the wells using ggplot2.
#'
#' @param colors A vector with 96 colors
#' @param plot_numbers Logical indicating if the well numbers should be plotted
#' @param numbers A vector with 96 numbers
#' @param plot_title The title of the plot (default is "Plate")
#' @param plot_legend Logical indicating if the legend should be plotted
#' @param legend_mapping A named vector with the colors mapping
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import grid
#' @import png
#'
#' @internal
plot_plate <- function(colors, plot_numbers = FALSE, numbers = NULL, plot_title = "Plate",
                       plot_legend = FALSE, legend_mapping = NULL) {

  if (length(colors) != 96) {
    stop("The colors vector must have 96 elements")
  }

  if (plot_numbers && is.null(numbers))  {
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
  rgb_image <- array(0, dim = c(dim(plate_img)[1], dim(plate_img)[2], 3))

  # this step is necessary because the image is black and white
  # and R works with either RGB or RGBA images
  # so I have to convert it
  rgb_image[,,1] <- plate_img[,,1]  # Red channel
  rgb_image[,,2] <- plate_img[,,1]  # Green channel
  rgb_image[,,3] <- plate_img[,,1]  # Blue channel

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
  area_size <- min(dev.size("px") / background_image_resolution)


  categories <- names(legend_mapping)
  well_positions$category <- factor(well_positions$color, levels = legend_mapping, labels = categories)


  # Plot the plate with colored wells
  p <- ggplot(well_positions, aes(x = x, y = y)) +
    annotation_custom(
      rasterGrob(rgb_image, width = unit(1, "npc"), height = unit(1, "npc")),
      -Inf, Inf, -Inf, Inf
    ) +
    geom_point(aes(fill = category), size = area_size * 19 - 1, shape = 21, color = "black", stroke = 0) +
    scale_fill_manual(values = legend_mapping) +
    theme_void() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    ggtitle(plot_title) +
    theme(
      aspect.ratio = aspect_ratio,
      plot.title = element_text(hjust = 0.5, size = area_size * 20 + 5, vjust = -1),
      legend.title = element_text(size = 0),
      legend.text = element_text(size = 12, face = "bold"),
      legend.background = element_rect(fill = "white", linewidth = 0),
      legend.key = element_rect(fill = "white", color = "white")
    )

  if ((dev.size("px") / background_image_resolution)[1] < (dev.size("px") / background_image_resolution)[2]){
    p <- p + theme(legend.position = "bottom")
  }

  if (plot_numbers) {
    p <- p + geom_text(aes(label = numbers), size = area_size * 8 - 0.5, color = "black", vjust = 0.5, hjust = 0.5, fontface = "bold")
  }

  if (!plot_legend) {
    p <- p + theme(legend.position = "none")
  }

  p
}


#' Plot counts in a 96-well plate
#'
#' It is a function to plot counts in a 96-well plate using a color scale.
#' The function uses a color scale to map the counts to colors and plot them
#' in a 96-well plate.
#' The function uses the plot_plate function to plot the counts.
#'
#' @param plate The plate object with the counts data
#' @param analyte_name The name of the analyte
#' @param plot_counts Logical indicating if the counts should be plotted
#' @param plot_legend Logical indicating if the legend should be plotted
#'
#' @return A ggplot object
#'
#' @examples
#' plate_filepath <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM", mustWork = TRUE)
#' layout_filepath <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
#' plate <- read_luminex_data(plate_filepath, layout_filepath)
#' plot_counts(plate = plate, analyte_name = "OC43_NP_NA", plot_counts = T, plot_legend = F)
#'
#' @export
plot_counts <- function(plate, analyte_name, plot_counts = TRUE, plot_legend = FALSE) {

  if (is.null(plate)) {
    stop("The plate object must be provided")
  }

  if (is.null(plate$data)) {
    stop("The plate object must have data")
  }

  if (is.null(analyte_name)){
    stop("The analyte_name must be provided")
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
  map_to_color <- function(count) {
    if (count < 0) {
      return(color_map[" "])
    }

    if (count < 50) {
      return(color_map["TO LITTLE"])
    } else if (count >= 50 && count <= 70) {
      return(color_map["WARNING"])
    } else {
      return(color_map["CORRECT"])
    }
  }


  # Apply the mapping function to the counts vector
  colors <- sapply(counts, map_to_color)
  title <- paste("Counts for", analyte_name)

  plot_plate(colors, plot_title = title, plot_numbers = plot_counts, numbers = counts, plot_legend = plot_legend, legend_mapping = color_map)
}


#' Plot layout of a 96-well plate
#'
#' It is a function to plot the layout of a 96-well plate using a color scale.
#' The function uses the plot_plate function to plot the layout.
#'
#' @param plate The plate object with the layout information
#' @param plot_legend Logical indicating if the legend should be plotted
#'
#' @return A ggplot object
#'
#' @examples
#' plate_filepath <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM", mustWork = TRUE)
#' layout_filepath <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)
#' plate <- read_luminex_data(plate_filepath, layout_filepath)
#' plot_layout(plate = plate, plot_legend = T)
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
    " " = "white"  # default for missing values,
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
