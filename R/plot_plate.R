library(ggplot2)
library(grid)
library(png)

# Function to generate random pastel colors
random_pastel_color <- function(n) {
  rgb_matrix <- matrix(runif(n * 3, min = 0.6, max = 1), ncol = 3)
  apply(rgb_matrix, 1, function(rgb) {
    rgb(rgb[1], rgb[2], rgb[3])
  })
}

# Generate random pastel colors for each well
colors <- random_pastel_color(96)

plot_plate <- function(colors, plot_numbers = FALSE, numbers = NULL, plot_title = "Plate") {

  if (length(colors) != 96) {
    stop("The colors vector must have 96 elements")
  }

  if (plot_numbers && is.null(numbers))  {
    stop("The numbers vector must be provided if plot_numbers is TRUE")
  }

  if (plot_numbers && length(numbers) != 96) {
    stop("The numbers vector must have 96 elements")
  }

  # Load the background image
  image_path <- system.file("img", "96_well_plate.png", package = "PvSTATEM", mustWork = TRUE)
  plate_img <- readPNG(image_path)
  rgb_image <- array(0, dim = c(dim(plate_img)[1], dim(plate_img)[2], 3))

  # Fill each channel with the grayscale values
  # this step is necessary because the image have two channels
  # this is unusual and R complains about it
  rgb_image[,,1] <- plate_img[,,1]  # Red channel
  rgb_image[,,2] <- plate_img[,,1]  # Green channel
  rgb_image[,,3] <- plate_img[,,1]  # Blue channel

  # values obtained using trial and error
  well_positions <- expand.grid(
    x = seq(0.075, 0.927, length.out = 12),
    y = seq(0.095, 0.904, length.out = 8)
  )

  # Add colors to the well positions data frame
  well_positions$color <- colors
  well_positions$numbers <- numbers

  # Define the aspect ratio of the background image
  background_image_resolution <- c(dim(plate_img)[2], dim(plate_img)[1])
  aspect_ratio <- background_image_resolution[2] / background_image_resolution[1]

  # Plot the plate with colored wells
  p <- ggplot(well_positions, aes(x = x, y = y)) +
    annotation_custom(
      rasterGrob(rgb_image, width = unit(1, "npc"), height = unit(1, "npc")),
      -Inf, Inf, -Inf, Inf
    ) +
    geom_point(aes(color = color), size = min(dev.size("px") / background_image_resolution) * 19.9 - 1.5, shape = 21, fill = colors, stroke = 0.5) +
    scale_color_identity() +
    theme_void() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme(aspect.ratio = aspect_ratio, plot.title = element_text(hjust = 0.5)) +
    ggtitle(plot_title)

  if (plot_numbers) {
    p <- p + geom_text(aes(label = numbers), size = 6, color = "black", vjust = 0.5, hjust = 0.5)
  }

  p
}

plot_plate(colors = colors, plot_numbers = T, 1:96)

