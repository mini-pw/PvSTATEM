# Install and load necessary packages
install.packages("ggplot2")
install.packages("grid")
install.packages("png") # or "jpeg" if the image is in JPEG format


library(ggplot2)
library(grid)
library(png)

# Load the background image
image_file <- "/Users/mat/Desktop/new_thesis_bc_i_suck/PvSTATEM/R/96_well_plate.png"
plate_img <- readPNG(image_file)

rgb_image <- array(0, dim = c(dim(plate_img)[1], dim(plate_img)[2], 3))

# Fill each channel with the grayscale values
rgb_image[,,1] <- plate_img[,,1]  # Red channel
rgb_image[,,2] <- plate_img[,,1]  # Green channel
rgb_image[,,3] <- plate_img[,,1]  # Blue channel

random_pastel_color <- function(n) {
  rgb_matrix <- matrix(runif(n * 3, min = 0.6, max = 1), ncol = 3)
  apply(rgb_matrix, 1, function(rgb) {
    rgb(rgb[1], rgb[2], rgb[3])
  })
}


well_positions <- expand.grid(
  x = seq(0.075, 0.927, length.out = 12),
  y = seq(0.095, 0.904, length.out = 8)
)

# Generate random pastel colors for each well
colors <- random_pastel_color(nrow(well_positions))

# Add colors to the well positions data frame
well_positions$color <- colors

# Define the aspect ratio of the background image
background_image_resolution <- c(906, 607)
aspect_ratio <- background_image_resolution[2] / background_image_resolution[1]

# Plot the plate with colored wells
ggplot(well_positions, aes(x = x, y = y)) +
  annotation_custom(
    rasterGrob(rgb_image, width = unit(1, "npc"), height = unit(1, "npc")),
    -Inf, Inf, -Inf, Inf
  ) +
  geom_point(aes(color = color), size = min(dev.size("px") / background_image_resolution) * 19.7 - 2, shape = 21, fill = colors, stroke = 0.5) +
  scale_color_identity() +
  theme_void() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(aspect.ratio = aspect_ratio, plot.title = element_text(hjust = 0.5)) +
  ggtitle("Plate Layout")

