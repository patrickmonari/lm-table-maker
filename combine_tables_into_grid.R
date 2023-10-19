# Ensure required packages are loaded
library(cowplot)
library(png)

combine_tables_into_grid <- function(png_files, output_filename = "combined_plots.png") {
  
  # Read in each PNG file and convert it to a grob
  plots <- lapply(png_files, function(file) {
    img <- png::readPNG(file)
    grob <- grid::rasterGrob(img, interpolate=TRUE)
    return(grob)
  })
  
  # Get the number of PNG files
  num_plots <- length(plots)
  
  
  # Prompt the user to specify image width or use the default value of 16
  cat("Enter the image width (or press enter to use default value of 16): ")
  user_width <- readline()
  if (user_width == "") {
    width <- 16
  } else {
    width <- as.numeric(user_width)
  }
  
  # Prompt the user to specify individual plot height or use the default value of 2.5
  cat("Enter the plot height (or press enter to use default value of 2.5): ")
  user_height <- readline()
  if (user_height == "") {
    height <- 2.5
  } else {
    height <- as.numeric(user_height)
  }
  
  # Prompt the user to specify label size
  cat("Enter the label size (or press enter to use default value of 32): ")
  user_label_size <- readline()
  if (user_label_size == "") {
    label_size <- 32
  } else {
    label_size <- as.numeric(user_label_size)
  }
  
  # Dynamically determine base width, height, and label size based on the number of plots
  base_width <- width
  base_height_per_plot <- height
  base_height <- height * num_plots
  label_size <- label_size
  
  # Combine the grobs into a grid
  combined_plot <- cowplot::plot_grid(
    plotlist = plots, 
    labels = "AUTO", 
    nrow = num_plots, 
    label_size = label_size,
    label_x = 0,    # Position at the left
    label_y = 1     # Position at the top
  )
  
  # Save the combined plot to a PNG file
  cowplot::save_plot(output_filename, combined_plot, base_width = base_width, base_height = base_height)
  
  cat(sprintf("Combined plots saved to %s!\n", output_filename))
}

# Usage example
# png_files <- c("lmer_table1.png", "lmer_table2.png", "lmer_table3.png")
# combine_tables_into_grid(png_files)
