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
  
  # Dynamically determine base width, height, and label size based on the number of plots
  base_width <- 14
  base_height_per_plot <- 2.5 # height for each plot, adjust if needed
  base_height <- base_height_per_plot * num_plots
  label_size <- 32 # adjusted so that label size decreases as the number of plots increases
  
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
