# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))
cat("\014")

# Function to set up plot parameters
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, left_margin = 1.2, right_margin = 1.2) {
  # Convert inches to points (1 inch = 72 points)
  width_pt <- width * 72
  height_pt <- height * 72
  
  # Set the plot area size
  par(pin = c(width - left_margin - right_margin, height - top_margin - bottom_margin))
  
  # Set margins in inches
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  
  # Set other plot parameters
  par(family = "sans",
      cex = 1.2,
      cex.axis = 1.2,
      cex.lab = 1.2,
      tck = 0.01,
      lwd = 0.8,
      las = 1,
      mgp = c(3, 0.8, 0))
}

# Function to format labels
format_labels <- function(x) {
  gsub("-", "\uad", format(x, scientific = FALSE, trim = TRUE))
}

# Function to create centered legend
create_centered_legend <- function(labels, line_widths, line_types, y_offset = 0.07, x_intersp = 0.5) {
  plot_info <- par("usr")
  plot_height <- plot_info[4] - plot_info[3]
  plot_width <- plot_info[2] - plot_info[1]
  
  # Calculate legend width
  legend_width <- sum(strwidth(labels, units="user")) + 
    length(labels) * par("csi") * x_intersp
  
  # Center of x-axis
  legend_x <- (plot_info[2] + plot_info[1]) / 2
  
  # Adjustable vertical position
  legend_y <- plot_info[3] - y_offset * plot_height
  
  legend(x = legend_x, y = legend_y,
         legend = labels,
         col = "black",
         lwd = line_widths,
         lty = line_types,
         bty = "n",
         horiz = TRUE,
         cex = 1.2,
         seg.len = 2,
         xpd = TRUE,
         xjust = 0.5,
         x.intersp = x_intersp)
}

# "Slavery's Effect on Rural Population Density in the 300-Mile Sample, 1860"
create_figure_1 <- function() {
  # Set plot dimensions
  plot_width <- 9.2  # inches
  plot_height <- 5.9   # inches
  extra_bottom_margin <- 0.4  # Additional bottom margin in inches
  
  # Set up plot parameters with extra bottom margin
  setup_plot(plot_width, plot_height, bottom_margin = 0.6 + extra_bottom_margin)
  
  # Read data from CSV file
  data <- read.csv("database.csv")
  
  # Filter data for year 1860 and distance within 300 miles
  data <- data[data$year == 1860 & data$distance >= -300 & data$distance <= 300, ]
  
  # Convert columns to numeric, replacing any non-numeric values with NA
  data$distance <- as.numeric(as.character(data$distance))
  data$farmv <- as.numeric(as.character(data$farmv))
  data$area <- as.numeric(as.character(data$area))
  
  # Convert area from square meters to square miles
  data$area_sqmiles <- data$area / 2589988.11  # 1 sq mile = 2,589,988.11 sq meters
  
  # Remove rows with NA or non-positive farmv values
  data <- data[!is.na(data$distance) & !is.na(data$farmv) & data$farmv > 0, ]
  
  # Create the plot
  plot(data$distance, data$farmv,
       type = "n",
       xlab = "Distance from the border",
       ylab = "People per square mile (log scale)",
       xlim = c(-300, 300),
       ylim = c(0.1, 1000),
       log = "y",
       xaxs = "i",
       yaxs = "i",
       axes = FALSE)
  
  # Draw custom axes
  axis(1, at = seq(-300, 300, by = 100), 
       labels = format_labels(seq(-300, 300, by = 100)), 
       lwd = 0, lwd.ticks = 0.5, padj = -0.1, cex.axis = 1.2)
  axis(2, at = c(0.1, 0.1, 1, 10, 100, 1000), 
       labels = c("0.1", "0.1", "1", "10", "100", "1000"), 
       lwd = 0, lwd.ticks = 0.5, padj = 0.4, cex.axis = 1.2)
  
  # Add box
  box(lwd = 0.5)
  
  # Add vertical dashed line at zero
  abline(v = 0, lty = 2, lwd = 0.8)
  
  # Calculate point sizes based on area
  max_point_size <- 1.8  # Maximum point size
  min_point_size <- 0.1  # Minimum point size
  point_sizes <- sqrt(data$area_sqmiles / max(data$area_sqmiles)) * (max_point_size - min_point_size) + min_point_size
  
  # Plot data points with varying size and no outline
  points(data$distance, data$farmv, 
         pch = 19,  # Solid circle with no outline
         col = rgb(0, 0, 0, 0.3),  # Transparent black
         cex = point_sizes)  # Size based on area
  
  # Save the plot as PDF
  dev.copy(pdf, "figure.pdf", width = plot_width, height = plot_height)
  dev.off()
}

# Run the function to create the plot
create_figure_1()