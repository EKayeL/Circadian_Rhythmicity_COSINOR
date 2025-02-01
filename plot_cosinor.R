# Loading Necessary Libraries
library(ggplot2)
library(dplyr)

#' Plot Cosinor Fit Function
#'
#' This function extrapolates and plots the cosinor fit model of a dataset.
#'
#' @param cosMod A cosinor model object.
#' @param original_data The dataset you wish to plot.
#' @param plot_title What you wish the plot to be titled.
#' @param data_type What type of light exposure the sample is (i.e. LD vs rLEN).
#' @param x_breaks How you want your x-axis dataticks to be arranged (ZT vs reg).
#' @param y_limits Optional vector of length 2 specifying the limits of the y-axis.
#' @return A plot of the original dataset with the cosinor fit model overlayed
#' on top.
#' @examples
#' # Example usage:
#' @export

plot_cosinor <- function(cosMod, original_data, plot_title, data_type,
                         x_breaks = seq(0, 24, by = 6), y_limits = NULL) {
  # Compute fitted values
  fitted_values <- cosMod$fit$fitted.values
  
  # Combine original data with fitted values
  plot_data <- original_data %>%
    mutate(Fitted = fitted_values)
  
  # Find the peak fitted value and its corresponding ZT
  peak_value <- max(fitted_values, na.rm = TRUE)
  peak_ZT <- plot_data$ZT[which(fitted_values == peak_value)][1]
  
  # Create dynamic variable name
  peak_ZT_name <- paste0("peak_ZT_", data_type)
  
  # Save peak_ZT as a project space variable with dynamic name
  assign(peak_ZT_name, peak_ZT, envir = .GlobalEnv)
  
  # Set color based on data type
  original_data_color <- ifelse(data_type == "LD", "gray", "red")
  
  # Set line type based on data type
  fit_line_type <- ifelse(data_type == "LD", "solid", "dashed")
  
  # Compute default y_limits if not provided
  if (is.null(y_limits)) {
    y_limits <- range(plot_data$`Concentration(mg)`, plot_data$Fitted, na.rm = TRUE)
  }
  
  # Create the plot
  plot <- ggplot(plot_data, aes(x = `ZT`)) +
    # Scatter plot of original data
    geom_point(aes(y = `Concentration(mg)`, color = "Original Data")) +
    geom_smooth(aes(y = `Fitted`, color = "Cosinor"), size = 1,
                linetype = fit_line_type) +  # Fitted line
    xlab("Zeitgeber Time (ZT)") +
    ylab("Concentration of Drug (mg)") +
    ggtitle(plot_title) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA)) +
    scale_color_manual(name = "Legend",
                       values = c("Original Data" = original_data_color,
                                  "Cosinor" = "black")) +
    # Use coord_cartesian to enforce y-limits
    scale_y_continuous(limits = c(0,y_peak)) +
    scale_x_continuous(limits = c(0, 25), breaks = x_breaks)
  
  return(plot)
}
