# Loading Necessary Libraries
library(base)
library(stats)
library(dplyr)


# filter_outliers.R

#' Filter Outliers Function
#'
#' This function replaces outliers with NA in each numeric column of a dataset based on the results from `outlier_detection`.
#'
#' @param dataset A data frame with numeric columns.
#' @param outlier_results A list of outlier detection results from `outlier_detection`.
#' @return A data frame with outliers replaced by NA.
#' @examples
#' data <- data.frame(x = rnorm(100), y = runif(100))
#' out_res <- outlier_detection(data)
#' filter_outliers(data, out_res)
#' @export

filter_outliers <- function(dataset, outlier_results) {
  for (col_name in names(outlier_results)) {
    # Get the thresholds for the current column
    threshold_high <- outlier_results[[col_name]]$threshold_high
    threshold_low <- outlier_results[[col_name]]$threshold_low
    
    # Replace outliers with NA in the current column
    dataset[[col_name]] <- ifelse(dataset[[col_name]] > threshold_high | dataset[[col_name]] < threshold_low, NA, dataset[[col_name]])
  }
  return(dataset)
}