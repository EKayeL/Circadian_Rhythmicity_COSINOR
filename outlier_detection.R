# Loading Necessary Libraries
library(tidyr)
library(magrittr)
library(dplyr)

#' Outlier Detection Function
#'
#' This function detects outliers in a dataset using specified methods.
#'
#' @param data A data frame containing the data to be analyzed.
#' @return A data frame with outliers filtered out.
#' @examples
#' # Example usage:
#' data <- data.frame(x = rnorm(100), y = runif(100))
#' out_res <- outlier_detection(data)
#' @importFrom dplyr pull
#' @importFrom tidyr drop_na
#' @importFrom magrittr `%>%`
#' @export

# Define the function to perform outlier detection
outlier_detection <- function(dataset) {
  # Initialize a list to store results
  list() -> results

  # Iterate over each column in the dataset
  for (col_name in colnames(dataset)) {
    data <- dataset %>%
      select(all_of(col_name)) %>%
      drop_na() %>%
      pull()

    # Skip non-numeric columns
    if (!is.numeric(data)) next

    # Calculate the first (Q1) and third (Q3) quartiles
    Q1 <- quantile(data, 0.25, na.rm = TRUE)
    Q3 <- quantile(data, 0.75, na.rm = TRUE)

    # Calculate the Interquartile Range (IQR)
    Q3 - Q1 -> IQR

    # Calculate the thresholds for outliers
    Q3 + 1.5 * IQR -> threshold_high
    Q1 - 1.5 * IQR ->  threshold_low

    # Identify higher outliers
    data[data > threshold_high] -> higher_outliers

    # Identify lower outliers
    data[data < threshold_low] -> lower_outliers

    # Store the results for this column
    results[[col_name]] <- list(
      Q1 = Q1,
      Q3 = Q3,
      IQR = IQR,
      threshold_high = threshold_high,
      threshold_low = threshold_low,
      higher_outliers = higher_outliers,
      lower_outliers = lower_outliers
    )
  }

  return(results)
}
