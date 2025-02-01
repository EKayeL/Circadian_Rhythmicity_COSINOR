# Loading Necessary Libraries
library(cosinor2)
library(gridExtra)
library(stats)
library(dplyr)

#' Zero Amplitude Test Function
#'
#' This function performs a zero amplitude test on a dataset.
#'
#' @param cosMod A cosinor model object.
#' @param data A dataset for which to perform the zero amplitude test.
#' @param suffix A string suffix to append to output files (optional).
#' @return Results of the zero amplitude test.
#' @examples
#' # Example usage:
#' @importFrom stats coef quantile
#' @export

zero_amp <- function(cosMod, suffix = NULL) {
  # Perform zero-amplitude analysis
  zero_amp_test <- cosinor.detect(cosMod)

  # Helper function to format values into scientific notation if below a certain
  # value threshold
  format_value <- function(value) {
    num_value <- as.numeric(value)
    if (num_value < 0.0001) {
      format(num_value, scientific = TRUE)
    } else {
      format(num_value, scientific = FALSE)
    }
  }
  
  # Extract and Format Coefficients
  ## Extract df1
  df1 <- as.numeric(zero_amp_test[2])
  df1 <- format_value(df1)
  
  ## Extract df2
  df2 <- as.numeric(zero_amp_test[3])
  df2 <- format_value(df2)
  
  ## Extract fvalue
  fval <- as.numeric(zero_amp_test[1])
  fval <- format_value(fval)
  
  ## Extract pvalue
  pval <- as.numeric(zero_amp_test[4])
  pval <- format_value(pval)
  
  # Calculate Standard Deviation and Standard Error
  # Assuming zero_amp_test[5] contains the amplitude values
  #amplitudes <- as.numeric(zero_amp_test[5])
  
  # Compute standard deviation and standard error
  #sd_amplitude <- sd(amplitudes)
  #se_amplitude <- sd_amplitude / sqrt(length(amplitudes))
  
  # Format standard deviation and standard error for display
  #sd_amplitude <- format_value(sd_amplitude)
  #se_amplitude <- format_value(se_amplitude)
  
  # Add code to append a star if pval is less than 0.05
  if (as.numeric(pval) < 0.05) {
    pval <- paste0(pval, " *")
  }
  
  # Create data frame with results
  results <- data.frame(
    Parameter = c(paste0(suffix, " Df1 (ndf, numerator)"),
                  paste0(suffix, " Df2 (ddf, denominator)"),
                  paste0(suffix, " F-value"),
                  paste0(suffix, " P-value")),
    Values = c(df1, df2, fval, pval)
  )

  # Convert the results into a graphical table object
  tableGrob(results) -> results_grob

  # Return both the graphical object and the raw results table
  return(list(results_grob = results_grob, results_table = results))
  }
