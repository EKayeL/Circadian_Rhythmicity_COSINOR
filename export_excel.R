# Loading Necessary Libraries
library(openxlsx)
library(here)

#' Export Grob Tables to Excel
#'
#' This function exports grob tables stored in the global environment to an Excel file. It identifies
#' grob objects based on specific naming patterns and writes their data into individual sheets in the workbook.
#'
#' @param file_abbrev A string used to filter grob objects in the global environment. 
#'   It matches objects with names starting with "LD_" or "rLEN_", followed by the specified abbreviation and ending with "_grob".
#' @return An Excel file containing the exported grob tables, saved in the current script's directory with a name 
#'   formatted as "combined_<file_abbrev>_results.xlsx".
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom here here
#' @export
#' # Example usage: Export all tables with a specific file abbreviation
## file_abbrev <- "CBmal1"  # Replace with the specific file abbreviation you are working with
## export_excel(file_abbrev)


export_excel <- function(file_abbrev) {
  # Use pattern matching to find all grob objects in the environment
  pattern_results <- paste0("(LD_|rLEN_)", file_abbrev[1], "_Results_table")
  pattern_amp_test <- paste0("(LD_|rLEN_)", file_abbrev[1], "_amp_test_Results")
  
  # List objects matching the patterns
  object_names_results <- ls(pattern = pattern_results, envir = .GlobalEnv)
  object_names_amp_test <- ls(pattern = pattern_amp_test, envir = .GlobalEnv)
  
  # Combine the object names
  object_names <- c(object_names_results, object_names_amp_test)
  
  # Debug: Print the names of the objects found
  print("Objects found:")
  print(object_names)
  
  # Check if object_names is empty
  if (length(object_names) == 0) {
    message("No grob objects found with the specified naming pattern.")
    return(NULL)
  }
  
  # Generate sheet names based on the grob object names, including the prefix
  sheet_names <- sub("^(LD_|rLEN_)", "\\1", object_names)
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Iterate over the object names and sheet names
  for (i in seq_along(object_names)) {
    # Use get() to access each object by name
    grob_object <- get(object_names[i], envir = .GlobalEnv)
    
    # Add worksheet and write data
    addWorksheet(wb, sheet_names[i])
    writeData(wb, sheet_names[i], grob_object)
  }
  
  # Define the directory for Excel exports
  export_dir <- here("XLSX Exports")
  
  # Create the directory if it does not exist
  if (!dir.exists(export_dir)) dir.create(export_dir)
  
  # Define the file path within the export directory
  file_prefix <- ifelse(length(file_abbrev) > 1 && file_abbrev[2] == "duplicated", "duplicated_", "")
  file_path <- file.path(export_dir, paste0(file_prefix, "combined_", file_abbrev[1], "_results.xlsx"))
  
  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
}