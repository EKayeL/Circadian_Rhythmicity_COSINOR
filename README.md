Circadian Rhythm Analysis Package

Introduction

This R package is designed to analyze the circadian rhythmicity of location-specific neurocompounds over Zeitgeber Time (ZT). It leverages cosinor models to fit sinusoidal curves to time-series data and provides tools for outlier detection, parameter extraction, zero amplitude testing, plotting, and exporting results to Excel and PDF formats. The package is particularly useful for researchers studying biological rhythms in neurochemical datasets.

Key Features





Outlier Detection and Filtering: Identifies and removes outliers using the Interquartile Range (IQR) method.



Cosinor Model Fitting: Fits sinusoidal models to assess circadian rhythmicity.



Parameter Extraction: Calculates key circadian parameters such as mesor, amplitude, and acrophase.



Zero Amplitude Testing: Tests the significance of circadian rhythm amplitude.



Visualization: Generates plots of original data with cosinor fits overlaid.



Exporting Results: Exports tables to Excel and creates combined PDF reports.

Package Overview

The package includes the following R scripts, each serving a specific purpose:





filter_outliers.R: Replaces outliers with NA in numeric columns based on IQR-based detection.



create_combined_pdf.R: Generates a PDF with a title, headers, and tables in a grid layout.



export_excel.R: Exports graphical table objects (grobs) to an Excel file with multiple sheets.



extract_cosinor.R: Extracts and calculates circadian parameters from cosinor models.



outlier_detection.R: Detects outliers in numeric columns using the IQR method.



plot_cosinor.R: Plots original data with the cosinor fit model overlaid.



zero_amp.R: Performs a zero amplitude test to assess rhythm significance.

Dependencies

The package relies on the following R libraries, which must be installed prior to use:





dplyr, tidyr, magrittr (data manipulation)



ggplot2 (plotting)



grid, gridExtra (graphical layouts and tables)



openxlsx, here (Excel export and file paths)



stats, base (statistical functions)



cosinor2, minpack.lm (cosinor modeling)

Install these libraries using:

install.packages(c("dplyr", "tidyr", "magrittr", "ggplot2", "grid", "gridExtra", "openxlsx", "here", "cosinor2", "minpack.lm"))

Installation

Since this package is a collection of R scripts, you can use it by sourcing the individual files into your R environment. Place the scripts in your working directory and source them as follows:

source("filter_outliers.R")
source("create_combined_pdf.R")
source("export_excel.R")
source("extract_cosinor.R")
source("outlier_detection.R")
source("plot_cosinor.R")
source("zero_amp.R")

Alternatively, you can bundle them into a local R package using tools like devtools if desired.

Usage

The typical workflow involves preparing your data, processing it through the package functions, and exporting the results. Below is an overview of the steps:

Data Requirements

Your input data should be a data frame with at least the following columns:





ZT: Zeitgeber Time (numeric, typically 0–23 representing hours).



Concentration(mg): Neurocompound concentration (numeric).

Workflow





Load and Prepare Data: Load your dataset into R, ensuring it meets the column requirements.

data <- read.csv("your_data.csv")



Detect and Filter Outliers: Identify outliers and replace them with NA.

out_res <- outlier_detection(data)
filtered_data <- filter_outliers(data, out_res)



Fit a Cosinor Model: Use an external cosinor package (e.g., cosinor2) to fit the model.

library(cosinor2)
cosMod <- cosinor.lm(`Concentration(mg)` ~ time(ZT), data = filtered_data, period = 24)



Extract Parameters: Extract circadian parameters like mesor, amplitude, and acrophase.

cosinor_results <- extract_cosinor(cosMod, filtered_data, "example")



Perform Zero Amplitude Test: Test if the rhythm amplitude is significantly different from zero.

zero_amp_results <- zero_amp(cosMod, "example")



Plot Results: Visualize the original data and cosinor fit.

plot <- plot_cosinor(cosMod, filtered_data, "Cosinor Fit Example", "LD")
print(plot)



Export to Excel: Save results tables to an Excel file.

export_excel("example")



Create Combined PDF: Generate a PDF with headers and tables.

headers <- c("Cosinor Parameters", "Zero Amp Parameters")
tables <- list(cosinor_results$results_grob, zero_amp_results$results_grob, NULL, NULL, NULL, NULL)
create_combined_pdf(headers, tables, "Analysis Results", "output.pdf", 10, 8)

Function Details

filter_outliers.R





Description: Replaces outliers with NA in numeric columns based on outlier_detection results.



Parameters:





dataset: Data frame with numeric columns.



outlier_results: List from outlier_detection.



Return: Data frame with outliers replaced by NA.



Example:

data <- data.frame(x = rnorm(100), y = runif(100))
out_res <- outlier_detection(data)
filtered_data <- filter_outliers(data, out_res)

create_combined_pdf.R





Description: Creates a PDF with a title, two headers, and up to six tables in a grid layout.



Parameters:





headers: Vector of 2 header strings.



tables: List of 6 table grobs (use NULL for placeholders).



title_text: PDF title.



output_file: Output PDF filename.



fig_width, fig_height: Figure dimensions in inches.



Return: Generates a PDF file.



Example:

headers <- c("Header 1", "Header 2")
tables <- list(tableGrob(data.frame(a = 1:3)), NULL, NULL, NULL, NULL, NULL)
create_combined_pdf(headers, tables, "Test PDF", "test.pdf", 10, 8)

export_excel.R





Description: Exports grob tables to an Excel file, organized by sheets.



Parameters:





file_abbrev: String to filter grob objects (e.g., "CBmal1").



Return: Saves an Excel file in the "XLSX Exports" directory.



Example:

export_excel("CBmal1")

extract_cosinor.R





Description: Extracts circadian parameters from a cosinor model.



Parameters:





cosMod: Cosinor model object.



data: Data frame with ZT and Concentration(mg) columns.



file_suffix: Suffix for naming results.



Return: List with a graphical table (results_grob) and raw results (results_table).



Example:

cosinor_results <- extract_cosinor(cosMod, filtered_data, "suffix")

outlier_detection.R





Description: Detects outliers using the IQR method.



Parameters:





dataset: Data frame with numeric columns.



Return: List of outlier detection results per column.



Example:

out_res <- outlier_detection(data)

plot_cosinor.R





Description: Plots original data with the cosinor fit overlaid.



Parameters:





cosMod: Cosinor model object.



original_data: Data frame with ZT and Concentration(mg).



plot_title: Plot title.



data_type: "LD" or "rLEN" for styling.



x_breaks: X-axis breaks (default: seq(0, 24, by = 6)).



y_limits: Optional y-axis limits.



Return: ggplot object.



Example:

plot <- plot_cosinor(cosMod, filtered_data, "Plot Title", "LD")

zero_amp.R





Description: Performs a zero amplitude test on a cosinor model.



Parameters:





cosMod: Cosinor model object.



suffix: Optional suffix for output naming.



Return: List with a graphical table (results_grob) and raw results (results_table).



Example:

zero_amp_results <- zero_amp(cosMod, "suffix")

Data Analysis





Cosinor Model: Fits a sinusoidal curve to time-series data to estimate:





Mesor: Midline estimating statistic of rhythm (average level).



Amplitude: Half the peak-to-trough distance.



Acrophase: Time of peak value (in radians or hours).



Zero Amplitude Test: Assesses whether the amplitude is significantly different from zero, indicating a detectable rhythm.

Plotting and Visualization





plot_cosinor.R creates a scatter plot of the original data with a smoothed cosinor fit line. Customize the plot with:





X-axis breaks (e.g., every 6 hours).



Y-axis limits (optional).



Color and line type based on data_type ("LD" or "rLEN").

Exporting Results





Excel Export: Saves grob tables to an Excel file in the "XLSX Exports" directory, with sheets named based on grob object names.



PDF Creation: Produces a PDF with a title, two headers, and up to six tables arranged in a three-row grid layout.

Examples

Here’s a complete example using sample data:

# Generate sample data
data <- data.frame(ZT = rep(0:23, each = 5), `Concentration(mg)` = sin(2 * pi * rep(0:23, each = 5) / 24) + rnorm(120, 0, 0.1))

# Detect and filter outliers
out_res <- outlier_detection(data)
filtered_data <- filter_outliers(data, out_res)

# Fit cosinor model
library(cosinor2)
cosMod <- cosinor.lm(`Concentration(mg)` ~ time(ZT), data = filtered_data, period = 24)

# Extract parameters
cosinor_results <- extract_cosinor(cosMod, filtered_data, "sample")

# Zero amplitude test
zero_amp_results <- zero_amp(cosMod, "sample")

# Plot
plot <- plot_cosinor(cosMod, filtered_data, "Sample Cosinor Fit", "LD")
print(plot)

# Export to Excel
export_excel("sample")

# Create PDF
headers <- c("Cosinor Parameters", "Zero Amp Parameters")
tables <- list(cosinor_results$results_grob, zero_amp_results$results_grob, NULL, NULL, NULL, NULL)
create_combined_pdf(headers, tables, "Sample Analysis", "sample_output.pdf", 10, 8)

Troubleshooting and FAQs





Missing Libraries: Install all required packages listed in the Dependencies section.



Data Format Errors: Ensure your data frame has ZT and Concentration(mg) columns.



Function Errors: Check parameter inputs match the expected types (e.g., cosMod must be a valid cosinor model object).

FAQs





Q: Why does my plot look empty?





A: Ensure cosMod contains fitted values and original_data has the correct columns.



Q: How do I interpret the zero amplitude test?





A: A p-value < 0.05 indicates a significant circadian rhythm.

Contributing and Support





Contributing: Feel free to fork the repository and submit pull requests with improvements.



Support: For assistance, email [support@example.com] or visit [forum link].

License and Citation





License: Distributed under the MIT License.



Citation: If used in research, please cite: "Circadian Rhythm Analysis Package, [Your Name], 2023."
