# Loading Necessary Libraries
library(grid)
library(gridExtra)

#' Create a Combined PDF with Headers and Tables
#'
#' This function generates a PDF file containing a title followed by a grid layout
#' of headers and tables. The layout consists of two headers and six table grobs
#' arranged in a specific format. The function also includes the option to specify
#' the title text, the output file name, and the figure width.
#'
#' @param headers A character vector of length 2, containing the texts for the two headers.
#' @param tables A list of 6 table grobs to be arranged in the layout. You can pass NULL for placeholders if fewer tables are used.
#' @param title_text A character string specifying the text for the title of the PDF.
#' @param output_file A character string specifying the name of the output PDF file.
#' @param fig_width A numeric value specifying the width of the figures in inches.
#'
#' @return NULL This function generates a PDF file and does not return any value.
#'
#' @examples
#' headers <- c("Cosinor Parameters", "Zero Amp Parameters")
#' tables <- list(LD_CS_Results_grob, LD_CS_amp_test_grob, rLEN_CS_Results_grob, rLEN_CS_amp_test_grob, NULL, NULL)
#' title_text <- "Comparison of Serum Corticosterone LD and rLEN Circadian Disruption Values"
#' output_file <- "combined_CS_tables.pdf"
#' fig_width <- 10
#' create_combined_pdf(headers, tables, title_text, output_file, fig_width)

create_combined_pdf <- function(headers, tables, title_text, output_file, 
                                fig_width, fig_height) {
  # Check if headers and tables lengths match the expected format
  if (length(headers) != 2 || length(tables) != 6) {
    stop("The number of headers or tables is not correct. Expected 2 headers and 6 tables.")
  }
  
  # Create headers for sections
  header_grobs <- lapply(headers, function(header_text) {
    textGrob(header_text, gp = gpar(fontsize = 12, fontface = "bold"))
  })
  
  # Combine headers and tables into a grid layout
  combined_layout <- arrangeGrob(
    # First row: headers
    arrangeGrob(
      header_grobs[[1]],
      header_grobs[[2]],
      ncol = 2,  # Two columns: one for each header
      widths = unit.c(unit(0.5, "npc"), unit(0.5, "npc"))
    ),
    # Second row: tables for LD and rLEN
    arrangeGrob(
      tables[[1]],
      tables[[2]],
      ncol = 2,  # Two columns: one for each type of data
      widths = unit.c(unit(0.5, "npc"), unit(0.5, "npc"))
    ),
    # Third row: tables for rLEN
    arrangeGrob(
      tables[[3]],
      tables[[4]],
      ncol = 2,  # Two columns: one for each type of data
      widths = unit.c(unit(0.5, "npc"), unit(0.5, "npc"))
    ),
    nrow = 3,  # Three rows total
    heights = unit.c(unit(0.1, "npc"), unit(0.4, "npc"), unit(0.4, "npc"))
  )
  
  # Create a title grob
  title_grob <- textGrob(title_text, gp = gpar(fontsize = 12, fontface = "bold"))
  # Arrange the title and the combined layout into a single layout and save to PDF
  pdf(output_file, width = fig_width, height = fig_height)
  grid.arrange(
    title_grob,
    combined_layout,
    nrow = 2,
    heights = c(0.05, 0.95)  # Height ratio for title and content
  )
  dev.off()
}
