# Loading Necessary Libraries
library(stats)
library(gridExtra)
library(minpack.lm)  # Required for nlsLM function

#' Extract Cosinor Function
#'
#' This function extracts and calculates parameters from cosinor models.
#'
#' @param cosMod A cosinor model object.
#' @param data The dataset you wish to analyze. It must contain 'ZT' and 'Fold_Changes' columns.
#' @param file_suffix The suffix that you want to have for your file name.
#' @return A graphical object containing a table of parameters.
#' @importFrom stats coef optimize
#' @importFrom gridExtra tableGrob
#' @importFrom minpack.lm nlsLM
#' @export

extract_cosinor_clock <- function(cosMod, data, file_suffix) {
  num_observations <- length(cosMod$fit$residuals)
  coefficients <- coef(cosMod)  # Isolating Coefficients in Cosinor Model
  
  # Error Check to confirm Cosinor Model is Correct Length
  if (length(coefficients) < 3) {
    stop("The cosinor model must have at least three coefficients: mesor, amplitude, and acrophase.")
  }
  
  # Indexing Unique/Important Coefficients for Table Storage
  ## Calculate Mesor
  mesor <- coefficients[1]
  
  ## Calculate Amplitude
  amplitude <- coefficients[2]
  
  ## Calculate Acrophase in Radians
  acrophase <- coefficients[3]
  
  ## Calculate Acrophase in Hours
  acrophase_hrs <- (acrophase * 24) / (2 * pi)
  if (acrophase_hrs < 0) {
    acrophase_hrs <- acrophase_hrs + 24
  }
  
  # Retrieve the peak ZT variable
  peak_ZT_name <- paste0("peak_ZT_", file_suffix)
  if (exists(peak_ZT_name, envir = .GlobalEnv)) {
    peak_ZT <- get(peak_ZT_name, envir = .GlobalEnv)
    
    # Compare and adjust acrophase_hrs if necessary
    if (abs(acrophase_hrs - peak_ZT) > 3) {
      # Recalculate acrophase using the provided method
      # Ensure 'ZT' and 'Fold_Changes' columns exist
      if (!("ZT" %in% colnames(data))) {
        stop("The data must contain a 'ZT' column representing time.")
      }
      if (!("Fold_Changes" %in% colnames(data))) {
        stop("The data must contain a 'Fold_Changes' column.")
      }
      
      # Extract the necessary columns
      ZT <- data$ZT
      Fold_Changes <- data$`Fold_Changes`
      
      # Define the sinusoidal model with fixed frequency
      freq_fixed <- 2 * pi / 24  # Fixing frequency to represent a 24-hour period
      
      sinusoidal_model_fixed <- function(t, amp, phase, offset) {
        amp * sin(freq_fixed * t + phase) + offset
      }
      
      # Initial parameter estimates
      start_params_fixed <- list(
        amp = (max(Fold_Changes) - min(Fold_Changes)) / 2,
        phase = 0,  # Initial guess for phase
        offset = mean(Fold_Changes)
      )
      
      # Fit the model using nlsLM from the minpack.lm package
      fit_nlsLM <- nlsLM(
        formula = Fold_Changes ~ sinusoidal_model_fixed(ZT, amp, phase, offset),
        data = data.frame(ZT = ZT, Fold_Changes = Fold_Changes),
        start = start_params_fixed,
        control = nls.lm.control(maxiter = 200)
      )
      
      # Extract the coefficients
      coeffs_nlsLM <- coef(fit_nlsLM)
      amp_nlsLM <- coeffs_nlsLM["amp"]
      phase_nlsLM <- coeffs_nlsLM["phase"]
      offset_nlsLM <- coeffs_nlsLM["offset"]
      
      # Calculate the acrophase, adjusting for the sign of amplitude
      if (is.finite(amp_nlsLM) && is.finite(phase_nlsLM)) {
        if (amp_nlsLM > 0) {
          t_peak <- (pi / 2 - phase_nlsLM) / freq_fixed
        } else if (amp_nlsLM < 0) {
          t_peak <- (3 * pi / 2 - phase_nlsLM) / freq_fixed
        } else {
          stop("Amplitude is zero; cannot compute acrophase.")
        }
        
        # Adjust t_peak to be within the 24-hour range
        t_peak_hours <- t_peak %% 24
        
        # Ensure t_peak_hours is positive
        if (t_peak_hours < 0) {
          t_peak_hours <- t_peak_hours + 24
        }
        
        # Update acrophase and acrophase_hrs with the new values
        acrophase_hrs <- t_peak_hours
        acrophase <- (acrophase_hrs * 2 * pi) / 24
      } else {
        warning("Failed to calculate acrophase using nlsLM; using original acrophase.")
      }
    }
  } else {
    warning(paste("The variable", peak_ZT_name, "does not exist in the global environment."))
  }
  
  ## Calculate Bathyphase in Hours
  bathyphase_hrs <- acrophase_hrs + 12
  if (bathyphase_hrs >= 24) {
    bathyphase_hrs <- bathyphase_hrs - 24
  }
  
  ## Calculate beta1 and beta2
  beta1 <- amplitude * cos(acrophase)  # β1 for the cosine term
  beta2 <- amplitude * sin(acrophase)  # β2 for the sine term
  
  # Calculate Residuals and Standard Error
  residuals_vec <- cosMod$fit$residuals
  residual_se <- sqrt(sum(residuals_vec^2) / (num_observations - length(coefficients)))
  
  # Calculate Standard Deviation and Error of Acrophase
  sd_acrophase <- residual_se * sqrt((cos(acrophase)^2 + sin(acrophase)^2) / (amplitude^2))
  se_acrophase <- sd_acrophase / sqrt(num_observations)
  
  # Convert Standard Deviation and Error to Hours
  sd_acrophase_hrs <- (sd_acrophase * 24) / (2 * pi)
  se_acrophase_hrs <- (se_acrophase * 24) / (2 * pi)
  
  # Calculate Standard Deviation and Error of Amplitude
  sd_amplitude <- residual_se / sqrt(num_observations)
  se_amplitude <- sd_amplitude / sqrt(num_observations)
  
  # Creating Table to Store Unique/Important Coefficients
  results <- data.frame(
    Parameter = c(
      paste0(file_suffix, " Total N"),
      paste0(file_suffix, " Mesor"),
      paste0(file_suffix, " Amplitude"),
      paste0(file_suffix, " Acrophase (Rads)"),
      paste0(file_suffix, " Acrophase (Hrs)"),
      paste0(file_suffix, " Bathyphase (Hrs)"),
      paste0(file_suffix, " Std. Dev. (Rads)"),
      paste0(file_suffix, " Std. Error (Rads)"),
      paste0(file_suffix, " Std. Dev (Hrs)"),
      paste0(file_suffix, " Std. Error (Hrs)"),
      paste0(file_suffix, " Std. Dev. Amplitude"),
      paste0(file_suffix, " Std. Error Amplitude")
    ),
    Values = c(
      num_observations, mesor, amplitude,
      acrophase, acrophase_hrs, bathyphase_hrs,
      sd_acrophase, se_acrophase, sd_acrophase_hrs, se_acrophase_hrs, 
      sd_amplitude, se_amplitude
    )
  )
  
  # Convert Table into a Graphical Object
  results_grob <- gridExtra::tableGrob(results)
  
  # Return both the graphical object and the raw results table
  return(list(results_grob = results_grob, results_table = results))
}