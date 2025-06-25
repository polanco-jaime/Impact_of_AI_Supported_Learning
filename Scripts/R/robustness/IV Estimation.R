# Load necessary libraries
library(AER)
library(dplyr)
library(lmtest)
library(sandwich)

# --- Critical Data Preparation ---
# (Assuming t0_Q1, TREATED, gained_tax_learning are already factors/numeric as needed from previous steps)

 


# Ensure factors still have appropriate levels after subsetting
if (is.factor(df_iv$t0_Q1)) {
  df_iv$t0_Q1 <- droplevels(df_iv$t0_Q1) # Drop unused factor levels
  if ("0" %in% levels(df_iv$t0_Q1)) {
    df_iv$t0_Q1 <- relevel(df_iv$t0_Q1, ref = "0")
  } else if ("1" %in% levels(df_iv$t0_Q1) && !("0" %in% levels(df_iv$t0_Q1))) {
    df_iv$t0_Q1 <- relevel(df_iv$t0_Q1, ref = "1")
  }
  print("Levels of instrument t0_Q1 in df_iv after subsetting/releveling:")
  print(levels(df_iv$t0_Q1))
  if(nlevels(df_iv$t0_Q1) < 2) stop("Instrument 't0_Q1' in df_iv must have at least two levels.")
}


# --- IV Estimation ---
iv_model <- NULL
summary_iv_model_diagnostics <- NULL
robust_test_results <- NULL

if(length(unique(df_iv$TREATED)) < 2) {
  print("Error: Endogenous variable 'TREATED' does not have at least two unique values in the *estimation sample (df_iv)*.")
} else {
  tryCatch({
    iv_model <- ivreg(gained_tax_learning ~ TREATED | t0_Q1, data = df_iv)
    summary_iv_model_diagnostics <- summary(iv_model, diagnostics = TRUE)
    print("IV Model Summary with Diagnostics:")
    print(summary_iv_model_diagnostics)
    
    robust_test_results <- coeftest(iv_model, vcov. = vcovHC(iv_model, type = "HC1"))
    print("IV Model Coefficients with Robust Standard Errors (HC1):")
    print(robust_test_results)
    
  }, error = function(e) {
    print(paste("Error during IV estimation with ivreg:", e$message))
  })
}




# Load necessary libraries
library(lmtest)
library(sandwich) # For robust standard errors with coeftest

# Ensure t0_Q1 is a factor with Control as the reference level
# (Assuming df_treated is your original dataframe)
if (!is.factor(df_treated$t0_Q1)) {
  df_treated$t0_Q1 <- factor(df_treated$t0_Q1)
}
if ("0" %in% levels(df_treated$t0_Q1)) {
  df_treated$t0_Q1 <- relevel(df_treated$t0_Q1, ref = "0")
  print("Reference level for t0_Q1 set to '0' (Control).")
} else if ("1" %in% levels(df_treated$t0_Q1) && !("0" %in% levels(df_treated$t0_Q1))) {
  df_treated$t0_Q1 <- relevel(df_treated$t0_Q1, ref = "1")
  print("Reference level for t0_Q1 set to '1' (Control).")
} else {
  warning("Could not automatically set Control as reference for t0_Q1. Assuming first level is reference.")
}

# Create a subset of completers (those for whom gained_tax_learning is likely available)
df_completers <- df_treated[df_treated$TREATED == 1 & !is.na(df_treated$gained_tax_learning), ]

print(paste("Number of completers with non-NA gained_tax_learning:", nrow(df_completers)))

if(nrow(df_completers) > 0 && nlevels(factor(df_completers$t0_Q1)) > 1) {
  # Intent-to-Treat (ITT) Analysis on completers
  # Effect of *assignment* on gained_tax_learning among those who completed
  print("--- Intent-to-Treat (ITT) Analysis on Completers ---")
  model_itt <- lm(gained_tax_learning ~ t0_Q1, data = df_completers)
  summary_itt <- summary(model_itt)
  print(summary_itt)
  
  # For robust standard errors
  robust_summary_itt <- coeftest(model_itt, vcov. = vcovHC(model_itt, type = "HC1"))
  print("ITT Coefficients with Robust Standard Errors (HC1):")
  print(robust_summary_itt)
  
  # If you want to add baseline covariates to ITT for precision:
  # model_itt_cov <- lm(gained_tax_learning ~ t0_Q1 + t0_Score, data = df_completers)
  # summary(model_itt_cov)
  # coeftest(model_itt_cov, vcov. = vcovHC(model_itt_cov, type = "HC1"))
  
} else {
  print("Not enough data or variation in t0_Q1 among completers to run ITT analysis.")
}
