# Create a clean, analysis-ready dataframe
source("Scripts/R/General_settings.R")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(fixest)

# --- Step 1: Prepare the Data ---
# Let's assume 'data_clean' is your main dataframe.
# First, create the 'gain' variables for all outcomes.

# Create a list of all psychosocial constructs for easy iteration
psy_vars <- c("Engagement & Commitment",
              "Attitude and Motivation",
              "Learning Experience & User Experience", 
              "Self-Regulation & Metacognition",
              "Self-Confidence & Self-Efficacy",
              "Emotional & Psychological Factors"
              )

# Loop to create gain scores for each psychosocial variable
for (var in psy_vars) {
  pre_var <-   paste0("pre_",  var) #paste0("pre_", gsub(" ", "_", gsub(" & ", "_and_", var)))
  post_var <- paste0("post_",  var) #paste0("post_", gsub(" ", "_", gsub(" & ", "_and_", var)))
  gain_var <- paste0("gain_", gsub(" ", "_", gsub(" & ", "_and_", var)))
  
  # Check if both pre and post variables exist before creating the gain score
  if (pre_var %in% names(data) && post_var %in% names(data)) {
    data <- data %>%
      mutate(!!gain_var := .data[[post_var]] - .data[[pre_var]])
  }
}

# Add main outcomes
data_clean <- data %>%
  mutate(
    gain_score_raw = post_Score - pre_Score,
    completed_post = ifelse(!is.na(post_id), 1, 0)
  )

# --- Step 2: The Lee Bounds Function ---
# This function calculates the upper and lower bounds for the ATE of a given treatment.

calculate_lee_bounds <- function(data, outcome_var, treat_var, selection_var) {
  
  # Ensure variables are correctly formatted
  data <- data %>%
    mutate(
      outcome = .data[[outcome_var]],
      treatment = .data[[treat_var]],
      selected = .data[[selection_var]]
    ) %>%
    filter(!is.na(outcome) | selected == 0) # Keep all selected or non-selected
  
  # Calculate selection probabilities
  p1 <- mean(data$selected[data$treatment == 1], na.rm = TRUE)
  p0 <- mean(data$selected[data$treatment == 0], na.rm = TRUE)
  
  # Stop if no selection difference
  if (p1 == p0) {
    ate_est <- mean(data$outcome[data$treatment == 1 & data$selected == 1]) - mean(data$outcome[data$treatment == 0 & data$selected == 1])
    return(data.frame(outcome_variable = outcome_var, lower_bound = ate_est, upper_bound = ate_est))
  }
  
  # Case 1: Treatment group has higher selection rate (less attrition)
  if (p1 > p0) {
    trim_fraction <- (p1 - p0) / p1
    
    # Data for trimming
    y1_selected <- data$outcome[data$treatment == 1 & data$selected == 1]
    y0_selected <- data$outcome[data$treatment == 0 & data$selected == 1]
    
    # Trim the top of Y1 for the lower bound
    trim_val_upper <- quantile(y1_selected, 1 - trim_fraction, na.rm = TRUE)
    y1_trimmed_lower <- y1_selected[y1_selected <= trim_val_upper]
    lower_bound <- mean(y1_trimmed_lower, na.rm = TRUE) - mean(y0_selected, na.rm = TRUE)
    
    # Trim the bottom of Y1 for the upper bound
    trim_val_lower <- quantile(y1_selected, trim_fraction, na.rm = TRUE)
    y1_trimmed_upper <- y1_selected[y1_selected >= trim_val_lower]
    upper_bound <- mean(y1_trimmed_upper, na.rm = TRUE) - mean(y0_selected, na.rm = TRUE)
    
    # Case 2: Control group has higher selection rate
  } else { # p0 > p1
    trim_fraction <- (p0 - p1) / p0
    
    # Data for trimming
    y1_selected <- data$outcome[data$treatment == 1 & data$selected == 1]
    y0_selected <- data$outcome[data$treatment == 0 & data$selected == 1]
    
    # Trim the bottom of Y0 for the lower bound
    trim_val_lower_y0 <- quantile(y0_selected, trim_fraction, na.rm = TRUE)
    y0_trimmed_upper <- y0_selected[y0_selected >= trim_val_lower_y0]
    lower_bound <- mean(y1_selected, na.rm = TRUE) - mean(y0_trimmed_upper, na.rm = TRUE)
    
    # Trim the top of Y0 for the upper bound
    trim_val_upper_y0 <- quantile(y0_selected, 1 - trim_fraction, na.rm = TRUE)
    y0_trimmed_lower <- y0_selected[y0_selected <= trim_val_upper_y0]
    upper_bound <- mean(y1_selected, na.rm = TRUE) - mean(y0_trimmed_lower, na.rm = TRUE)
  }
  
  return(data.frame(outcome_variable = outcome_var, lower_bound = lower_bound, upper_bound = upper_bound))
}


# --- Step 3: Run the Lee Bounds Calculation for All Outcomes ---

# Define the list of outcome variables (using raw scores, not standardized yet)
outcome_vars <- c(
  "gain_score_raw",
  "gain_Engagement_and_Commitment",
  "gain_Attitude_and_Motivation",
  "gain_Learning_Experience_and_User_Experience",
  "gain_Self-Regulation_and_Metacognition",
  "gain_Emotional_and_Psychological_Factors",
  "gain_Self-Confidence_and_Self-Efficacy"
)
data$`gain_Self-Confidence_and_Self-Efficacy`
# Calculate bounds for Tailored AI (T2)
data_clean$assign_t2 = ifelse(data_clean$Q1=='3',1,0)
lee_bounds_t2 <- lapply(outcome_vars, function(var) {
  calculate_lee_bounds(data = data_clean, 
                       outcome_var = var, 
                       treat_var = "assign_t2", 
                       selection_var = "completed_post")
}) %>% bind_rows()

# Calculate bounds for Generic AI (T1)
data_clean$assign_t1 = ifelse(data_clean$Q1=='2',1,0)
lee_bounds_t1 <- lapply(outcome_vars, function(var) {
  calculate_lee_bounds(data = data_clean, 
                       outcome_var = var, 
                       treat_var = "assign_t1", 
                       selection_var = "completed_post")
}) %>% bind_rows()


# --- Step 4: Display the Results ---
print("Lee (2009) Bounds for Tailored AI (T2) vs. Control")
print(lee_bounds_t2)

print("Lee (2009) Bounds for Generic AI (T1) vs. Control")
print(lee_bounds_t1)



##### 1. Estiamtions Gained Score ####

# --- Step 1: Define Imputation Values for Each Outcome ---
# For each outcome, we find the minimum, maximum, and midpoint observed among completers.

imputation_values <- lapply(outcome_vars, function(var) {
  vals <- data_clean %>% 
    filter(completed_post == 1) %>%
    pull(.data[[var]])
  
  data.frame(
    outcome_variable = var,
    impute_min = min(vals, na.rm = TRUE),
    impute_max = max(vals, na.rm = TRUE),
    impute_mid = median(vals, na.rm = TRUE) # Using median is more robust than mean
  )
}) %>% bind_rows()

print("Imputation Values for Sensitivity Analysis:")
print(imputation_values)


# --- Step 2: Create a Loop to Run Regressions for Each Imputation Scenario ---

# This list will store all our regression models
sensitivity_models <- list()
data_clean$school_id =  data_clean$pre_G01Q07
for (i in 1:nrow(imputation_values)) {
  
  var_name <- imputation_values$outcome_variable[i]
  min_val <- imputation_values$impute_min[i]
  mid_val <- imputation_values$impute_mid[i]
  max_val <- imputation_values$impute_max[i]
  
  # Create a temporary dataframe with the imputed columns
  temp_data <- data_clean %>%
    mutate(
      outcome_imputed_zero = ifelse(completed_post == 1, .data[[var_name]], 0),
      outcome_imputed_min  = ifelse(completed_post == 1, .data[[var_name]], min_val),
      outcome_imputed_mid  = ifelse(completed_post == 1, .data[[var_name]], mid_val),
      outcome_imputed_max  = ifelse(completed_post == 1, .data[[var_name]], max_val)
    )
  
  # Run the four regressions for this outcome variable
  model_zero <- feols(outcome_imputed_zero ~ assign_t1 + assign_t2 | school_id, data = temp_data)
  model_min  <- feols(outcome_imputed_min  ~ assign_t1 + assign_t2 | school_id, data = temp_data)
  model_mid  <- feols(outcome_imputed_mid  ~ assign_t1 + assign_t2 | school_id, data = temp_data)
  model_max  <- feols(outcome_imputed_max  ~ assign_t1 + assign_t2 | school_id, data = temp_data)
  
  # Store the models in the list, naming them clearly
  sensitivity_models[[paste0(var_name, "_Zero")]] <- model_zero
  sensitivity_models[[paste0(var_name, "_Min")]]  <- model_min
  sensitivity_models[[paste0(var_name, "_Mid")]]  <- model_mid
  sensitivity_models[[paste0(var_name, "_Max")]]  <- model_max
}


# --- Step 3: Display Results in a Table ---
# Example for one outcome: "gain_score_raw"
etable(sensitivity_models[c("gain_score_raw_Zero", "gain_score_raw_Min", 
                            "gain_score_raw_Mid", "gain_score_raw_Max")],
       headers = list("Impute 0 (Lower Bound)" = 1, "Impute Min" = 1, "Impute Mid" = 1, "Impute Max" = 1),
       tex = TRUE)
etable(sensitivity_models, 
       headers = list("Impute 0 (Lower Bound)" = 1, "Impute Min" = 1, "Impute Mid" = 1, "Impute Max" = 1),
       tex = TRUE)
#### 2. Other results ####
# --- Step 1: Ensure Your 'sensitivity_models' List is Complete ---
# The code you previously ran should have already created this list with all regressions.
# We will now systematically extract them to create clear, separate tables.

# --- Step 2: Define Key Outcomes and Create a Loop to Generate Tables ---

# Define the primary outcomes you want to create tables for
key_outcomes <- c(
  "gain_score_raw", 
  # "learning_efficiency_std", 
  # "retention_score_std", 
  "gain_Engagement_and_Commitment",
  "gain_Attitude_and_Motivation",
  "gain_Learning_Experience_and_User_Experience",
  "gain_Self-Regulation_and_Metacognition" ,
  "gain_Emotional_and_Psychological_Factors",
  "gain_Self-Confidence_and_Self-Efficacy" 
)

# Loop through each key outcome to generate and print its sensitivity table
for (outcome_name in key_outcomes) {
  
  # Construct the names of the four models for the current outcome
  model_names <- paste0(outcome_name, c("_Zero", "_Min", "_Mid", "_Max"))
  
  # Create a clean title for the table
  outcome_title <- tools::toTitleCase(gsub("_", " ", gsub("_std|_raw", "", outcome_name)))
  table_title <- paste0("Sensitivity Analysis for: ", outcome_title)
  
  # Print a header to the console to separate the outputs
  cat("\n\n--- LaTeX Table Code for:", outcome_title, "---\n\n")
  
  # Generate the etable
  print(
    etable(
      sensitivity_models[model_names],
      headers = list(
        "Impute 0 (Lower Bound)" = 1, 
        "Impute Min" = 1, 
        "Impute Median" = 1, 
        "Impute Max" = 1
      ),
      title = table_title,
      tex = TRUE # Set to FALSE if you just want to view in the console
    )
  )
}

# TO GENERATE PSYCHOSOCIAL TABLES:
# Simply add their gain-score names to the 'key_outcomes' vector. For example:
# key_outcomes_psy <- c("gain_Engagement_and_Commitment", "gain_Self_Confidence_and_Self_Efficacy")
# And re-run the loop.


#### 3. Psycosocial effect for compelters ####
# Load necessary libraries if not already loaded
library(dplyr)
library(fixest)

# --- Step 1: Prepare the Data ---
# Let's assume 'data_clean' has the 'gain' variables for psychosocial outcomes.
# We filter to keep only students who completed the post-test.
completers_data <- data_clean %>%
  filter(completed_post == 1)

# --- Step 2: Run the Regressions for Each Psychosocial Outcome ---

# Define the psychosocial 'gain' variables you want to analyze
psycho_gain_vars <- c(
  "gain_Engagement_and_Commitment",
  "gain_Attitude_and_Motivation",
  "gain_Self_Confidence_and_Self_Efficacy",
  "gain_Learning_Experience_and_User_Experience",
  "gain_Self_Regulation_and_Metacognition",
  "gain_Emotional_and_Psychological_Factors"
)
colnames(completers_data)[336] = "gain_Self_Regulation_and_Metacognition"
colnames(completers_data)[337] = "gain_Self_Confidence_and_Self_Efficacy"
# Create an empty list to store the regression models
psycho_models <- list()
completers_data$baseline_score = completers_data$pre_Score
completers_data$gender = completers_data$pre_G01Q03
# Loop through each variable, run the feols model, and store it
for (var in psycho_gain_vars) {
  # The formula is constructed dynamically for each outcome
  formula <- as.formula(paste(var, "~ assign_t1 + assign_t2 + baseline_score + gender | school_id"))
  
  # Run the regression
  model <- feols(
    fml = formula,
    data = completers_data,
    cluster = ~school_id
  )
  
  # Store the model in the list
  psycho_models[[var]] <- model
}

# --- Step 3: Generate the Professional Table ---
# We use etable with style.panel to group results by the dependent variable.
etable(psycho_models,
       drop = c("baseline_score", "gender"),
       
       title = "The Effect of AI Assignment on Psychosocial Outcomes (Completer Sample)",
       tex = TRUE)
etable(
  psycho_models,
  style.panel = style.depvar(
    depvar.title = "Dependent Variable: Gain in",
    panel.title = "Panel %s"
  ),
  drop = c("baseline_score", "gender"), # Keep the table clean
  headers = list("ITT on Completers" = 4),
  title = "The Effect of AI Assignment on Psychosocial Outcomes (Completer Sample)",
  tex = TRUE # Set to FALSE to view in R console
)
