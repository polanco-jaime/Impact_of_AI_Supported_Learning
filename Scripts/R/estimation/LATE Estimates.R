# Load necessary libraries if not already loaded
library(dplyr)
library(fixest)
# Loading data#
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
              "Emotional & Psychological Factors",
              "Score"
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
  mutate(completed_post = ifelse(is.na(post_id)==F, 1, 0)) %>% 
  mutate(
    assign_t1 = ifelse(Q1 == '2', 1, 0),
    assign_t2 = ifelse(Q1 == '3', 1, 0),
    assign_t0 = ifelse(Q1 == '0', 1, 0)
    
  ) %>% 
  mutate(
    school_id      = factor(pre_G01Q05),
    baseline_score = as.numeric(pre_Score),
    gender         = factor(pre_G01Q03),
    school_type    = factor(pre_G01Q07) 
  ) %>% 
  select(
    pre_id, completed_post, assign_t1, assign_t2,
    school_id, baseline_score, gender,
    school_type,postpost_id,
    postpost_Score,pre_Score,post_Score,
    
    `pre_Engagement & Commitment`,
    `pre_Attitude and Motivation`,
    `pre_AI Attitude and Motivation`,
    `pre_Learning Experience & User Experience`,
    `pre_Self-Regulation & Metacognition`,
    `pre_Emotional & Psychological Factors`,
    
    `post_Engagement & Commitment`,
    `post_Attitude and Motivation`,
    `post_Learning Experience & User Experience`,
    `post_Self-Regulation & Metacognition`,
    `post_Emotional & Psychological Factors`,
    
    `postpost_Engagement & Commitment`,
    `postpost_Attitude and Motivation`,
    `postpost_Learning Experience & User Experience`,
    `postpost_Self-Regulation & Metacognition`,
    `postpost_Emotional & Psychological Factors`,
    
    post_interviewtime,pre_interviewtime, pre_id_student, post_id_student,
  )

# First, ensure you have a 'completer' dataframe
completers <- data_clean %>% filter(completed_post == 1)

# And a dataframe for the follow-up
# Assuming 'postpost_id' indicates completion of the follow-up test
data_with_followup <- data_clean %>%
  mutate(completed_followup = ifelse(is.na(postpost_id)==F, 1, 0)) %>%
  filter(completed_followup == 1) %>%
  # Merge relevant pre-test variables needed for controls
  select(pre_id, postpost_Score, assign_t1, assign_t2,
         school_id, baseline_score, gender, school_type)

# Calculate the mean and SD of the control group's gain score

control_gain_stats <- completers %>%
  filter(assign_t1 == 0 & assign_t2 == 0) %>%
  mutate(gain_score = post_Score - pre_Score) %>% 
  summarise(mean_gain = mean(gain_score, na.rm = TRUE),
            sd_gain = sd(gain_score, na.rm = TRUE))
control_gain_stats
# Standardize Gained Financial Literacy for all completers

completers <- completers %>%
  mutate(
    gain_score = post_Score - pre_Score 
  )
completers$gain_score = ifelse(completers$gain_score <= 0, completers$gain_score *-1, completers$gain_score )

completers <- completers %>%
  mutate( 
    gained_lit_std = (gain_score - control_gain_stats$mean_gain) / control_gain_stats$sd_gain
  )
hist(completers$gain_score,6)
hist(completers$gained_lit_std,6)

# Create Learning Efficiency
# Assuming `post_interviewtime` is the time spent in seconds, convert to minutes
summary(completers$post_interviewtime)
summary(completers['post_interviewtime'])


completers_real = completers

  summary(completers_real$post_interviewtime/24)
  completers_real <- completers_real %>%
    mutate(
      time_spent_min = post_interviewtime / 60,
      learning_efficiency = gained_lit_std / time_spent_min
    )
  summary(completers_real$learning_efficiency)
  # Standardize Learning Efficiency using the control group's distribution
  control_efficiency_stats <- completers_real %>%
    filter(assign_t1 == 0 & assign_t2 == 0) %>%
    summarise(mean_eff = mean(learning_efficiency, na.rm = TRUE, finite = TRUE),
              sd_eff = sd(learning_efficiency, na.rm = TRUE))
  
  completers_real <- completers_real %>%
    mutate(
      learning_efficiency_std = (learning_efficiency - control_efficiency_stats$mean_eff) / control_efficiency_stats$sd_eff
    )
  
  # Standardize Retention Score using the control group's distribution
  control_retention_stats <- data_with_followup %>%
    filter(assign_t1 == 0 & assign_t2 == 0) %>%
    summarise(mean_ret = mean(postpost_Score, na.rm = TRUE),
              sd_ret = sd(postpost_Score, na.rm = TRUE))
  
  data_with_followup <- data_with_followup %>%
    mutate(
      retention_score_std = (postpost_Score - control_retention_stats$mean_ret) / control_retention_stats$sd_ret
    )
  data_with_followup <- data_with_followup %>%
    mutate(
      completed_t1 = ifelse(assign_t1 == 1  , 1, 0),
      completed_t2 = ifelse(assign_t2 == 1  , 1, 0)
    )

  

  
completers_real <- completers_real %>%
    mutate(
      completed_t1 = ifelse(assign_t1 == 1 & completed_post == 1, 1, 0),
      completed_t2 = ifelse(assign_t2 == 1 & completed_post == 1, 1, 0)
    )
 

# 'analysis_data' is now your final, complete dataframe for all regressions.

# --- Step 4: Run the LATE (2SLS) Regressions ---

# Define the main learning outcomes
learning_outcomes <- c("gained_lit_std", "learning_efficiency_std")
late_models <- list()


for (outcome in learning_outcomes) {
  formula <- as.formula(
    paste(outcome, "~ 1 | school_type | completed_t1 + completed_t2 ~ assign_t1 + assign_t2")
  )
  model <- feols(fml = formula, data = completers_real)
  late_models[[outcome]] <- model
}


model <- feols(retention_score_std ~ 1 | school_type | completed_t1 + completed_t2 ~ assign_t1 + assign_t2
                 , data = data_with_followup)
late_models[["retention_score_std"]] <- model

# --- Step 5: Generate the Final LaTeX Table ---

# This will now run without errors.

etable(
  late_models ,
  tex = T,
  title = "The Effect of Treatment Completion on Learning Outcomes (LATE Estimates)"
)
?etable
# Alternative etable command for older fixest versions
etable(
  late_models,
  title = "The Effect of Treatment Completion on Learning Outcomes (LATE Estimates)",
  # Instead of style.panel, we use the 'headers' argument
  # and manually create the panel titles.
  headers = list(
    "Gained Score (SD)" = 1,
    "Learning Eff. (SD)" = 1,
    "Retention (SD)" = 1
  ),
  # The rest of the command remains the same
  drop = c("Intercept", "baseline_score", "gender"),
  fitstat = ~ n + ivf1.t1 + ivf1.t2,
  dict = c(ivf1.t1 = "F-stat (T1 Instrument)", ivf1.t2 = "F-stat (T2 Instrument)"),
  tex = TRUE
)
