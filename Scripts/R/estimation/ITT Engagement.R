library(dplyr)
library(fixest)
library(readr)

# ----------------------------------------------------------------------------
# Step 1: Load and Prepare the Data
# ----------------------------------------------------------------------------

# Load your data. Replace "path/to/your/data.csv" with the actual file path.
# Using read_csv from the 'readr' package is generally faster and better.
table(data$Q1)
table(is.na(data$post_id))
# Create a clean, analysis-ready dataframe
summary(as.numeric(data$pre_groupTime17388))

data_clean <- data %>%
  
  # --- Define the Outcome Variable: Completion ---
  # We define completion as having a non-missing 'post_id'.
  # This creates a binary (0/1) variable.
  mutate(completed_post = ifelse(is.na(post_id)==F, 1, 0)) %>%
  
  # --- Define Treatment Assignment Variables ---
  #   Q1 is coded as 1=Control, 2=Generic AI, 3=Tailored AI.
   mutate(
    assign_t1 = ifelse(Q1 == 2, 1, 0),
    assign_t2 = ifelse(Q1 == 3, 1, 0)
  ) %>%
  
  # --- Define Control and Cluster Variables ---
  # Using more descriptive names for clarity
  mutate(
    school_id      = factor(pre_G01Q05),
    baseline_score = as.numeric(pre_Score),
    gender         = factor(pre_G01Q03),
    school_type    = factor(pre_G01Q07)
    # Note: We are omitting grade variables for this first simple model
    # to match the table outline, but you can add them.
  ) %>%
  
  # --- Select only the columns needed for this analysis ---
  select(
    pre_id, completed_post,pre_groupTime17388, assign_t1, assign_t2,
    school_id, baseline_score, gender, school_type
  )

# ----------------------------------------------------------------------------
# Step 2: Estimate the ITT Model for Engagement
# ----------------------------------------------------------------------------
# This is an OLS regression of completion on treatment assignment,
# including controls and school fixed effects, as discussed.
# The syntax `| school_id` tells feols to include school fixed effects.

itt_engagement <- feols(
  completed_post ~ assign_t1 + assign_t2 #+ baseline_score + gender + school_type #No Covariates 
  # | school_id
  ,
  data = data_clean 
)
summary(itt_engagement)
itt_engagement_X <- feols(
  completed_post ~ assign_t1 + assign_t2 + baseline_score + gender + pre_groupTime17388
  | school_type,
  data = data_clean
)
etable(itt_engagement_X, itt_engagement , tex = T)
# Print the results to the console with standard errors clustered by school
# This is how you verify the results yourself.
summary(itt_engagement,itt_engagement_X)
summary(itt_engagement_X, cluster = ~school_id)
# ----------------------------------------------------------------------------
# Step 3: Generate the Publication-Quality LaTeX Table
# ----------------------------------------------------------------------------
# The `etable` function from `fixest` is powerful for creating tables.
# Note on the Intercept: `feols` absorbs the intercept when you add fixed effects.
# The coefficients on T1 and T2 are the estimated effects relative to the control group.
# To get the Control Group Mean for the table, we can calculate it directly.

control_mean_completion <- data_clean %>%
  filter(assign_t1 == 0, assign_t2 == 0) %>%
  summarise(mean_val = mean(completed_post, na.rm = TRUE)) %>%
  pull(mean_val)

# The table in your paper draft shows an intercept. To replicate that specific table,
# we would run the model *without* fixed effects. Let's do that for the table.

# itt_engagement_for_table <- feols(
#   completed_post ~ assign_t1 + assign_t2 + baseline_score + gender + school_type,
#   data = data_clean
# )
# 
# # Now, create the table using this model
# etable(itt_engagement_for_table,
#        cluster = ~school_id, # Still cluster the errors by school
#        title = "The Effect of Treatment Assignment on Module Completion (Engagement)",
#        label = "tab:results_engagement",
#        # Rename variables for the table
#        dict = c("completed_post" = "Completed Post-Test (0/1)",
#                 "assign_t1" = "Assigned to Generic AI (T1)",
#                 "assign_t2" = "Assigned to Tailored AI (T2)",
#                 "(Intercept)" = "Control Group Mean (Constant)"),
#        # Add a note about controls
#        extraline = list("Controls" = "Yes", "School Fixed Effects" = "No"),
#        # Add the detailed table notes
#        notes = c("\\textit{Notes:} OLS estimates from an ITT specification. The dependent variable is an indicator equal to 1 if the student completed the post-test and 0 otherwise.",
#                  "The sample is the full set of randomized students (N=2,430).",
#                  "Controls include baseline pre-test score, gender, and indicators for school type (ASO, TSO, BSO).",
#                  "Robust standard errors, clustered by school (58 clusters), are in parentheses.",
#                  "*** p<0.01, ** p<0.05, * p<0.1."),
#        style.tex = style.tex("aer") # Use a style that mimics AER tables
# )
# 
