# ----------------------------------------------------------------------------
# Preamble: Ensure necessary objects are in your environment
# ----------------------------------------------------------------------------
# This code assumes you have already run the previous scripts and have
# the 'data' dataframe and the 'completers' dataframe loaded and prepared.
# If not, please re-run the data preparation steps first.

library(dplyr)
library(fixest)

# ----------------------------------------------------------------------------
# Step 1: Create the "Gained" Score Variables for Completers
# ----------------------------------------------------------------------------

# We will create new columns in the 'completers' dataframe for the change
# in each psychosocial score from pre-test to post-test.
# I am using backticks (`) to handle the spaces in the column names.

completers_gained <- data %>%
  mutate(
    assign_t1 = ifelse(Q1 == 2, 1, 0),
    assign_t2 = ifelse(Q1 == 3, 1, 0)
  ) %>%
  mutate(
    school_id      = factor(pre_G01Q05),
    baseline_score = as.numeric(pre_Score),
    gender         = factor(pre_G01Q03),
    school_type    = factor(pre_G01Q07) 
  ) %>% 
  mutate(
    # Gained Financial Literacy (already created, but let's ensure it's here)
    gained_lit_std = (post_Score - pre_Score - control_gain_stats$mean_gain) / control_gain_stats$sd_gain,
    # Gained Psychosocial Scores
    gained_attitude = `post_Attitude and Motivation` - `pre_Attitude and Motivation`,
    gained_learning_exp = `post_Learning Experience & User Experience` - `pre_Learning Experience & User Experience`,
    gained_self_reg = `post_Self-Regulation & Metacognition` - `pre_Self-Regulation & Metacognition`,
    gained_engagement = `post_Engagement & Commitment` - `pre_Engagement & Commitment`,
    gained_self_confidence = `post_Self-Confidence & Self-Efficacy` - `pre_Self-Confidence & Self-Efficacy`,
    gained_emotional = `post_Emotional & Psychological Factors` - `pre_Emotional & Psychological Factors`,
    post_self_confidence =  `post_Self-Confidence & Self-Efficacy` ,
    pre_self_confidence =  `pre_Self-Confidence & Self-Efficacy`, 
    post_emotional_psyco =  `post_Emotional & Psychological Factors`, 
    pre_emotional_psyco =  `pre_Emotional & Psychological Factors`
  )
completers_gained = completers_gained[completers_gained['post_interviewtime']>=60 , ]
# ----------------------------------------------------------------------------
# Step 2: Run ITT Regressions for All Secondary Outcomes
# ----------------------------------------------------------------------------
# We can run all regressions at once using fixest's multiple LHS syntax.
# This is highly efficient.

itt_secondary <- feols(
  # List all dependent variables on the left-hand side
  c(gained_lit_std, gained_self_confidence, gained_attitude, gained_learning_exp, 
    gained_self_reg, gained_engagement, gained_emotional) 
  
  # The rest of the model is the same
  ~ assign_t1 + assign_t2, #+ baseline_score + gender + school_type | school_id,
  data = completers_gained,
  cluster = ~school_id
)
feols(post_Score~ assign_t1 + assign_t2 +baseline_score,  data = completers_gained,
      cluster = ~school_id
) 

feols(post_self_confidence~ assign_t1 + assign_t2+pre_self_confidence,  data = completers_gained,
      cluster = ~school_id
) 

feols(post_emotional_psyco~ assign_t1 + assign_t2+pre_emotional_psyco,  data = completers_gained,
      cluster = ~school_id
) 

# ----------------------------------------------------------------------------
# Step 3: View the Results to Inform the Paper
# ----------------------------------------------------------------------------

# This will print a clean summary table of all regressions to your console.
# Look for which variables have significant coefficients for assign_t1 or assign_t2.
summary(itt_secondary)
etable(itt_secondary)
