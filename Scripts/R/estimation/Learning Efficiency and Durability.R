# Create a clean, analysis-ready dataframe
source("Scripts/R/General_settings.R")
#### 1. Data Cleaning  - Conditional on completion ####

  if (1==1) {
    
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
    if (1==1) {
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
      
      
      # --- Step 2: Run the Regressions ---
      
      ##### Model for Gained learning ####
      
      itt_gained <- feols(
        gain_score ~ assign_t1 + assign_t2 | school_type,
        data = completers_real,
        cluster = ~school_type+school_id
      )
      itt_gained_X <- feols(
        gain_score ~ assign_t1 + assign_t2 + baseline_score + gender | school_type,
        data = completers_real,
        cluster = ~school_type+school_id
      )
      itt_gained_X
      ##### Model for Learning Efficiency  ####
      itt_efficiency <- feols(
        learning_efficiency_std ~ assign_t1 + assign_t2 | school_type,
        data = completers_real,
        cluster = ~school_type+school_id
      )
      
      summary(itt_efficiency )
      
      itt_efficiency_X <- feols(
        learning_efficiency_std ~ assign_t1 + assign_t2 + baseline_score + gender | school_type,
        data = completers_real,
        cluster = ~school_type+school_id
      )
      summary(itt_efficiency_X )
      
      ##### Model for Knowledge Retention   ####
      # completers_real$retention_score_std = ifelse(
      #   is.na(completers_real$retention_score_std)==T, 0, 
      #   completers_real$retention_score_std
      # )
      
      itt_retention <- feols(
        retention_score_std ~ assign_t1 + assign_t2  | school_type,
        data = data_with_followup,
        cluster = ~school_type+school_id
      )
      
      # summary(itt_retention)
      
      itt_retention_X <- feols(
        retention_score_std ~ assign_t1 + assign_t2  + baseline_score + gender    | school_type,
        data = data_with_followup,
        cluster = ~school_type+school_id
      )
      
      
    }
    
    etable(itt_gained_X, itt_efficiency_X,  itt_retention_X )
    
  }
etable(itt_gained, itt_efficiency,  itt_retention )
etable(itt_efficiency_X, itt_efficiency, itt_retention_X,    itt_retention)
etable(itt_retention_X,itt_efficiency,  itt_efficiency_X, itt_retention)

# For efficiency

completers_real %>% filter(assign_t1 == 0 & assign_t2 == 0) %>% summarise(mean(learning_efficiency, na.rm = T))
# For retention
data_with_followup %>% filter(assign_t1 == 0 & assign_t2 == 0) %>% summarise(mean(retention_score_std))


#### 2. Data Cleaning  - Inputation ####


data_with_followup_inp = data_clean %>%   
  mutate(completed_followup = ifelse(is.na(postpost_id)==F, 1, 0) ) %>%
  mutate(postpost_Score = ifelse(is.na(postpost_id)==T, 0, postpost_id )) %>%
  # Merge relevant pre-test variables needed for controls
  select(pre_id, postpost_Score, assign_t1, assign_t2,
         school_id, baseline_score, gender, school_type)
# Standardize Retention Score using the control group's distribution
control_retention_stats_inp <- data_with_followup_inp %>%
  filter(assign_t1 == 0 & assign_t2 == 0) %>%
  summarise(mean_ret = mean(postpost_Score, na.rm = TRUE),
            sd_ret = sd(postpost_Score, na.rm = TRUE))

data_with_followup_inp <- data_with_followup_inp %>%
  mutate(
    retention_score_std = (postpost_Score - control_retention_stats$mean_ret) / control_retention_stats$sd_ret
  )

# Calculate the mean and SD of the control group's gain score

control_gain_stats_inp <- data_clean %>%
  filter(assign_t1 == 0 & assign_t2 == 0) %>%
  mutate(gain_score = post_Score - pre_Score) %>% # Assuming post_Score and pre_Score exist
  mutate(gain_score = ifelse(is.na(gain_score)==T, 0, gain_score )) %>%
  summarise(mean_gain = mean(gain_score, na.rm = TRUE),
            sd_gain = sd(gain_score, na.rm = TRUE))

# Standardize Gained Financial Literacy for all completers
completers_inp <- data_clean %>%
  mutate( gain_score = post_Score - pre_Score )%>%
  mutate(gain_score = ifelse(is.na(gain_score)==T, 0, gain_score )) %>%
  mutate(gained_lit_std = (gain_score - control_gain_stats$mean_gain) / control_gain_stats$sd_gain )
 

hist(completers_inp$gain_score,6)
hist(completers_inp$gained_lit_std,6)

# Create Learning Efficiency
# Assuming `post_interviewtime` is the time spent in seconds, convert to minutes
summary(completers_inp$post_interviewtime)
summary(completers_inp['post_interviewtime'])


completers_real = completers_inp
if (1==1) {
  
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
  control_retention_stats_inp <- data_with_followup_inp %>%
    filter(assign_t1 == 0 & assign_t2 == 0) %>%
    summarise(mean_ret = mean(postpost_Score, na.rm = TRUE),
              sd_ret = sd(postpost_Score, na.rm = TRUE))
  
  data_with_followup_inp <- data_with_followup_inp %>%
    mutate(
      retention_score_std = (postpost_Score - control_retention_stats$mean_ret) / control_retention_stats$sd_ret
    )
  
  
  # --- Step 2: Run the Regressions ---
  summary(completers_real$learning_efficiency_std)
  completers_real$learning_efficiency_std = ifelse(
    is.na(completers_real$learning_efficiency_std)==T, 0, completers_real$learning_efficiency_std
  )
  ##### Model for Learning Efficiency ####
  itt_efficiency_inp <- feols(
    learning_efficiency_std ~ assign_t1 + assign_t2  | school_id,
    data = completers_real,
    cluster = ~school_type
  )

  itt_efficiency_inp_X <- feols(
    learning_efficiency_std ~ assign_t1 + assign_t2 + baseline_score + gender  | school_id,
    data = completers_real,
    cluster = ~school_type
  )
  summary(itt_efficiency_inp_X )
  summary(itt_efficiency_inp )
  
  ##### Model for Gained learning ####
  
  itt_gained_inp <- feols(
    gain_score ~ assign_t1 + assign_t2 | school_type,
    data = completers_real,
    cluster = ~school_type+school_id
  )
  itt_gained_inp_X <- feols(
    gain_score ~ assign_t1 + assign_t2 + baseline_score + gender | school_type,
    data = completers_real,
    cluster = ~school_type+school_id
  )
  


    #####  Model for Knowledge Retention  ####
# completers_real$retention_score_std = ifelse(
#   is.na(completers_real$retention_score_std)==T, 0, 
#   completers_real$retention_score_std
# )

itt_retention_inp <- feols(
  retention_score_std ~ assign_t1 + assign_t2 | school_id+school_type,
  data = data_with_followup_inp,
  cluster = ~school_type+school_id
)
summary(itt_retention)

itt_retention_inp_X <- feols(
  retention_score_std ~ assign_t1 + assign_t2 + baseline_score + gender  | school_id,
  data = data_with_followup_inp,
  cluster = ~school_type
)
summary(itt_retention_X)



# --- Step 3: View Results and Populate Table ---


}
etable(itt_retention_inp_X)
etable(itt_retention_inp_X,   itt_efficiency_inp_X,itt_gained_inp_X ,  itt_retention_X, itt_efficiency_X, itt_gained_X, tex = T)
etable(itt_gained_inp ,itt_gained_inp_X )