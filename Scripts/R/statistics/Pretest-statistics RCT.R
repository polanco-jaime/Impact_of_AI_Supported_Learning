#### 1. General Setting ####
general_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Impact_of_AI_Supported_Learning/"

source(paste0(general_path,"Scripts/R/General_settings.R" ))

fractions_by_group(data, 'pre_Q1')
fractions_by_group(data, 'post_Q1')
fractions_by_group(data, 'postpost_Q1')
colnames(data)[16] ='Q1'

#### 2. Checking randomization #### 

number_by_group(data, 'Q1')

fractions_by_group(data, 'Q1')

fractions_by_group_print(table_= data ,
                         column= 'Q1',
                         filename = "preteset_treatment" ) 

#### 3. Personal information cleaning ####

##### 3.2 Gender  ####
data$pre_G01Q03 
Gender = generate_perception_plot(data[data$pre_G01Q03!='', ], 'pre_G01Q03', 'Q1', 
                         "Gender Distribution by Groups", separate = F)
ggsave(file.path('Graph/', "gender_distribution.png"), Gender$plot)
write.csv(Gender$data, file.path('Metadata/', "gender_distribution.csv"), row.names = FALSE)

##### 3.3 Municipality  ####
data$pre_G01Q04
generate_perception_plot(data , 'pre_G01Q04', 'Q1', 
                         "City", separate = F)
##### 3.4 School Name  ####
data$pre_G01Q05
toupper(data$pre_G01Q05)
##### 3.5 Type of School  ####
table(data$pre_G01Q07)
type_school = generate_perception_plot(data , 'pre_G01Q07', 'Q1', 
                                       'Type of School', separate = F)

save_perception_plot_metadata(type_school , 'Type of School')

##### 3.6 "Secondary School Field of Study"   ####
data$pre_G01Q06 = categorize_education(data$pre_G01Q06)
table(data$pre_G01Q06)
field_study = generate_perception_plot(data , 'pre_G01Q06', 'Q1', 
              "Secondary School Field of Study", separate = F)

save_perception_plot_metadata(field_study , "Secondary School Field of Study")
#### 4. Academic performance and Home Enviroment #### 
##### 4.1 Last Dutch Grade (Previous School Year) ####
table(data$pre_G02Q01)
Graph_ = generate_perception_plot(data , 'pre_G02Q01', 'Q1', 
                                  "Last Dutch Grade", separate = F)
save_perception_plot_metadata(Graph_,  "Last Dutch Grade")
 

##### 4.2 Last Math Grade (Previous School Year) ####
table(data$pre_G02Q012)
Graph_ = generate_perception_plot(data , 'pre_G02Q012', 'Q1', 
                         "Last Math Grade", separate = F)
save_perception_plot_metadata(Graph_,  "Last Math Grade")
##### 4.3 Predominant Language Used at Home  ####
table(data$pre_G02Q02)
data$pre_G02Q02 = translate_responses(data$pre_G02Q02, translations)

Graph_ = generate_perception_plot(data , 'pre_G02Q02', 'Q1', 
                         "Language Used at Home", separate = F)

save_perception_plot_metadata(Graph_,  "Language Used at Home")

##### 4.4 Parents' educational level  ####
Graph_ =generate_perception_plot(data,    'pre_G02Q031',    'Q1',   
                         "Parents' educational level", separate = F)
save_perception_plot_metadata(Graph_,   "Parents' educational level")

##### 4.5 Siblings at home ####
table(data$pre_G02Q04)
data$pre_G02Q04 = ifelse(data$pre_G02Q04=="3 of meer", "3 or more", data$pre_G02Q04)
Graph_ =generate_perception_plot(data,    'pre_G02Q04',    'Q1',   
                                 "Siblings at home", separate = F)
save_perception_plot_metadata(Graph_,   "Siblings at home")

##### 4.6 Frequency of Asking Teachers for Help ####
table(data$pre_G02Q08)
data  =data %>%
  mutate(
    # First, translate the values
    pre_G02Q08 = case_when(
      pre_G02Q08 == "Altijd" ~ "Always",
      pre_G02Q08 == "Nooit" ~ "Never",
      pre_G02Q08 == "Soms" ~ "Sometimes",
      pre_G02Q08 == "Vaak" ~ "Often",
      pre_G02Q08 == "Zelden" ~ "Rarely",
      TRUE ~ NA_character_ # Catches any other unexpected values or NAs
    ) 
  )

Graph_ =generate_perception_plot(data,    'pre_G02Q08',    'Q1',   
                                 "Frequency of Asking Teachers for Help", separate = F)
save_perception_plot_metadata(Graph_,   "Frequency of Asking Teachers for Help")
##### 4.7 Statistics Learning Style #### 
table(data$pre_LS_Mumford)
Graph_ =generate_perception_plot(data,    'pre_LS_Mumford',    'Q1',   
                                 "Honey and Mumford's Learning Styles", separate = F)
save_perception_plot_metadata(Graph_,   "Honey and Mumford's Learning Styles")
#### 5. Statistics self-perception #### 
 
fractions_by_perception(data, 'pre_PT01[SQ001]', 'Do you think taxes are fair in your country?')

fractions_by_perception(data, 'pre_PT01[SQ002]', 'Do you think people in your country\n know a lot about taxes?')

fractions_by_perception(data, 'pre_PT01[SQ003]', 'Taxes are essential for financing public services.')

fractions_by_perception(data, 'pre_PT01[SQ004]', 'In general, I feel comfortable doing calculations with numbers')

fractions_by_perception(data, 'pre_PT01[SQ005]', 'I expect AI can help me learn about taxes.')

# By treatment state
main_base <-  'Do you think taxes are fair in your country?'
Graph_ =generate_perception_plot(data , 'pre_PT01[SQ001]', 'Q1', 
                         main_base, separate = F)
 
save_perception_plot_metadata(Graph_,  "taxes are fair")


main_base <- "I expect AI can help me learn about taxes."
Graph_ =generate_perception_plot(data , 'pre_PT01[SQ005]', 'Q1', 
                         main_base, separate = F)
save_perception_plot_metadata(Graph_,  "AI can help")
#### 8 Outcomes results #### 
test_distibution_score(data, 'pre_Score', 'data')
# Create the density plot 

library(tidyverse)
library(knitr) # For kable, if you want an R-based table output for checking
library(kableExtra) # For more advanced kable formatting

# --- Example Usage (Assuming 'data' dataframe exists) ---
# Make sure Q1 is a factor with appropriate levels if it's not already
# If Q1 is numeric 1, 2, 3:
if (!is.factor(data$Q1)) {
  data$Q1 <- factor(data$Q1, levels = c("1", "2", "3")) # Ensuring order
}

statistics_cols <- c(
  "pre_Score", 
  'pre_AI Attitude and Motivation',
  "pre_Attitude and Motivation",
  "pre_Learning Experience & User Experience",
  "pre_Self-Regulation & Metacognition",
  "pre_Engagement & Commitment",
  "pre_Self-Confidence & Self-Efficacy",
  "pre_Emotional & Psychological Factors"
)

# Generate LaTeX and print
latex_table_string <- descriptive_stats_by_group(
  data_df = data, 
  score_cols = statistics_cols, 
  group_col = "Q1",
  latex_caption = "Descriptive Statistics for Baseline Psychosocial Outcomes by Group",
  latex_label = "tab:baseline_outcomes_balance_paper_generated",
  latex_notes = "Values are Mean (Standard Deviation). Scales are 1-5 Likert unless otherwise noted. N might vary per row due to missing data handling.",
  save_to_file_path = "Tables/baseline_descriptives.tex" # Optional: specify path
)

cat(latex_table_string)
#### 1. Testing continous variable ####


library(tidyverse)
library(broom)
library(stringr)

# Function to add significance stars (remains the same)
add_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01)  return("**")
  if (p_value < 0.05)  return("*")
  if (p_value < 0.1)   return(".")
  return("")
}

existing_vars <- statistics_cols[statistics_cols %in% colnames(data)]

latex_balance_table_string <- baseline_balance_tests_to_latex(
  data_df = data,
  vars_to_test = existing_vars,
  group_col = "Q1",
  control_group_level = "1",
  t1_level = "2",
  t2_level = "3",
  latex_caption = "Baseline Balance Tests for Key Pre-Treatment Variables",
  latex_label = "tab:baseline_balance_detailed"
)
cat(latex_balance_table_string)

#### 2. Testing categorical variable ####
add_stars_pval_only <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01)  return("**")
  if (p_value < 0.05)  return("*")
  return("")
}

 
# --- Example Usage ---
# Define your dictionary (named vector)
var_dictionary <- c(
  `pre_G01Q03` = "Gender",
  `pre_G01Q07` = "Type of School",
  `pre_G01Q06` = "Secondary School Field of Study",
  `pre_G02Q01` = "Last Dutch Grade",
  `pre_G02Q012` = "Last Math Grade",
  `pre_G02Q02` = "Language Used at Home",
  `pre_G02Q031` = "Parents' Educational Level",
  `pre_G02Q04`=    "Siblings at home", 
  `pre_PT01[SQ001]` = "Taxes are fair in your country?", # Question mark might be tricky for some LaTeX compilers if not escaped
  `pre_PT01[SQ005]` = "AI can help learn about taxes?",
  `pre_LS_Mumford` = "Learning Style (Mumford)",
  `pre_G02Q08` =   "Frequency of Asking Teachers for Help",
  `G02Q08` =   "Frequency of Asking Teachers for Help",
  `pre_st` = "Schools with Teacher Shortages"
  # Add any other pre_ variables you want to display nicely
)

categorical_baseline_vars_to_test <- c(
  "pre_G01Q03", 
  "pre_G01Q06",
  "pre_G01Q07", 
  "pre_G02Q01",
  "pre_G02Q012", 
  "pre_G02Q02", 
  "pre_G02Q031",
  "pre_G02Q04",
  "pre_G02Q08",
  "pre_PT01[SQ001]", 
  "pre_PT01[SQ005]",
  "pre_LS_Mumford",
  "pre_st"
)

  
                             
table(data$pre_st)
existing_categorical_vars <- categorical_baseline_vars_to_test[categorical_baseline_vars_to_test %in% colnames(data)]

latex_cat_balance_table <- baseline_balance_categorical_to_latex(
      data_df = data,
      categorical_vars_to_test = categorical_baseline_vars_to_test,
      group_col = "Q1",
      var_name_dictionary = var_dictionary,
      latex_caption = "Baseline Balance Tests for Categorical Characteristics",
      latex_label = "tab:baseline_balance_cat_detailed",
      save_to_file_path = "Tables/baseline_balance_categorical.tex"
)
cat(latex_cat_balance_table)

 
