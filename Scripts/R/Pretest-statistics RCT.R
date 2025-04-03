


#### 2. Checking randomization #### 

number_by_group(pretest, 'Q1')

fractions_by_group(pretest, 'Q1')

fractions_by_group_print(table_= pretest ,
                         column= 'Q1',
                         filename = "preteset_treatment" ) 

#### 3. Personal information cleaning ####

##### 3.2 Gender  ####
pretest$G01Q03 
Gender = generate_perception_plot(pretest[pretest$G01Q03!='', ], 'G01Q03', 'Q1', 
                         "Gender Distribution by Groups", separate = F)
ggsave(file.path('Graph/', "gender_distribution.png"), Gender$plot)
write.csv(Gender$data, file.path('Metadata/', "gender_distribution.csv"), row.names = FALSE)

##### 3.3 Municipality  ####
pretest$G01Q04
generate_perception_plot(pretest , 'G01Q04', 'Q1', 
                         "City", separate = F)
##### 3.4 School Name  ####
pretest$G01Q05
toupper(pretest$G01Q05)
##### 3.5 Type of School  ####
table(pretest$G01Q07)
type_school = generate_perception_plot(pretest , 'G01Q07', 'Q1', 
                         "Type of School", separate = F)

save_perception_plot_metadata(type_school , 'Type of School')


#### 4. Academic performance and Home Enviroment #### 
##### 4.1 Last Dutch Grade (Previous School Year) ####
table(pretest$G02Q01)
Graph_ = generate_perception_plot(pretest , 'G02Q01', 'Q1', 
                                  "Last Dutch Grade", separate = F)
save_perception_plot_metadata(Graph_,  "Last Dutch Grade")
 

##### 4.2 Last Math Grade (Previous School Year) ####
table(pretest$G02Q012)
Graph_ = generate_perception_plot(pretest , 'G02Q012', 'Q1', 
                         "Last Math Grade", separate = F)
save_perception_plot_metadata(Graph_,  "Last Math Grade")
##### 4.3 Predominant Language Used at Home  ####
table(pretest$G02Q02)
pretest$G02Q02 = translate_responses(pretest$G02Q02, translations)

Graph_ = generate_perception_plot(pretest , 'G02Q02', 'Q1', 
                         "Language Used at Home", separate = F)

save_perception_plot_metadata(Graph_,  "Language Used at Home")

####  
Graph_ =generate_perception_plot(pretest,    'G02Q031',    'Q1',   
                         "Parents' educational level", separate = F)
save_perception_plot_metadata(Graph_,   "Parents' educational level")
#### 4. Statistics Learning Style #### 


#### 5. Statistics self-perception #### 
 
fractions_by_perception(pretest, 'PT01[SQ001]', 'Do you think taxes are fair in your country?')

fractions_by_perception(pretest, 'PT01[SQ002]', 'Do you think people in your country\n know a lot about taxes?')

fractions_by_perception(pretest, 'PT01[SQ003]', 'Taxes are essential for financing public services.')

fractions_by_perception(pretest, 'PT01[SQ004]', 'In general, I feel comfortable doing calculations with numbers')

fractions_by_perception(pretest, 'PT01[SQ005]', 'I expect AI can help me learn about taxes.')

# By treatment state
main_base <-  'Do you think taxes are fair in your country?'
Graph_ =generate_perception_plot(pretest , 'PT01[SQ001]', 'Q1', 
                         main_base, separate = F)
 
save_perception_plot_metadata(Graph_,  "taxes are fair")


main_base <- "I expect AI can help me learn about taxes."
Graph_ =generate_perception_plot(pretest , 'PT01[SQ005]', 'Q1', 
                         main_base, separate = F)
save_perception_plot_metadata(Graph_,  "AI can help")
#### 8 Outcomes results #### 
test_distibution_score(pretest, 'Score', 'pretest')
# Create the density plot 

descriptive_stats_by_group <- function(data, score_cols, group_col, latex = FALSE, save_to_file = FALSE) {
  # Subset data with selected columns
  data <- data[, c(group_col, score_cols)]
  data <- na.omit(data)
  
  # Rename columns
  colnames(data)[1] <- 'group'
  
  # Define group labels
  group_labels <- c(
    "1" = "Control Group: Traditional Path (TP)",
    "2" = "Treatment: Reduced TP + AI",
    "3" = "Treatment: Tailored AI in the Belgian Tax System"
  )
  
  # Calculate descriptive statistics for multiple columns
  group_stats <- data %>%
    pivot_longer(cols = -group, names_to = "variable", values_to = "score") %>%
    group_by(group, variable) %>%
    summarize(
      mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(group_label = group_labels[as.character(group)])
  
  # Reshape data so groups are columns
  table_data <- group_stats %>%
    pivot_wider(names_from = group_label, values_from = c(mean_score, sd_score))
  
  # Get the unique group labels for the table header
  unique_groups <- unique(group_stats$group_label)
  
  # Create LaTeX output
  latex_lines <- c()
  latex_lines <- c(latex_lines, "\\begin{tabular}{l", paste(rep("c", length(unique_groups)), collapse = ""), "}")
  latex_lines <- c(latex_lines, "\\hline")
  latex_lines <- c(latex_lines, paste("Variable", paste(unique_groups, collapse = " & "), "\\\\"))
  latex_lines <- c(latex_lines, "\\hline")
  
  for (row in 1:nrow(table_data)) {
    row_name <- table_data$variable[row]
    row_values <- sapply(unique_groups, function(group) {
      mean_col <- paste0("mean_score_", group)
      sd_col <- paste0("sd_score_", group)
      if (mean_col %in% colnames(table_data) & sd_col %in% colnames(table_data)) {
        paste0(sprintf("%.2f", table_data[[mean_col]][row]), " (", sprintf("%.2f", table_data[[sd_col]][row]), ")")
      } else {
        "NA (NA)"
      }
    })
    latex_lines <- c(latex_lines, paste(row_name, paste(row_values, collapse = " & "), "\\\\"))
  }
  
  latex_lines <- c(latex_lines, "\\hline")
  latex_lines <- c(latex_lines, "\\end{tabular}")
  
  # Save to file if requested
  if (save_to_file) {
    dir.create("Tables", showWarnings = FALSE)
    writeLines(latex_lines, "Tables/descriptive_stats.tex")
  }
  
  return(paste(latex_lines, collapse = "\n"))
}

library(tidyverse)
colnames(pretest)
statistics = c("Score", "Attitude and Motivation" ,
  "Learning Experience & User Experience" ,
  "Self-Regulation & Metacognition"  ,   
  "Engagement & Commitment", 
  "Self-Confidence & Self-Efficacy" ,
  "Emotional & Psychological Factors")
descriptive_stats_by_group(pretest, statistics, "Q1", latex = TRUE, save_to_file = TRUE)

a =  (descriptive_stats_by_group(pretest, statistics, "Q1", latex = T,
                                 "descriptive_stats")) 
cat(a )







save_analysis_output(data=pretest,
                     score_col="Score",
                     group_col= "Q1",
                     analysis_name= "Baseline Financial Literacy by Group")


test_distibution_score(subset(pretest, pretest$Q1==1), 'Score', 'pretest', '')
test_distibution_score(subset(pretest, pretest$Q1==2), 'Score', 'pretest')
test_distibution_score(subset(pretest, pretest$Q1==3), 'Score', 'pretest')
summary(lm(data=pretest, Score~factor(Q1)) )

summary(lm(data=merged_data, t0_Score ~factor(t0_Q1)) )
plot_rct_density(merged_data, "t0_Score", "t0_Q1", "")


summary(lm(data=subset( pretest, pretest$to_drop==0), Score~factor(Q1)) )

subset( pretest, pretest$st==F)

print(descriptive_stats_by_group(pretest, "Score", "Q1", latex = T))
pretest$Treatment_state_i = ifelse(pretest$Q1==1, 0, 1)

##### Knowledge Score ####
## Are there difference between control assigment and treatment assigment at baseline?
plot_rct_density(pretest, score_col = "Score", group_col = "Q1", "")
summary(lm(data=pretest, Score~factor(Treatment_state_i)) )
summary(lm(data=pretest, Score~factor(Q1)) )

summary(lm(data=pretest[ pretest$st==F,], Score~factor(Q1)) )
summary(lm(data=pretest[ pretest$st==F,], Score~factor(Treatment_state_i)) )

##### Measure Emotional Attitudes ####
###### 8.1 Attitude and Motivation ####
pretest[["Attitude and Motivation"]]
plot_rct_density(pretest, score_col = "Attitude and Motivation", group_col = "Q1", "")
print(descriptive_stats_by_group(postest, "Attitude and Motivation", "Q1", latex = T))

summary(lm(data=pretest, `Attitude and Motivation`~factor(Q1)) )

###### 8.2 Learning Experience & User Experience ####
hist(pretest$`Learning Experience & User Experience`)
save_analysis_output(data=pretest,
                     score_col = "Learning Experience & User Experience",
                     group_col= "Q1",
                     analysis_name= "Baseline\nLearning Experience & User Experience")



summary(lm(data=pretest, `Learning Experience & User Experience`~factor(Q1)) )


###### 8.3 Self-Regulation & Metacognition ####
hist(pretest$`Self-Regulation & Metacognition`)
save_analysis_output(data=pretest,
                     score_col = "Self-Regulation & Metacognition",
                     group_col= "Q1",
                     analysis_name= "Baseline -Self-Regulation & Metacognition")



plot = plot_rct_density(pretest, score_col = "Self-Regulation & Metacognition", group_col = "Q1")
ggsave(filename = 'Graph/Baseline -Self-Regulation & Metacognition.png', plot = plot, width = 8, height = 6, units = "in") 
linear_model <- summary(lm(data=pretest, `Self-Regulation & Metacognition`~factor(Q1)) )
linear_model_summary <- capture.output(summary(linear_model))


print(descriptive_stats_by_group(pretest, "Self-Regulation & Metacognition",
                                 "Q1", latex = T))
 


###### 8.4 Engagement & Commitment ####

hist(pretest$`Engagement & Commitment`)
plot_rct_density(pretest, score_col = "Engagement & Commitment", group_col = "Q1")
print(descriptive_stats_by_group(pretest,"Engagement & Commitment",
                                 "Q1", latex = T))
summary(lm(data=pretest, `Engagement & Commitment`~factor(Q1)) )

###### 8.5 Self-Confidence & Self-Efficacy ####
hist(pretest$`Self-Confidence & Self-Efficacy`)
plot_rct_density(pretest, score_col = "Self-Confidence & Self-Efficacy", group_col = "Q1")
print(descriptive_stats_by_group(pretest,"Self-Confidence & Self-Efficacy",
                                 "Q1", latex = T))
summary(lm(data=pretest, `Self-Confidence & Self-Efficacy`~factor(Q1)) )


###### 6.3.6 Emotional & Psychological Factors ####
hist(pretest$`Emotional & Psychological Factors`)
 
plot_rct_density(pretest, score_col = "Emotional & Psychological Factors", group_col = "Q1")
print(descriptive_stats_by_group(pretest, "Emotional & Psychological Factors",
                                 "Q1", latex = T))
summary(lm(data=pretest, `Emotional & Psychological Factors`~factor(Q1)) )



