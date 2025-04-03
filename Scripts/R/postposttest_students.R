source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/General_settings.R")
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/function analysis.R")
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/Pretest-cleaning.R")
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/postest_cleaning.R")

postpost_test = posttest[is.na(posttest$G04Q24)==F, ] 
postpost_test$fortnight_category <- categorize_fortnight(postpost_test$submitdate)

table(postpost_test$fortnight_category)

colnames(pretest)
output = c("Score","Q1",
           "Attitude and Motivation","Learning Experience & User Experience",
           "Self-Regulation & Metacognition","Engagement & Commitment",
           "Self-Confidence & Self-Efficacy","Emotional & Psychological Factors",
           "id_student"   )
pretest_selected = pretest[, c(output,"LS_Mumford", "st","disventaged") ]
posttest_selected = postpost_test[, c(output, 'AT', "G04Q24")]
for (i in 1:ncol(pretest_selected)) {
  colnames(pretest_selected )[i] = paste0('t0_',colnames(pretest_selected )[i] )
}

for (i in 1:ncol(posttest_selected)) {
  colnames(posttest_selected )[i] = paste0('t_',colnames(posttest_selected )[i] )
}



#### Joining ####
table(posttest$AT)
colnames(posttest_selected )[9] = "id_student"
colnames(pretest_selected )[9] = "id_student"
#   * Use `anti_join` to find student_ids present in *posttest* but *not* in *pretest*

 
#### Outcome preparation ATT ####

merged_data <- merge(pretest_selected, posttest_selected, by = "id_student", all = FALSE)
table(merged_data$t0_st)
merged_data$gained_tax_learning = scale(merged_data$t_Score - merged_data$t0_Score )
merged_data$gained_tax_learning =  (merged_data$t_Score) -  (merged_data$t0_Score )
merged_data$`Gained Attitude and Motivation` = merged_data$`t_Attitude and Motivation` - merged_data$`t0_Attitude and Motivation`
merged_data$`Gained Learning Experience & User Experience` = merged_data$`t_Learning Experience & User Experience` - merged_data$`t0_Learning Experience & User Experience`
merged_data$`Gained Self-Regulation & Metacognition` = merged_data$`t_Self-Regulation & Metacognition` - merged_data$`t0_Self-Regulation & Metacognition`
merged_data$`Gained Engagement & Commitment` = merged_data$`t_Engagement & Commitment`- merged_data$`t0_Engagement & Commitment`

summary(merged_data$t_Score)

#### Outcome preparation ATT ####
merged_data$t_G04Q24
