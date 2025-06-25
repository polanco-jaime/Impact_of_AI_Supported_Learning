general_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Impact_of_AI_Supported_Learning/"
library(sandwich) 
source(paste0(general_path,"Scripts/R/cleaning/Merge-pre-post.R" ))


fractions_by_group(data, 'pre_Q1')
fractions_by_group(data, 'post_Q1')
fractions_by_group(data, 'postpost_Q1')
colnames(data)[16] ='Q1'
# time from post to postpost
data$time_post_treatment =data$postpost_startdate - data$post_startdate
summary(lm(data=data, pre_Score ~ factor(Q1) ))

summary(lm(data=data, post_Score ~ factor(Q1)+pre_Score ))
summary(lm(data=data, postpost_Score ~ factor(Q1)+pre_Score )) #*time_post_treatment


# arrow::write_parquet(data, "Data/data.parquet")

summary(lm(data=data, post_Score ~ factor(pre_Q1)*pre_st + pre_Score))
summary(lm(data=data, postpost_Score ~ factor(pre_Q1)*pre_st + pre_Score))



#### Outcome  ATT ####

fractions_by_group(merged_data, 't0_Q1')

fractions_by_group_print(table_= merged_data ,
                         column= 't0_Q1',
                         filename = "merged_fractions" ) 


fractions_by_group_print(table_= merged_data[merged_data$duplicates_merge==F,] ,
                         column= 't0_Q1',
                         filename = "merged_fractions_no_duplicates" ) 
  
fractions_by_group(merged_data[merged_data$duplicates_merge==F,], 't0_Q1')

fractions_by_group(merged_data[merged_data$t_AT==F,], 't0_Q1')
fractions_by_group(merged_data[merged_data$t0_st==F,], 't0_Q1')

number_by_group(merged_data, 't0_Q1')

number_by_group(merged_data[merged_data$duplicates_merge==F,], 't0_Q1')


##### 1.1 Gained Financial Learning ####
###### Overall ATE #### 
library(lmtest)
library(sandwich)
summary(lm(data=merged_data, t_Score ~ factor(t0_Q1) + t0_Score))
summary(lm(data=merged_data, gained_tax_learning ~ factor(t0_Q1) ))
model_itt = (lm(data=merged_data, gained_tax_learning ~ factor(t0_Q1) ))
robust_summary_itt <- coeftest(model_itt, vcov. = vcovHC(model_itt, type = "HC2"))



summary(lm(data=merged_data[merged_data$t_AT==F ,], gained_tax_learning ~ factor(t0_Q1) ))


summary(lm(data=merged_data, t_Score ~ factor(t0_Q1) * t0_st ))
summary(lm(data=merged_data, t_Score ~ factor(t0_Q1) * t0_st ))

summary(lm(data=merged_data, gained_tax_learning ~ factor(t0_Q1) * t0_st ))
summary(lm(data=merged_data, t_Score ~ factor(t0_Q1) * t0_st + t0_Score))
summary(lm(data=merged_data, t_Score ~ factor(t0_Q1) * t0_st + factor(t0_Q1) * t0_Score))

merged_data= merged_data[merged_data$duplicates_merge==F ,]
# merged_data= merged_data[merged_data$t_AT==F ,]
save_analysis_output(merged_data, 
                     "gained_tax_learning",
                     "t0_Q1",
                     "Gained Tax Learning")
a= merged_data[ merged_data$gained_tax_learning!=0.000,]
# a = a[a$gained_tax_learning<=0.1,]
summary(a[a$gained_tax_learning<=0.1,]$t0_Score)

summary(a[a$gained_tax_learning<=0.1,]$t_Score)
save_analysis_output(merged_data, 
                     "gained_tax_learning",
                     "t0_Q1",
                     "Gained Tax Learning")
summary(lm(data=merged_data , gained_tax_learning~factor(t0_Q1)  ) )

save_analysis_output(data=merged_data ,
                     score_col="gained_tax_learning",
                     group_col= "t0_Q1",
                     analysis_name= "Standardized Improvement in\nFinancial Literacy by\n Treatment Group")
  
print(descriptive_stats_by_group(merged_data, "gained_tax_learning", "t0_Q1", latex = T))
plot_rct_density(merged_data, 
                 score_col = "gained_tax_learning", 
                 group_col = "t0_Q1",
                 "Standardized Improvement in\nFinancial Literacy by\n Treatment Group")


plot_rct_density(posttest, 
                 score_col = "Score", 
                 group_col = "Q1",
                 "Distribution of Standardized Improvement in\nFinancial Literacy by\n Treatment Group")
summary(lm(data=pretest, Score~factor(Q1)  ) )
 
summary(lm(data=posttest, Score~factor(Q1)  ) )

summary(lm(data=posttest[posttest$flag_duplicate_id_score==0,], Score~factor(Q1)  ) )
summary(lm(data=posttest[  posttest$groupTime14859>60,], Score~factor(Q1)  ) )

summary(lm(data=posttest[  posttest$groupTime14859>59
                           # & 
                             # posttest$AT==F &
                             # posttest$flag_duplicate_id==0
                           ,], Score~factor(Q1)  ) )

summary(lm(data=posttest[  posttest$groupTime14859>59 #& 
                             # posttest$AT==F &
                             # posttest$Score!=0 &
                             # posttest$flag_duplicate_id_score==0
                           ,], Score~factor(Q1)  ) )

summary(lm(data=merged_data, gained_tax_learning~factor(t0_Q1)  ) )

summary(lm(data=merged_data[merged_data$t0_Score<=median(merged_data$t0_Score),],
           gained_tax_learning~factor(t0_Q1)  ) )

summary(lm(data=merged_data[merged_data$t0_disventaged==1,],
           gained_tax_learning~factor(t0_Q1)  ) )

summary(lm(data=merged_data, gained_tax_learning~factor(t0_Q1)*factor(t0_LS_Mumford) ) )

summary(lm(data=merged_data, gained_tax_learning~factor(t0_Q1)*factor(t0_st) ) )

mean_=mean(merged_data[merged_data$t0_Q1=='1',]$gained_tax_learning )
summary(lm(data=merged_data, scale(gained_tax_learning, center = mean_ )~factor(t0_Q1)) )
###### 1.1.1 No shortages ####
table(merged_data$t0_st)
summary(lm(data=subset(merged_data,
                       merged_data$t0_st==T),
           gained_tax_learning~factor(t0_Q1)) )

plot_rct_density(subset(merged_data,
                        merged_data$t0_st==F), 
                 score_col = "gained_tax_learning", 
                 group_col = "t0_Q1",
                 "Distribution of Standardized Improvement in\nFinancial Literacy by\n Treatment Group")
summary(lm(data= merged_data ,
           gained_tax_learning~factor(t0_Q1) *  (t0_st)) )
###### 1.1.2 No Ambigous Treatment ####
summary(lm(data=subset(merged_data,
                       merged_data$t_AT==F),
           gained_tax_learning~factor(t0_Q1)) )
plot_rct_density(subset(merged_data,
                        merged_data$t_AT==F), 
                 score_col = "gained_tax_learning", 
                 group_col = "t0_Q1",
                 "Distribution of Standardized Improvement in\nFinancial Literacy by\n Treatment Group")

summary(lm(data=merged_data, gained_tax_learning~factor(t0_Q1)+factor(id_student2.x)) )

###### 1.1.3 Teacher shortages ####
table(merged_data$t_G01Q08)
TS_df = subset(merged_data,
               merged_data$t0_st==T|
                 merged_data$t_G01Q08=='In the regular class but not with my usual teacher'|
                 # merged_data$t_G01Q08=='At home'|
                 merged_data$t_G01Q08=='In study'
)
merged_data$teacher_shortages = ifelse( 
               merged_data$t0_st==T|
                 merged_data$t_G01Q08=='In the regular class but not with my usual teacher'|
                 # merged_data$t_G01Q08=='At home'|
                 merged_data$t_G01Q08=='In study',1,0
)
summary(lm(data=TS_df,
           gained_tax_learning~factor(t0_Q1)) )

save_analysis_output(data=TS_df,
                     score_col="gained_tax_learning",
                     group_col= "t0_Q1",
                     analysis_name= "Standardized Improvement in\nFinancial Literacy\n (Teacher Shortages)")

merged_data$t_Score
# t0_st
summary(lm(data= merged_data,
           gained_tax_learning~factor(t0_Q1)+teacher_shortages  ))
merged_data$t0_Score
summary(lm(data=subset(merged_data,
                       merged_data$t0_st==T),
           t_Score~factor(t0_Q1)*t0_st ) )
plot_rct_density(subset(merged_data,
                        merged_data$t0_st==T), 
                 score_col = "gained_tax_learning", 
                 group_col = "t0_Q1",
                 "Distribution of Standardized Improvement in\nFinancial Literacy by\n Treatment Group\nFor Teacher-Shortages School")
summary(lm(data=subset(merged_data,
                       merged_data$t0_st==T), gained_tax_learning~factor(t0_Q1) ) )
###### 1.1.4 Initial  Performance ####
p10=quantile(merged_data$t0_Score, probs = 0.25)
p90=quantile(merged_data$t0_Score, probs = 0.9)

summary(lm(data=subset(merged_data,
                       merged_data$t0_Score<=p10),
           gained_tax_learning~factor(t0_Q1) ) )

summary(lm(data=subset(merged_data,
                       merged_data$t0_Score>=p90),
           gained_tax_learning~factor(t0_Q1) ) )

plot_rct_density(subset(merged_data,
                        merged_data$t0_st==T), 
                 score_col = "gained_tax_learning", 
                 group_col = "t0_Q1",
                 "Distribution of Standardized Improvement in\nFinancial Literacy by\n Treatment Group\nFor Teacher-Shortages School")
##### 1.2 Gained Attitude and Motivation ####
print(descriptive_stats_by_group(merged_data, "Gained Attitude and Motivation", "t0_Q1", latex = T))
plot_rct_density(merged_data, 
                 score_col = "Gained Attitude and Motivation", 
                 group_col = "t0_Q1")
summary(lm(data=merged_data, `Gained Attitude and Motivation`~factor(t0_Q1)) )
##### 1.3 Gained Learning Experience & User Experience ####
print(descriptive_stats_by_group(merged_data, 
             "Gained Learning Experience & User Experience", "t0_Q1", latex = T))
plot_rct_density(merged_data, 
                 score_col = "Gained Learning Experience & User Experience", 
                 group_col = "t0_Q1")
summary(lm(data=merged_data, `Gained Learning Experience & User Experience`~factor(t0_Q1)) )

##### 1.4 Gained Self-Regulation & Metacognition ####
print(descriptive_stats_by_group(merged_data, 
                                 "Gained Self-Regulation & Metacognition", "t0_Q1", latex = T))
plot_rct_density(merged_data, 
                 score_col = "Gained Self-Regulation & Metacognition", 
                 group_col = "t0_Q1")
summary(lm(data=merged_data, `Gained Self-Regulation & Metacognition`~factor(t0_Q1)) )

##### 1.4 Gained Engagement & Commitment ####
print(descriptive_stats_by_group(merged_data, 
                                 "Gained Engagement & Commitment", "t0_Q1", latex = T))
plot_rct_density(merged_data, 
                 score_col = "Gained Engagement & Commitment", 
                 group_col = "t0_Q1")
summary(lm(data=merged_data, `Gained Engagement & Commitment`~factor(t0_Q1)) )



