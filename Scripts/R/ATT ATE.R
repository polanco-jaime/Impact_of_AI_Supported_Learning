# Running full clenaing #
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/General_settings.R")
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/function analysis.R")
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/Pretest-cleaning.R")
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/postest_cleaning.R")

posttest$AT
colnames(pretest)
output = c("Score","Q1",
 "Attitude and Motivation","Learning Experience & User Experience",
 "Self-Regulation & Metacognition","Engagement & Commitment",
  "Self-Confidence & Self-Efficacy","Emotional & Psychological Factors",
 "id_student"   )
pretest_selected = pretest[, c(output,"LS_Mumford", "st","disventaged") ]
posttest_selected = posttest[, c(output, 'AT')]
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


no_pretest= c("VAN_DE_VENSTER_FRAUKE","OSAKPAMWAN_AGHO", 
              "LEON_VERFAILLIE", "GAERDELEN_MONA", 
              "RANIA_UNLU", "Y('RZ", "THOMAS_DE_GREEF",
              "PAULINE_INGHELBRECHT", "ROOSE_ROBIN", "BERHEM_DURMAZ",
              "JIETER_PAN", "TIEBE_VORSTER", "ROBERT_VERSTRAELEN",  #2025-03-24
              "CHRIS_EMERY_TRAZIE", 'FOLLON_MATS','MAES_PAULINE',
              'ISABELLE_SARRAZYN','VERMEIRE_KAAT',
              'THOR_SULS', 'BLUEKENS_DAAN',
              'ALEXIA_VAN_ANTWERPEN',
              'ENRIQUE_JOOSEN','SEPPE_LIBIEZ' ,
              'FREDERIK',
              'BERT_VANLERBERGHE',  ## Since 27-03 Nostra spes
              'JUSTINAS_LANKELIS',
              'GOUTSMET_SAMUEL',
              'CHRISTIAENS_HAN',
              'PHEBE_CLAEYS', 
              'DE_CORTE_LIANA', 
              'MOKHTARI_ELIAS',
              "EL_MAZZOUJI_KAWTAR",
              "ADAM_LAURENCE",
              "MAARTEN_VELGHE",
              "ALEXANDRA_CALLEWAERT",
              "ANAÏS_COUCKE",
              "RONSSE_REBECCA",
              "VAN_DEN_BOGAERT_YANA",
              "HANNE_AMEYE",
              "FILLE_VANDEWEYER",
              "FERRE_DISTELMANS"  , ## 31 -03- 2025
           "NM",
           "YANA_JIMENEZ_LOPEZ"
              )
homonims = c('JANSEN_JAN')
posttest <- subset(posttest, !(G01Q01 %in% no_pretest))
posttest_only <- anti_join(posttest, pretest, by = "id_student")

as.data.frame(posttest_only$G01Q01)




filter_and_select(pretest, "ANAS_KARAMZIANI")  
filter_and_select(posttest,"ANAS_KARAMZIANI") 

#### name fixing ####  
posttest$G01Q01 = fix_names(posttest_name = "CLEO",
                            pretest_name = "JIMENEZ_LOPEZ_CLÉO",
                            posttest, "G01Q01")
 
#### Treatment fixing ####
posttest$Q1 = ifelse(posttest$G01Q01=="VAN_HOEY_TESS",'3',posttest$Q1)
 

#### School fixing #### 
posttest$G01Q05 = ifelse(posttest$G01Q01=="ANAS_KARAMZIANI",
                         'Moretus Ekeren',
                         posttest$G01Q05)

#### Outcome preparation ATT ####

merged_data <- merge(pretest_selected, posttest_selected, by = "id_student", all = FALSE)
# Add a 'duplicates_merge' column to merged_data
library(dplyr)
merged_data <- merged_data %>%
  group_by(id_student) %>%
  mutate(duplicates_merge = ifelse(n() > 1, TRUE, FALSE)) %>%
  ungroup()  # Ungroup to avoid affecting further operations

table(merged_data$duplicates_merge)

table(merged_data$t0_st)
merged_data$gained_tax_learning = scale(merged_data$t_Score - merged_data$t0_Score )
merged_data$gained_tax_learning =  (merged_data$t_Score) -  (merged_data$t0_Score )
merged_data$`Gained Attitude and Motivation` = merged_data$`t_Attitude and Motivation` - merged_data$`t0_Attitude and Motivation`
merged_data$`Gained Learning Experience & User Experience` = merged_data$`t_Learning Experience & User Experience` - merged_data$`t0_Learning Experience & User Experience`
merged_data$`Gained Self-Regulation & Metacognition` = merged_data$`t_Self-Regulation & Metacognition` - merged_data$`t0_Self-Regulation & Metacognition`
merged_data$`Gained Engagement & Commitment` = merged_data$`t_Engagement & Commitment`- merged_data$`t0_Engagement & Commitment`

summary(merged_data$t_Score)

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
save_analysis_output(merged_data[merged_data$duplicates_merge==F & merged_data$t_AT==F,], 
                     "gained_tax_learning",
                     "t0_Q1",
                     "Gained Tax Learning")
summary(lm(data=merged_data[merged_data$duplicates_merge== F & merged_data$t_AT==F ,], gained_tax_learning~factor(t0_Q1)  ) )

save_analysis_output(data=merged_data,
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
                       merged_data$t0_st==F),
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

summary(lm(data=merged_data, gained_tax_learning~factor(t0_Q1)+factor(id_student)) )

###### 1.1.3 Teacher shortages ####
summary(lm(data=subset(merged_data,
                       merged_data$t0_st==T),
           gained_tax_learning~factor(t0_Q1)) )
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



