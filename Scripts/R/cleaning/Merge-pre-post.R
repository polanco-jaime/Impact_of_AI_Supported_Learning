# Running full clenaing #
general_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Impact_of_AI_Supported_Learning/"

source(paste0(general_path,"Scripts/R/General_settings.R" ))
source(paste0(general_path,"Scripts/R/cleaning/Pretest-cleaning.R" ))
source(paste0(general_path,"Scripts/R/cleaning/postest-cleaning.R" ))
source(paste0(general_path,"Scripts/R/cleaning/Postposttest-cleaning.R" ))
 
for (i in 1:ncol(pretest)) {
  colnames(pretest )[i] = paste0('pre_',colnames(pretest )[i] )
}

for (i in 1:ncol(posttest)) {
  colnames(posttest )[i] = paste0('post_',colnames(posttest )[i] )
}
 
for (i in 1:ncol(postposttest)) {
  colnames(postposttest )[i] = paste0('postpost_',colnames(postposttest )[i] )
}

pretest$pre_id_student
posttest$post_id
postposttest$postpost_id
post_x_postpost = sqldf("
      SELECT * FROM posttest A
      LEFT JOIN postposttest B
      ON lower(post_G04Q24)  = lower(postpost_G04Q24)
      OR lower(post_id_student)=lower(postpost_id_student)
      OR lower(post_id_student2)=lower(postpost_id_student2)
      ")
 
 

data = sqldf::sqldf("
      --   SELECT * FROM (
             SELECT * FROM pretest
             LEFT JOIN post_x_postpost
             ON lower(post_id_student)=lower(pre_id_student)
             OR lower(post_id_student2)=lower(pre_id_student)
      -- )
      --- A
      -- LEFT JOIN postposttest
      -- ON lower(pre_id_student)=lower(postpost_id_student)
             ")
table(is.na(data$pre_id))
table(is.na(data$post_id))
table(is.na(data$postpost_id))

data = flag_last_duplicate(data, "pre_id_student")
arrow::write_parquet(data, "Data/data.parquet")
# 
# table(posttest$Q1)
# if (1==1) {
#   # posttest$AT
#   colnames(pretest)
#   output = c("Score","Q1", 
#              "id_student" , "id_student2"  )
#   pretest$gender = pretest$G01Q03
#   pretest$`Type of School` = pretest$G01Q07
#   pretest_selected = pretest[, c(output,"LS_Mumford", "st","disventaged", "gender", "Type of School") ]
#   
#   posttest_selected = postposttest[, c(output)]
#   for (i in 1:ncol(pretest_selected)) {
#     colnames(pretest_selected )[i] = paste0('t0_',colnames(pretest_selected )[i] )
#   }
# 
#   for (i in 1:ncol(posttest_selected)) {
#     colnames(posttest_selected )[i] = paste0('t_',colnames(posttest_selected )[i] )
#   }
# 
# 
# 
#   #### Joining ####
#   posttest_selected[[10]]
#   colnames(posttest_selected )[9] = "id_student"
#   colnames(pretest_selected )[9] = "id_student"
#   colnames(posttest_selected )[10] = "id_student2"
#   colnames(pretest_selected )[10] = "id_student2"
#   #   * Use `anti_join` to find student_ids present in *posttest* but *not* in *pretest*
# 
# 
#   no_pretest= c("VAN_DE_VENSTER_FRAUKE","OSAKPAMWAN_AGHO",
#                 "LEON_VERFAILLIE",
#                 "DAAN_VAN_DEN_VENNE",
#                 "AELBRECHT_TOON",
#                 "Y('RZ",
#                 "AHIE",
#                 "NM",
#                 "TEST",
#                 "LAURA_FIVÉ",
#                 "YANA_JIMENEZ_LOPEZ", # it is ambigus
#                 "BERT_VANLERBERGHE", #Nosa School but not in pretest
#                 "JUSTINAS_LANKELIS","GOUTSMET_SAMUEL","CHRISTIAENS_HAN","PHEBE_CLAEYS","DE_CORTE_LIANA",
#                 "MOKHTARI_ELIAS","EL_MAZZOUJI_KAWTAR","ZITA_DEHULLU","ADAM_LAURENCE","MAARTEN_VELGHE","ALEXANDRA_CALLEWAERT",
#                 "ANAÏS_COUCKE","RONSSE_REBECCA", "VAN_DEN_BOGAERT_YANA",
#                 "HANNE_AMEYE", "FILLE_VANDEWEYER", "FERRE_DISTELMANS","FOLLON_MATS","MAES_PAULINE",
#                 "VERMEIRE_KAAT", "THOR_SULS", "BLUEKENS_DAAN", "ALEXIA_VAN_ANTWERPEN", "ENRIQUE_JOOSEN",
#                 "SEPPE_LIBIEZ", "FREDERIK", "CHRIS_EMERY_TRAZIE", "JANSEN_JEFFKE", "JIETER_PAN","ROBERT_VERSTRAELEN",
#                 "BOMBARDINO_CROCODILO",
#                 "SALEMBIER_ILJO", "MESSELY_LUNA","LOÏC_NGOMBE"
# 
#   )
# 
#   homonims = c('JANSEN_JAN')
#   posttest <- subset(posttest, !(G01Q01 %in% no_pretest))
#   
# }


#### 1.No Joining ####

# posttest_only <- anti_join(postposttest, pretest, by = "id_student")
# as.data.frame(posttest_only$G01Q01)
# #
# filter_and_select(pretest, "VAN_HAMME_JADE")
# filter_and_select(posttest,"VAN_HAMME_JADE")
# filter_and_select(postposttest,"VAN_HAMME_JADE")
# posttest$G04Q24
# postposttest$postpost_G04Q24
# 
# 
# #
# # pretest[pretest$G01Q01=='ACHBARI_HAFSA',]$id_student ==
# # posttest[posttest$G01Q01=='ACHBARI_HAFSA',]$id_student
# # ###### 2.1.1 Fixing names ####
# postposttest$G01Q01 = fix_names(posttest_name = "WIKTOR_FRANCISZEK_BAZYLUK",
#                             pretest_name = "WIKTOR_BAZYLUK",
#                             postposttest, "G01Q01")
# 
# posttest$G01Q01 = fix_names(posttest_name = "TIEBE_VORSTER",
#                             pretest_name = "VORSTERS_TIEBE",
#                             posttest, "G01Q01")
# #
# # ###### 2.2 Fixing treatment group ####
# posttest$Q1 = ifelse(posttest$G01Q01=="MARIAM_ELSORNGAWY",'2',posttest$Q1)
# 
# posttest$AT = ifelse(posttest$G01Q01=="MARIAM_ELSORNGAWY",T,posttest$AT )
# #
# #
# # ###### 2.4.1 Fixing school name ####
# postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_WINTER_ELINE",
#                          'Sintpaulus Instituut',
#                          postposttest$G01Q05)
# 
# # #### Outcome preparation ATT ####
# # colnames(pretest_selected)
# # merged_data <- merge(pretest_selected, posttest_selected, by = "id_student", all = FALSE)
# # # Add a 'duplicates_merge' column to merged_data
# # library(dplyr)
# # 
# # merged_data <- merged_data %>%
# #   dplyr::group_by(id_student) %>%
# #   dplyr::mutate(duplicates_merge = ifelse(n() > 1, TRUE, FALSE)) %>%
# #   dplyr::ungroup()  # Ungroup to avoid affecting further operations
# # 
# # table(merged_data$duplicates_merge)
# # 
# # table(merged_data$t0_st)
# # merged_data$gained_tax_learning = scale(merged_data$t_Score - merged_data$t0_Score )
# # merged_data$gained_tax_learning =  (merged_data$t_Score) -  (merged_data$t0_Score )
# # merged_data$`Gained Attitude and Motivation` = merged_data$`t_Attitude and Motivation` - merged_data$`t0_Attitude and Motivation`
# # merged_data$`Gained Learning Experience & User Experience` = merged_data$`t_Learning Experience & User Experience` - merged_data$`t0_Learning Experience & User Experience`
# # merged_data$`Gained Self-Regulation & Metacognition` = merged_data$`t_Self-Regulation & Metacognition` - merged_data$`t0_Self-Regulation & Metacognition`
# # merged_data$`Gained Engagement & Commitment` = merged_data$`t_Engagement & Commitment`- merged_data$`t0_Engagement & Commitment`
# # 
# # summary(merged_data$t_Score)

