library(haven)
library(readr)
# install.packages("jsonlite")
library(jsonlite)
getwd()
general_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Impact_of_AI_Supported_Learning/"
# general_path = "~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/"

# source(paste0(general_path,"Scripts/R/function_cleaning.R" ))
library(dplyr)


library(ggplot2)
#### 1. Reading sav #### 
# source(paste0(general_path,'Scripts/R/General_settings.R' )) 


pretest$st =F
pretest_st$st = T
colnames(pretest_st)= colnames(pretest)
pretest = rbind(pretest, pretest_st)
##### 1.1 Flag duplicates ####
pretest <- flag_duplicates(pretest)
##### 1.2 Resolve duplicates ####
table(is.na(pretest$Q1))
pretest <- subset(pretest, is.na(pretest$Q1)==F)
table(is.na(pretest$Q1))
pretest$Q1 = as.character(pretest$Q1)
# pretest <- pretest[pretest$id!=1142, ]  #duplicated with 1653
# pretest <- pretest[pretest$id!=2290, ] #duplicated with 1706
# pretest <- pretest[pretest$id!=2351, ] #duplicated with 756
# pretest <- pretest[pretest$id!=2314, ] #duplicated with 2313
# pretest <- pretest[pretest$id!=2293, ] #duplicated with 1698
# pretest <- pretest[pretest$id!=2774, ] #duplicated with 1225
# pretest <- pretest[pretest$id!=737, ] #duplicated with 695
# pretest <- pretest[pretest$id!=2393, ] #duplicated with 2467
# pretest <- pretest[pretest$id!=2387, ] #duplicated with 2466
#pretest <- resolve_duplicates(pretest)
##### 1.3 Droping no completed at all #### 
table(is.na(pretest$`AS06[SQ001]`))
# pretest_no =pretest[is.na(pretest$`AS06[SQ001]`)==T,]
# pretest =pretest[is.na(pretest$`AS06[SQ001]`)==F,]

pretest =pretest[ (pretest$G01Q04)!="Test",]
pretest <- pretest[!grepl("test", tolower(pretest$G01Q01), ignore.case = TRUE), ]
pretest <- pretest[is.na(pretest$G01Q01)==F, ]
#### 2. Checking randomization #### 
 
number_by_group(pretest, 'Q1')

fractions_by_group(pretest, 'Q1')


##### 2.1 Time to respond ####
summary(pretest$groupTime12808)

pretest$interaction_time_minutes =  as.numeric(pretest$datestamp-pretest$startdate)
summary(pretest$interaction_time_minutes)
# pretest = pretest[pretest$interaction_time_minutes>=5, ]

#### 3. Personal information cleaning ####
##### 3.1 full name  ####
pretest$G01Q01 = homogenize_name(pretest$G01Q01)

pretest$G01Q01 = ifelse(pretest$G01Q01=='GAÉTAN_DEVOS', 'GAETAN_DEVOS', pretest$G01Q01)
###### 3.1.1 Teachers in the test ####
c("KRISTIEN_LASEURE", 'TEST_LEERKRACHT', "TEST_TEST", "CARLA_DE_LATHAUWER",
  "CINDY_KEERSMAEKERS")

##### 3.2 Gender  ####

pretest$G01Q03 = translate_responses(pretest$G01Q03, translations)
table(pretest$G01Q03)
##### 3.3 Municipality  ####

pretest$G01Q04 = homogenize_cities(pretest$G01Q04)
unique(pretest$G01Q04)
##### 3.4 School Name  ####
sort(unique(pretest$G01Q05))
# 1. Uppercase and Initial Cleaning

pretest$G01Q05 <- homogenize_school_name(pretest$G01Q05)
unique(pretest$G01Q05)
 
pretest$G01Q05 = ifelse(pretest$G01Q01=="MATTIZ_VERCRUYCE",
                        'Sint-Godelievecollege',pretest$G01Q05   ) 
pretest$G01Q05 = ifelse(pretest$G01Q05=="Hhc",
                        'Heilig-Hartcollege',
                        pretest$G01Q05)
pretest$G01Q05 = ifelse(pretest$G01Q05=="Sbc",
                        'Sint-Barbaracollege',
                        pretest$G01Q05)

sort(unique(paste0("School: ", pretest$G01Q05, "; City: ", pretest$G01Q04)) )
##### 3.5 Course of Student  ####
pretest$G01Q06 <- homogenize_course_group(pretest$G01Q06)
##### 3.6 Type of School  ####
pretest$G01Q07 = translate_responses(pretest$G01Q07, translations)

 

#### 4. Academic performance and Home Enviroment #### 
##### 4.1 Last Dutch Grade (Previous School Year) ####
 
pretest$G02Q01 = translate_responses(pretest$G02Q01, translations)
##### 4.2 Last Math Grade (Previous School Year) ####
table(pretest$G02Q012)
pretest$G02Q012 = translate_responses(pretest$G02Q012, translations)
##### 4.3 Predominant Language Used at Home  ####
table(pretest$G02Q02)
pretest$G02Q02 = translate_responses(pretest$G02Q02, translations)


pretest$G02Q031 = translate_responses(pretest$G02Q031, translations)
table(pretest$G02Q031)

pretest$G02Q01
pretest$G02Q01


#### 4. Cleanning Learning Style #### 
translations_ls = c(
  "Mee oneens" = 1,
  "Neutraal" = 2,
  "Mee eens" = 3,
  "Helemaal mee oneens" = 4,
  'Helemaal mee eens' = 5)
pretest$`G05Q01[SQ001]`= translate_responses(pretest$`G05Q01[SQ001]`, translations_ls)
pretest$`G05Q01[SQ002]`= translate_responses(pretest$`G05Q01[SQ002]`, translations_ls)
pretest$`G05Q01[SQ003]`= translate_responses(pretest$`G05Q01[SQ003]`, translations_ls)
pretest$`G05Q01[SQ004]`= translate_responses(pretest$`G05Q01[SQ004]`, translations_ls)
pretest$`G05Q01[SQ005]`= translate_responses(pretest$`G05Q01[SQ005]`, translations_ls)
pretest$`G05Q01[SQ006]`= translate_responses(pretest$`G05Q01[SQ006]`, translations_ls)
pretest$`G05Q01[SQ007]`= translate_responses(pretest$`G05Q01[SQ007]`, translations_ls)
pretest$`G05Q01[SQ008]`= translate_responses(pretest$`G05Q01[SQ008]`, translations_ls)
'
Determine the relevance of each question (SQ001–SQ008) 
to financial literacy and AI interaction.
Assign higher weights to questions that align more closely with these themes. 
'
pretest$LS_Activist = (pretest$`G05Q01[SQ001]`*1.5+pretest$`G05Q01[SQ002]`*1.2)/2.7
pretest$LS_Reflector = (pretest$`G05Q01[SQ003]`+pretest$`G05Q01[SQ004]`*1.2)/2.2
pretest$LS_Theorist = (pretest$`G05Q01[SQ005]`*1.5+pretest$`G05Q01[SQ006]`*1.5)/3
pretest$LS_Pragmatist = (pretest$`G05Q01[SQ006]`*1.5+pretest$`G05Q01[SQ008]`*1.2)/2.7

pretest$LS_Mumford <- sapply(1:nrow(pretest), function(i) {
  scores <- pretest[i, c("LS_Activist", "LS_Reflector", "LS_Theorist", "LS_Pragmatist")]
  if (all(is.na(scores))) {
    return(NA)  # Return NA for rows with all NA scores
  }
  styles <- c("Activist", "Reflector", "Theorist", "Pragmatist")
  styles[which.max(scores)]
})
table(pretest$LS_Mumford)


'
(1. Activist) SQ001,SQ002
(2. Reflector) SQ003,SQ004
(3. Theorist) SQ005, SQ006
(4. Pragmatist)  SQ007, SQ008

Scoring and Interpretation:
  
  Use the 5-point Likert scale 
  (Strongly Agree, Agree, Neutral, Disagree, Strongly Disagree) where 
  Strongly Agree equals 5 and Strongly Disagree equals 1. 
Calculate the average score for each learning style.
# Helemaal mee eens 5
# Helemaal mee oneens 1
'
pretest <- pretest[, !colnames(pretest) %in% c("LS_Activist", "LS_Reflector", "LS_Theorist", "LS_Pragmatist")]
table(pretest$LS_Mumford)
table(pretest[pretest$Q1=='1',]$LS_Mumford)
table(pretest[pretest$Q1=='2',]$LS_Mumford)
table(pretest[pretest$Q1=='3',]$LS_Mumford)
#### 5. Cleanning self-perception #### 
pretest[['PT01[SQ001]']] = translate_responses(pretest[['PT01[SQ001]']] , translations)
pretest[['PT01[SQ002]']] = translate_responses(pretest[['PT01[SQ002]']] , translations)
pretest[['PT01[SQ003]']] = translate_responses(pretest[['PT01[SQ003]']] , translations)
pretest[['PT01[SQ004]']] = translate_responses(pretest[['PT01[SQ004]']] , translations)
pretest[['PT01[SQ005]']] = translate_responses(pretest[['PT01[SQ005]']] , translations)                                 
  

#### 6. Cleanning pretest #### 

##### 6.1 Assesses understanding of percentages in the context of a tax rate. ####
pretest$PT03 = ifelse(pretest$PT03=='20%'| pretest$PT03== 'AO02', 1, 0)
table(pretest$PT03 )
pretest$disventaged= ifelse(pretest$PT03==0,1,0 )
 
##### 6.2 Measure Tax System Knowledge. ####

pretest$PTF01 =ifelse(pretest$PTF01=='173'| pretest$PTF01== 'AO01', 1, 0)
table(pretest$PTF01)
pretest$PTF01 = as.numeric(pretest$PTF01)

pretest$PTF02 = translate_responses(pretest$PTF02, translations)
table(pretest$PTF02)
pretest$PTF02 =ifelse(pretest$PTF02=="Progressive tax system"| pretest$PTF02== 'AO03', 1, 0)
pretest$PTF02 = as.numeric(pretest$PTF02)

table(pretest$PTF06)
20000*0.25+20000*0.40+17000*0.53
pretest$PTF06 =ifelse(pretest$PTF06=="€22010"| pretest$PTF06== 'AO03', 1, 0)

23000*0.25+19000*0.4

pretest$PTF06 = as.numeric(pretest$PTF06)


table(pretest$PTF07)
pretest$PTF07=ifelse(pretest$PTF07=="30100"| pretest$PTF07== 'AO01', 1, 0)
pretest$PTF07 = as.numeric(pretest$PTF07)

table(pretest$PTF09)
pretest$PTF09 = translate_responses(pretest$PTF09, translations)
pretest$PTF09=ifelse(pretest$PTF09=="The national average wage"| pretest$PTF09== 'AO03', 1, 0)
pretest$PTF09 = as.numeric(pretest$PTF09)

table(pretest$PTF10)
pretest$PTF10=ifelse(pretest$PTF10=="62%"| pretest$PTF10== 'AO01', 1, 0)
pretest$PTF10 = as.numeric(pretest$PTF10)

table(pretest$PTF11)
ans_ptf11 = 'Bij een degressief systeem zou het totale gemiddelde belastingtarief op het inkomen dalen na de extra opdracht.'
pretest$PTF11=ifelse(pretest$PTF11==ans_ptf11| pretest$PTF11== 'AO03', 1, 0)
pretest$PTF11 = as.numeric(pretest$PTF11)


pretest$PTF01+pretest$PTF02+pretest$PTF06+pretest$PTF07+pretest$PTF09+pretest$PTF10+pretest$PTF11
# pretest$Score = (pretest$PTF01+ pretest$PTF06+pretest$PTF07+pretest$PTF09+pretest$PTF10+pretest$PTF11)/5


pretest$Score = (pretest$PTF01+pretest$PTF02+pretest$PTF06+pretest$PTF07+pretest$PTF09+pretest$PTF10+pretest$PTF11)/7

pretest$Score = ifelse(is.na(pretest$Score)==T, 0, pretest$Score)
summary(pretest$Score)
  
##### 6.3 Measure Emotional Attitudes ####
###### 6.3.1 Attitude and Motivation ####
"
Where 5 it is a higher level of attitude and motivation.
A score closer to 1 indicates lower attitude/motivation.
A score around 3 would be neutral.
(Page 4, Scoring the MSLQ states: Scales are constructed by taking the mean of the items that make up that scale.'

Motivation is a multi-faceted construct. The MSLQ itself, which you referenced, breaks motivation down into components like Intrinsic Goal Orientation, Task Value, Self-Efficacy, etc.
Your separation aligns with this idea of distinct components. I could argue that
'Attitude and Motivation' (items 1-4) reflects a general academic or subject-specific task value and intrinsic interest,
while 'AI Attitude and Motivation' (items 5-7)
reflects task value and interest specifically related to using
AI/computer tools for learning.
"
translations_ls
pretest$`AS01[SQ001]` = as.numeric(translate_responses(pretest$`AS01[SQ001]`, translations_ls) )
pretest$`AS01[SQ002]` = as.numeric(translate_responses(pretest$`AS01[SQ002]`, translations_ls) )
pretest$`AS01[SQ003]` = as.numeric(translate_responses(pretest$`AS01[SQ003]`, translations_ls) )
pretest$`AS01[SQ004]` = as.numeric(translate_responses(pretest$`AS01[SQ004]`, translations_ls) )
pretest$`AS01[SQ005]` = as.numeric(translate_responses(pretest$`AS01[SQ005]`, translations_ls) )
pretest$`AS01[SQ006]` = as.numeric(translate_responses(pretest$`AS01[SQ006]`, translations_ls) )
pretest$`AS01[SQ007]` = as.numeric(translate_responses(pretest$`AS01[SQ007]`, translations_ls) )

pretest$`Attitude and Motivation` =  (pretest$`AS01[SQ001]` + pretest$`AS01[SQ002]`+   
                                        pretest$`AS01[SQ003]` + pretest$`AS01[SQ004]`
                                      )/4
hist(pretest$`Attitude and Motivation`)

####### 6.3.1.1 AI Attitude and Motivation ####
pretest$`AI Attitude and Motivation` =  (    pretest$`AS01[SQ005]` +
                pretest$`AS01[SQ006]`+   pretest$`AS01[SQ007]`  )/3

hist(pretest$`AI Attitude and Motivation`)
  
###### 6.3.2 Learning Experience & User Experience ####
"
Gebaseerd op de Technology Acceptance Model (TAM)
(Davis, F. D. (1989). Perceived usefulness, perceived ease of use, and user acceptance of information technology. MIS Quarterly, 13(3), 319–340.)

liekr scale from  Strongly Negative Experience to Strongly Positive Experience
"
pretest$`AS02[SQ001]` = as.numeric(translate_responses(pretest$`AS02[SQ001]`, translations_ls) )
pretest$`AS02[SQ002]` = as.numeric(translate_responses(pretest$`AS02[SQ002]`, translations_ls) )
pretest$`AS02[SQ003]` = as.numeric(translate_responses(pretest$`AS02[SQ003]`, translations_ls) )
pretest$`Learning Experience & User Experience` =  (pretest$`AS02[SQ001]` + pretest$`AS02[SQ002]`+   pretest$`AS02[SQ003]`   )/3

hist(pretest$`Learning Experience & User Experience`)
summary(pretest$`Learning Experience & User Experience`)
"
Students are not strongly enthusiastic about their learning/user experience
with new digital tools
Median = 2.667
"
###### 6.3.3 Self-Regulation & Metacognition ####

pretest$`AS03[SQ001]` = as.numeric(translate_responses(pretest$`AS03[SQ001]`, translations_ls) )
pretest$`AS03[SQ002]` = as.numeric(translate_responses(pretest$`AS03[SQ002]`, translations_ls) )
pretest$`AS03[SQ003]` = as.numeric(translate_responses(pretest$`AS03[SQ003]`, translations_ls) )
pretest$`Self-Regulation & Metacognition` =  (pretest$`AS03[SQ001]` + pretest$`AS03[SQ002]`+   pretest$`AS03[SQ003]`   )/3

hist(pretest$`Self-Regulation & Metacognition`)

###### 6.3.4 Engagement & Commitment ####
colnames(pretest)
pretest$`AS04[SQ001]` = as.numeric(translate_responses(pretest$`AS04[SQ001]`, translations_ls) )
pretest$`AS04[SQ002]` = as.numeric(translate_responses(pretest$`AS04[SQ002]`, translations_ls) )
pretest$`AS04[SQ003]` = as.numeric(translate_responses(pretest$`AS04[SQ003]`, translations_ls) )
pretest$`Engagement & Commitment` =  (pretest$`AS04[SQ001]` + pretest$`AS04[SQ002]`+   pretest$`AS04[SQ003]`   )/3

hist(pretest$`Engagement & Commitment`)

###### 6.3.5 Self-Confidence & Self-Efficacy ####
colnames(pretest)
pretest$`AS05[SQ001]` = as.numeric(translate_responses(pretest$`AS05[SQ001]`, translations_ls) )
pretest$`AS05[SQ002]` = as.numeric(translate_responses(pretest$`AS05[SQ002]`, translations_ls) )

pretest$`Self-Confidence & Self-Efficacy` =  (pretest$`AS05[SQ001]` + pretest$`AS05[SQ002]`   )/2

###### 6.3.6 Emotional & Psychological Factors ####
# reverse-score AS06[SQ001] for a Positive Challenge Orientation
pretest$`AS06[SQ001]` = as.numeric(translate_responses(pretest$`AS06[SQ001]`, translations_ls) )
pretest$`AS06[SQ001]` = 6 - pretest$`AS06[SQ001]`

pretest$`AS06[SQ002]` = as.numeric(translate_responses(pretest$`AS06[SQ002]`, translations_ls) )

pretest$`Emotional & Psychological Factors` =  (pretest$`AS06[SQ001]` + pretest$`AS06[SQ002]`   )/2
# Reverse score AS06[SQ001] so higher means LESS nervousness


library(digest)


pretest$Q1 = as.character(pretest$Q1)
#### Create a new ID in the pretest ####
dpre1 = subset(pretest, pretest$G01Q01== 'TUUR_VERDONCK_EN_PAULINE_GOOSSENS')
dpre2 = subset(pretest, pretest$G01Q01== 'TUUR_VERDONCK_EN_PAULINE_GOOSSENS')

dpre3 = subset(pretest, pretest$G01Q01== 'JACK_DE_SCHUTTER_EN_MATTHIAS_LUTGEN')
dpre3$G01Q01<-'DE_SCHUTTER_JACK'
dpre3$Q1<-'1'
dpre3$id=2290

pretest = pretest[pretest$G01Q01!= 'TUUR_VERDONCK_EN_PAULINE_GOOSSENS',]
dpre1$G01Q01<-'TUUR_VERDONCK'
dpre1$G01Q03<-'Boy'

dpre1$id=1142
dpre2$G01Q01<-'PAULINE_GOOSSENS'

pretest = rbind(pretest, dpre1)
pretest = rbind(pretest, dpre2)
pretest = rbind(pretest, dpre3)

pretest$G01Q01 = ifelse(pretest$G01Q01=='JACK_DE_SCHUTTER_EN_MATTHIAS_LUTGEN', 'MATTHIAS_LUTGEN', pretest$G01Q01)

# pretest$Q1 = ifelse(pretest$G01Q01=="JARNE_VAN_DOREN",'1',posttest$Q1   )
#### Fixing things of the pretest ####
pretest$Q1 = ifelse(pretest$G01Q01=="KOBE_ROELS",'2',pretest$Q1)
pretest$Q1 = ifelse(pretest$G01Q01=="JEF_VANHAELST",'2',pretest$Q1)
pretest$Q1 = ifelse(pretest$G01Q01=="ORENS_HAYDEN",'2',pretest$Q1)
pretest$Q1 = ifelse(pretest$G01Q01=="IMRAN_SOUSSI",'3',pretest$Q1)



pretest$G01Q03 = ifelse(pretest$G01Q01=="ANAS_ELASSOOUDI",'Boy',pretest$G01Q03)






###### Solving joining data ####
pretest$Q1 <- trimws(pretest$Q1)
table(pretest$Q1)
pretest$G01Q03 <- trimws(pretest$G01Q03)
pretest$G01Q05 <- trimws(pretest$G01Q05)
pretest$G01Q01 <- trimws(pretest$G01Q01)
pretest$Q1 <- as.character(pretest$Q1)
pretest$G01Q03 <- as.character(pretest$G01Q03)
pretest$G01Q05 <- as.character(pretest$G01Q05)
pretest$G01Q01 <- as.character(pretest$G01Q01)
##### More clenaing ####
pretest$G01Q01 <- ifelse(pretest$id==277, 
                              'OSAKPAMWAN_AGHO', 
                         pretest$G01Q01)

##### end ####
# pretest$G01Q01 <- iconv(pretest$G01Q01, to = "ASCII//TRANSLIT")

pretest$id_student <- paste0(as.character(pretest$Q1), "_", pretest$G01Q03, "_",
                             pretest$G01Q05,"_", pretest$G01Q01)
pretest$id
# pretest = anonymize_column(pretest, "id_student")

flag_last_duplicate <- function(df, grouping_col_name = "id_student") {
 value_col_name <- "id"
 
  df_flagged <- df %>%
    group_by(.data[[grouping_col_name]]) %>%
    mutate(
      # Temporary helper columns, using a dot prefix to reduce chance of name collision
      .n_occurrences = n(), 
      .is_max_id_value_ = (.data[[value_col_name]] == max(.data[[value_col_name]], na.rm = TRUE)),
      .is_max_score_value = (.data[['Score']] == max(.data[['Score']], na.rm = TRUE)),
      # The flag column, named 'flag_duplicate' as in your original function body
      flag_duplicate_id = if_else(.n_occurrences > 1 & .is_max_id_value_, 1, 0),
      flag_duplicate_id_score = if_else(.n_occurrences > 1 & .is_max_score_value, 1, 0)
    ) %>%
    ungroup() %>% # Good practice to ungroup after mutations
    select(-.n_occurrences, -.is_max_id_value_, -.is_max_score_value) # Remove helper columns
  
  return(df_flagged)
}

# Apply the function
pretest = flag_last_duplicate(pretest, "id_student")
table(pretest$flag_duplicate_id)
table(pretest$flag_duplicate_id_score)

# pretest <- subset(pretest,  pretest$flag_duplicate==0)
pretest$id_student2 <- paste0(
                                as.character(pretest$Q1), "_",
                                tolower( substr( 
                                  gsub( " ", "", gsub("[[:punct:]\\s]", "", pretest$G01Q05) ),   1, 4 ) ), "_",
                                pretest$G01Q01
                              )
pretest = flag_last_duplicate(pretest, "id_student2")
table(pretest$flag_duplicate_id)
# pretest <- subset(pretest,  pretest$flag_duplicate==0)
# pretest <- subset(pretest,  is.na(pretest$Score)==F)
summary(pretest$Score)
sd(pretest$Score, na.rm = T)

summary(pretest$groupTime12808)
time = pretest[is.na(pretest$groupTime12808)==F , ]
p1=quantile(time$groupTime12808, probs = 0.05, )
p99=quantile(time$groupTime12808, probs = 0.99)

pretest$anormal_asnwering_flag= ifelse(
  pretest$groupTime12808>=p99 |
    pretest$groupTime12808<=p1 , 1, 0
)
table(pretest$anormal_asnwering_flag)
# pretest = pretest[pretest$anormal_asnwering_flag==0,]
# pretest = pretest[pretest$flag_duplicate_id==0,]
# 
# pretest = pretest[pretest$flag_duplicate_id_score==0,]
# pretest$id = seq(1:nrow(pretest))
