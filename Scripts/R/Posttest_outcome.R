##### Knowledge Score ####
posttest$Score = scale(posttest$Score )

print(descriptive_stats_by_group(posttest, "Score", "Q1", latex = T))

plot_rct_density(posttest, score_col = "Score", group_col = "Q1",  )
summary(lm(data=posttest , Score~factor(Treatment_state_i)) )
summary(lm(data=posttest , Score~factor(Q1)) )
summary(lm(data=posttest[posttest$Q1!='1',], Score~factor(Q1)) )
summary(lm(data=posttest[posttest$Q1!='2',], Score~factor(Treatment_state_i)) )
summary(lm(data=posttest[posttest$Q1!='3',], Score~factor(Treatment_state_i)) )
 
fractions_by_group(merged_data, 't0_Q1')
fractions_by_group(posttest, 'Q1')
number_by_group(pretest, 'Q1')
number_by_group(merged_data, 't0_Q1')
##### Measure Emotional Attitudes ####

##### 6.3 Measure Emotional Attitudes ####
###### 8.1 Attitude and Motivation ####
posttest[["Attitude and Motivation"]]
plot_rct_density(posttest, score_col = "Attitude and Motivation", group_col = "Q1", "")
print(descriptive_stats_by_group(posttest, "Attitude and Motivation", "Q1", latex = T))

summary(lm(data=posttest, `Attitude and Motivation`~factor(Treatment_state_i)) )

###### 8.2 Learning Experience & User Experience ####
hist(posttest$`Learning Experience & User Experience`)
plot_rct_density(posttest, score_col = "Learning Experience & User Experience", group_col = "Q1")
print(descriptive_stats_by_group(posttest, "Learning Experience & User Experience",
                                 "Q1", latex = T))

summary(lm(data=posttest, `Learning Experience & User Experience`~factor(Treatment_state_i)) )


###### 8.3 Self-Regulation & Metacognition ####
hist(posttest$`Self-Regulation & Metacognition`)
plot_rct_density(posttest, score_col = "Self-Regulation & Metacognition", group_col = "Q1")
print(descriptive_stats_by_group(posttest, "Self-Regulation & Metacognition",
                                 "Q1", latex = T))
summary(lm(data=posttest, `Self-Regulation & Metacognition`~factor(Treatment_state_i)) )


###### 8.4 Engagement & Commitment ####

hist(posttest$`Engagement & Commitment`)
plot_rct_density(posttest, score_col = "Engagement & Commitment", group_col = "Q1")
print(descriptive_stats_by_group(posttest,"Engagement & Commitment",
                                 "Q1", latex = T))
summary(lm(data=posttest, `Engagement & Commitment`~factor(Treatment_state_i)) )

###### 8.5 Self-Confidence & Self-Efficacy ####
hist(posttest$`Self-Confidence & Self-Efficacy`)
plot_rct_density(posttest, score_col = "Self-Confidence & Self-Efficacy", group_col = "Q1")
print(descriptive_stats_by_group(posttest,"Self-Confidence & Self-Efficacy",
                                 "Q1", latex = T))
summary(lm(data=posttest, `Self-Confidence & Self-Efficacy`~factor(Treatment_state_i)) )
summary(lm(data=posttest, `Self-Confidence & Self-Efficacy`~factor(Q1)) )


###### 6.3.6 Emotional & Psychological Factors ####
hist(posttest$`Emotional & Psychological Factors`)

plot_rct_density(posttest, score_col = "Emotional & Psychological Factors", group_col = "Q1")
print(descriptive_stats_by_group(posttest, "Emotional & Psychological Factors",
                                 "Q1", latex = T))
summary(lm(data=posttest, `Emotional & Psychological Factors`~factor(Treatment_state_i)) )

