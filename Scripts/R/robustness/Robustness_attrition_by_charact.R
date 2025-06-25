general_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Impact_of_AI_Supported_Learning/"

source(paste0(general_path,"Scripts/R/Data_Prep_ATE.R" ))

df_treated =  sqldf::sqldf("
             SELECT *, 
             CASE WHEN posttest_selected.id_student IS NULL THEN 0 ELSE 1 END TREATED
             FROM pretest_selected
             LEFT JOIN posttest_selected
             on posttest_selected.id_student =pretest_selected.id_student
             ")
attrition_model = (glm(data = df_treated, factor(TREATED)~factor(t0_LS_Mumford) +
                         `t0_Attitude and Motivation`+`t0_Learning Experience & User Experience`+
                         `t0_Engagement & Commitment`+`t0_Self-Confidence & Self-Efficacy`+
                         t0_gender +`t0_Type of School`
                        , family = "binomial"))
summary(attrition_model)
exp(coef(attrition_model))

exp(confint(attrition_model))

model_full <- glm(data = df_treated, factor(TREATED) ~ factor(t0_Q1), family = "binomial")
model_null <- glm(data = df_treated, factor(TREATED) ~ 1, family = "binomial")
anova(model_null, model_full, test = "LRT")
df_treated$t0_Q1  = factor(df_treated$t0_Q1)
table(factor(df_treated$t0_Q1))

#### IV REG ####
# Make sure t0_Q1 is a factor with appropriate reference
df_treated$t0_Q1 <- relevel(factor(df_treated$t0_Q1), ref = "1")
df_treated$gained_tax_learning = (df_treated$t_Score - df_treated$t0_Score )
# IV regression: instrument TREATED using t0_Q1
iv_model <- ivreg(t_Score ~ TREATED + factor(t0_Q1) + t0_Score| factor(t0_Q1) +t0_Score, data = df_treated)
summary(iv_model)

summary_iv_model_cov <- summary(iv_model, vcov = sandwich, diagnostics = TRUE)
#############################################
fs = lm(data = df_treated, factor(TREATED) ~factor(t0_Q1) )
summary(fs)
df_treated$iv = fs$fitted.values
ss = lm(data = df_treated[is.na(df_treated$gained_tax_learning)==F,], gained_tax_learning~iv*t0_Q1 )
summary(ss)

colnames(df_treated)
df_iv = df_treated
df_iv_cov <- df_iv[complete.cases(df_iv$t0_Score), ] # if t0_Score was not in cols_for_iv initially
# install.packages("ivreg")
library(ivreg)
library(sandwich)
table(df_iv$TREATED)
iv_model_cov <- ivreg(gained_tax_learning ~ (TREATED)   | factor(t0_Q1)+t0_Score  , data = df_iv[is.na(df_iv$gained_tax_learning)==F,])
summary(iv_model_cov)
# iv_model_cov <- ivreg(gained_tax_learning ~ factor(TREATED) + t0_Score | t0_Q1 + t0_Score, data = df_iv)
summary_iv_model_cov <- summary(iv_model_cov, vcov = sandwich, diagnostics = TRUE)
print(summary_iv_model_cov)
table(df_iv$TREATED)
iv_model <- ivreg(gained_tax_learning ~ TREATED | t0_Q1, data = df_iv)

