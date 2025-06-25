general_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Impact_of_AI_Supported_Learning/"

source(paste0(general_path,"Scripts/R/Data_Prep_ATE.R" ))

df_treated =  sqldf::sqldf("
             SELECT *, 
             CASE WHEN posttest_selected.id_student IS NULL THEN 0 ELSE 1 END TREATED
             FROM pretest_selected
             LEFT JOIN posttest_selected
             on posttest_selected.id_student =pretest_selected.id_student
             ")
attrition_model = (glm(data = df_treated, factor(TREATED)~factor(t0_Q1) , family = "binomial"))

exp(coef(attrition_model))

exp(confint(attrition_model))

model_full <- glm(data = df_treated, factor(TREATED) ~ factor(t0_Q1), family = "binomial")
model_null <- glm(data = df_treated, factor(TREATED) ~ 1, family = "binomial")
anova(model_null, model_full, test = "LRT")
