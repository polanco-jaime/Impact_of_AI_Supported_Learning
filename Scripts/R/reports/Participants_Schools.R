library(haven)
library(ggplot2)
base_ = function(potential_participants,attrition,potential_students_participating){
  return(paste0())
}
base_ = "Considering an attrition of % the number of potential intervented students are: "
library(readr)
schools_participants <- read_csv("~/Downloads/results-survey655762 (1).csv")
# schools_participants <- read_sav("~/Downloads/survey_655762_spss.sav")

schools_participants = schools_participants[schools_participants$lastpage==1 & schools_participants$G01Q11!='TEST', ]
schools_participants$G01Q11= gsub(pattern = '-tal', replacement = '', schools_participants$G01Q11)
schools_participants$G01Q11 = gsub(pattern = '19 + 15 + 14 + 13 = ', replacement = '', schools_participants$G01Q11)
schools_participants$G01Q11 = gsub(pattern = ' \\(en 3 leerkrachten\\)', replacement = '', schools_participants$G01Q11)
schools_participants$G01Q11 =gsub(pattern = '\\d+ \\+ \\d+ \\+ \\d+ \\+ \\d+ = ', replacement = '', schools_participants$G01Q11)
schools_participants$G01Q11 =gsub(pattern = "9 en 22" , replacement = '31', schools_participants$G01Q11)
schools_participants$G01Q11 =gsub(pattern = "tussen min.13 en max. 24 leerlingen"  , replacement = '13', schools_participants$G01Q11)
schools_participants[67,20] ='40'
schools_participants[69,20] ='90'
schools_participants[53,20] ='15'
schools_participants[29,20] ='20'
schools_participants[22,20] ='21'
schools_participants[21,20] ='12'
schools_participants[83,20] ='15'
schools_participants[85,20] ='52'
schools_participants[88,20] ='140'
schools_participants[91,20] ='29'

potential_participants = sum(as.numeric(schools_participants$G01Q11) ,na.rm = T )
potential_participants
(schools_participants$G01Q11)
library(openxlsx)
schools_participants$email_teacher = ifelse(schools_participants$G01Q05=='AO01', schools_participants$G01Q07 , schools_participants$G01Q04)
schools_participants$email_teacher 
schools_participants$email_teacher = ifelse( grepl("@", schools_participants$email_teacher)==T,  schools_participants$email_teacher,  schools_participants$G01Q04)
schools_participants[7,38] ='Celine.lebbe@kagr.be'
schools_participants[52,38] = 'pascal.debruyne@vlot.be'
schools_participants[107,38]  = 'Drci@broeders.be'


potential_students_participating = 0.8
attrition = 2/3
expected =potential_participants*attrition*potential_students_participating
print(paste0(base_, expected ))

#### G01Q10 - Number of clases per school
round(mean(as.numeric(schools_participants$G01Q10) ,na.rm = T ), 0)

#### G01Q12 - Period in which you want to use the teaching material (between January and April)
library(dplyr)
schools_participants$main_month = homogenize_months(schools_participants$G01Q12)$main_month


write.xlsx(schools_participants, file = "schools_participants.xlsx")

##### Number of schools per month
descriptive_stats <- schools_participants %>%
  count(main_month) %>%
  arrange(desc(n))
paste0('School: ',
schools_participants[schools_participants$main_month=='January', ]$G01Q01,
', Addres: ',
schools_participants[schools_participants$main_month=='January', ]$G01Q02 
)
# Definir el orden cronológico de los meses
descriptive_stats$main_month <- factor(
  descriptive_stats$main_month,
  levels = c("January", "February", "March", "April")
)

# Crear el gráfico de barras
ggplot(descriptive_stats, aes(x = main_month, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of schools per month",
       x = "Month",
       y = "Schools") +
  theme_minimal()

##### Number of students per month
descriptive_stats <- schools_participants  %>%
  mutate(G01Q11 = as.numeric(G01Q11)) %>%
  group_by(main_month) %>%
  summarise(total_students = sum(G01Q11, na.rm = TRUE)) %>%
  arrange(desc(total_students) )


descriptive_stats$main_month <- factor(
  descriptive_stats$main_month,
  levels = c("January", "February", "March", "April")
)

ggplot(descriptive_stats, aes(x = main_month, y = total_students)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of students per month",
       x = "Month",
       y = "Total students") +
  theme_minimal()

