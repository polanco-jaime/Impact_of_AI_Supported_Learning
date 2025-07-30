#######################
#https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
#https://lost-stats.github.io/Model_Estimation/Research_Design/two_by_two_difference_in_difference.html
#setwd("C:/Users/USER/Desktop/DID roads/")


if (Sys.info()["nodename"] ==  "Jaimes-MacBook-Pro.local" ){
  general_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Impact_of_AI_Supported_Learning/"
  General_path =general_path
  setwd(General_path)
  data_dir <- paste0(General_path , "Data/")
  graphs_dir <-  paste0(General_path , "Graph/") 
  tables_dir <- paste0(General_path , "Tables/")
  graph_metada_dir <-paste0(General_path , "Graph_Metadata/")
}  else if ( Sys.info()["nodename"] ==  "51768"  ){
  General_path ="~/Polanco/AI-Assisted-Financial-Literacy" 
  setwd(General_path)
  data_dir <- paste0(General_path , "Data/")
  graphs_dir <-  paste0(General_path , "Graph/") 
  tables_dir <- paste0(General_path , "Tables/")   
}



global_path = General_path
getwd()
lista <- c('readr', 'readxl', 'sqldf', 'plyr', 
           'did', 'arrow', 'ggplot2', 'dplyr', "digest",
           'fixest', 'gargle', 'stringr', 'broom', 
           'panelView', 'bacondecomp', 'paneltools', 
           'fect', 'PanelMatch', 'bigrquery')
for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}
getwd()

source(paste0(general_path,'Scripts/R/functions/function_cleaning.R' ))

source(paste0(general_path,'Scripts/R/functions/function analysis.R' ))
# source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/function analysis.R")

# source("scripts/R/apis.R")

# # #### 1. Pretest - Reading csv #### 
# # 
# path_data= '~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Data/'
# pretest <- read_csv(paste0(path_data,"results-survey457438.csv") )
# # 
# pretest_st <- read_csv(paste0(path_data,"results-survey959811.csv") )
# # # 
# # # #### 2. Postest - Reading csv #### 
# # # 
# posttest <-  read_csv(paste0(path_data,"results-survey324716.csv") )
# # 
# # # #### 3. Postposttest - Reading csv #### 
# postposttest <- read_csv(paste0(path_data,"results-survey767828.csv") )  
# # 
# # 
data <- arrow::read_parquet("Data/data.parquet")
colnames(data)[16] ='Q1'

# 
# A= data[data$pre_flag_duplicate_id_score==1,]
# data <- data[data$pre_flag_duplicate_id_score==0,]
# data <- data[data$pre_flag_duplicate_id==0 & data$pre_flag_duplicate_id_score==0,]
# 
# summary(data['pre_interviewtime'])
# data =  data[data['pre_interviewtime']/60 >=9 , ]
 
# table(data$post_interviewtime>=200)
# data <- data[data$post_interviewtime>=100 | is.na(data$post_interviewtime)==T ,]
data <- data[data$pre_interviewtime>=220   ,]
# data <- data[data$post_interviewtime>=60 | is.na(data$post_interviewtime)==T ,]
# data = data[(data$pre_flag_duplicate_id)==0, ]

# data = data[is.na(data$post_flag_duplicate_id)==T |(data$post_flag_duplicate_id)==0, ]
data = data[is.na(data$post_flag_duplicate_id_score)==T | (data$post_flag_duplicate_id_score)==0, ]
# # write_csv(data, "data_trash.csv")
colnames(data)[16] ='Q1'
# 
 
# data =  data[data['pre_interviewtime']/60 >=5 , ]
fractions_by_group_print(data, 'Q1')
