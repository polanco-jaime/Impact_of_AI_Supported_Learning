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

# #### 1. Pretest - Reading csv #### 
# 
# pretest <- read_csv("~/Downloads/results-survey457438 (39).csv")
# pretest_st <- read_csv("~/Downloads/results-survey959811 (16).csv")
# 
# #### 2. Postest - Reading csv #### 
# 
# posttest <- read_csv("~/Downloads/results-survey324716 (39).csv")
# #### 3. Postposttest - Reading csv #### 
# postposttest <- read_csv("~/Downloads/results-survey767828 (2).csv")

data <- arrow::read_parquet("Data/data.parquet")

data <- data[data$pre_flag_duplicate_id_score==0,]
# write_csv(data, "data_trash.csv")

