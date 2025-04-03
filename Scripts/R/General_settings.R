#######################
#https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
#https://lost-stats.github.io/Model_Estimation/Research_Design/two_by_two_difference_in_difference.html
#setwd("C:/Users/USER/Desktop/DID roads/")

 
if (Sys.info()["nodename"] ==  "Jaimes-MacBook-Pro.local" ){
  General_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/" 
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
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/function_cleaning.R")
source("~/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/AI-Assisted-Financial-Literacy/Scripts/R/function analysis.R")

# source("scripts/R/apis.R")
