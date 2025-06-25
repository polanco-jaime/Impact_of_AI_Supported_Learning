library(sqldf)
#### SinClara ####
posttest$FECHA = as.character(posttest$startdate)
sqldf("
      SELECT  --FECHA STARTDATE ,   
      replace(UPPER(G01Q01), '_', ' ') STUDENTEN, 
      G01Q05 School,
      SCORE
      FROM posttest
      WHERE lower(G01Q05) LIKE '%clara%' OR lower(G01Q05) LIKE '%sintclara%' 
      ORDER BY startdate
      ")

#### Go4City ####
posttest$FECHA = as.character(posttest$startdate)
sqldf("
      SELECT  FECHA STARTDATE ,   UPPER(G01Q01) STUDENTEN, SCORE FROM posttest
      WHERE G01Q05 LIKE '%4%' OR UPPER(G01Q05) LIKE '%GO4CITY%' OR  UPPER(G01Q05) LIKE '%CITY%'
      ORDER BY startdate
      ")


#### TA Brasschaat ####
pretest$FECHA = as.character(pretest$startdate)
sqldf("
      SELECT  FECHA STARTDATE ,   UPPER(G01Q01) STUDENTEN, SCORE FROM pretest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%brasschaat%'  
      ORDER BY  G01Q01
      ")
sqldf("
      SELECT   distinct   replace(UPPER(G01Q01), '_', ' ') STUDENTEN , Q1 LeerpadGroep
      FROM pretest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%brasschaat%'  
      ORDER BY  G01Q01
      ")
posttest$FECHA = as.character(posttest$startdate)
# sqldf("
#       SELECT  FECHA STARTDATE ,   UPPER(G01Q01) STUDENTEN, SCORE FROM posttest
#       WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%tabrasschaat%'  
#       ORDER BY startdate
#       ")
sqldf("
      SELECT  FECHA STARTDATE ,   replace(UPPER(G01Q01), '_', ' ') STUDENTEN,
      --G01Q04 CITY,
      SCORE FROM posttest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%brasschaat%'  
 
      ORDER BY STUDENTEN
      ")
#### Sint-Paulus  ####
posttest$FECHA = as.character(posttest$startdate)
library(sqldf)
school = sqldf("
      SELECT  FECHA STARTDATE ,   replace(UPPER(G01Q01), '_', ' ') STUDENTEN,
      --G01Q04 CITY,
      SCORE FROM posttest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%paulus%'  
      and lower(G01Q04) like '%herzele%'
      ORDER BY startdate
      ")
write_csv(school, 'school.csv')

#### Sint-Barbaracollege ####
posttest$FECHA = as.character(posttest$startdate)
A = posttest[ , c('G01Q01',   'G01Q04', 'G01Q05', "Score", "FECHA")]
school = sqldf("
      SELECT  FECHA STARTDATE ,   replace(UPPER(G01Q01), '_', ' ') STUDENTEN,
      --G01Q04 CITY,
      SCORE FROM posttest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%barbara%'  
 
      ORDER BY STUDENTEN
      ")
school
First_shot = school[ as.Date(school$STARTDATE)<= "2025-02-14", ]
Second = school[ as.Date(school$STARTDATE)> "2025-02-14", ]
mean(First_shot$Score)

mean(posttest$Score, na.rm = T)
infirst = c('ADRIAAN ROGIERS', 'AMY DHAENE', 'AMéLIE DE BACKER', 
  'CéLESTINE LANGENBERG', 'DE VOS JULIETTE', 'JULIE VANRAES', "LEON VERFAILLIE",
  "LIZZ VANLEE", "HAECK LUKAS","MONALISA VANHOLME", "SAMUEL D'HAENENS","TOBIAS VERHEYE","AUGUST VAN DE WALLE",
  "VAN NIEUWENHUYSE FéLIX")
A = Second[Second$STUDENTEN %in% infirst, ]
mean(A$Score)  
  
#### Topsportschool Gent ####
posttest$FECHA = as.character(posttest$startdate)
school = sqldf("
      SELECT  FECHA STARTDATE ,   replace(UPPER(G01Q01), '_', ' ') STUDENTEN,
      G01Q04 CITY,
      SCORE FROM posttest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%top%'  
      And Lower(G01Q04) LIKE 'g%'  
 
      ORDER BY FECHA
      ")
school

pretest$FECHA = as.character(pretest$startdate)
school = sqldf("
      SELECT  FECHA STARTDATE ,   replace(UPPER(G01Q01), '_', ' ') STUDENTEN,
      G01Q04 CITY, G01Q05 School
      --SCORE 
      FROM pretest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%top%'  
      And Lower(G01Q04) LIKE 'g%'  
 
      ORDER BY STUDENTEN
      ")
school



####  Heilige Familie Ieper of Technisch Instituut Heilige Familie  #### 
posttest$FECHA = as.character(posttest$startdate)
school = sqldf("
      SELECT  FECHA STARTDATE ,   replace(UPPER(G01Q01), '_', ' ') STUDENTEN,
      G01Q04 CITY,
      SCORE FROM posttest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%heilige%'  or
      lower(replace(G01Q05, ' ', '')) LIKE '%familie%' 
      And Lower(G01Q04) LIKE 'g%'  
 
      ORDER BY STUDENTEN
      ")
school



######## Kobos ####

posttest$FECHA = as.character(posttest$startdate)
A = posttest[ , c('G01Q01',   'G01Q04', 'G01Q05', "Score", "FECHA")]
school = sqldf("
      SELECT  FECHA STARTDATE ,   replace(UPPER(G01Q01), '_', ' ') STUDENTEN,
      --G01Q04 CITY,
      SCORE FROM posttest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%kobos%'  
 
      ORDER BY STUDENTEN
      ")
school
