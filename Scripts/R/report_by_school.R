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
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%tabrasschaat%'  
      ORDER BY startdate
      ")
posttest$FECHA = as.character(posttest$startdate)
sqldf("
      SELECT  FECHA STARTDATE ,   UPPER(G01Q01) STUDENTEN, SCORE FROM posttest
      WHERE  lower(replace(G01Q05, ' ', '')) LIKE '%tabrasschaat%'  
      ORDER BY startdate
      ")
