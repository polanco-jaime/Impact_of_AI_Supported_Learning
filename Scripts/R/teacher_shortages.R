library(arrow)
teacher_shortages = arrow::read_parquet('~/Downloads/vdab_jobs_2025_01_24.parquet')

teacher_shortages = teacher_shortages[is.na(teacher_shortages$Email)==F,]
teacher_shortages = teacher_shortages[is.na(teacher_shortages$Title)==F,]
teacher_shortages = teacher_shortages[(teacher_shortages$Timing2)>='2025-01-01',]
teacher_shortages
write.xlsx(teacher_shortages, file = "teacher_shortages.xlsx")
