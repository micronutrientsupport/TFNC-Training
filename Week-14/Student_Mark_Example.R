library(tidyverse)
library(readxl)
sr <- read_excel("hces-data/Student Results.xlsx",skip = 1)
View(sr)

names(sr)

sr <- rename(sr,StudentName = Name,
             Mark= "TOTAL (100)",
             UE = "UE (60)",
             CW = "CW (40)")

# sr <- select(sr,StudentName,Mark)

sr <- mutate(sr,Grade=case_when(Mark>=70~"A",
                                  Mark>=60&Mark<70 ~"B+",
                                  Mark>=50&Mark<60 ~"B",
                                  Mark>=40&Mark<50 ~"C",
                                  Mark>=35&Mark<40 ~"D",
                                  Mark<35 ~"F",
                                TRUE~"UNGRADED"))

sr <- mutate(sr,Grade = case_when(is.na(UE)~"Postponed",
                                    TRUE~Grade))

View(sr)

sr <- mutate(sr,Grade = as_factor(Grade))

summary(sr)

hist(sr$Mark)

sr <- mutate(sr,MarkR = 0.8*CW+(0.2*UE)

             