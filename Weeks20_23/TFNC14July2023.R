install.packages("summarytools")
library(tidyverse)
library(summarytools)

# open data A

library(readr)
DataA <- read.csv("~/MAPS/TFNC/TFNCassingment/Data_file_A.csv")
View(DataA)
view(dfSummary(DataA))
str(DataA)
names(DataA)

# select wheat flour from each HH and identify the house consumed the wheat flour (as 1)

df_wheatA <- DataA %>% 
  filter(foodcode==116)
df_wheatA2 <- df_wheatA %>% 
  filter(daysconsumed >= 1)

(2337/4500)*100

# Q1 answer: 2337 (51.9%)

# Or:

df_wheat <- DataA %>% 
  filter(foodcode == 116) %>% 
  count(daysconsumed) %>% 
  mutate(per=(n/4500)*100)

df_wheat1 <- DataA %>% 
  filter(foodcode == 116) %>% 
  mutate(cons_yn = case_when((daysconsumed >= 1) ~ 1,TRUE ~ 0)) %>% 
  count(cons_yn) %>% 
  mutate(per=(n/4500)*100) %>% 
  mutate(per = round(per, 1))

df_wheat2 <- DataA %>% 
  filter(foodcode == 116) %>% 
  mutate(cons_yn = if_else(daysconsumed>=1, 1, 0)) %>% 
  count(cons_yn) %>% 
  mutate(per=(n/4500)*100) %>% 
  mutate(per = round(per, 1))



# counting HH consumed the food items.

names(df_wheatA)

df_wheatA <- df_wheatA %>% 
  mutate(home_val_n = ifelse(is.na(home_val), 0, home_val)) %>% 
  mutate(purchase_val_n = ifelse(is.na(purchase_val), 0, purchase_val)) %>% 
  mutate(total_val_n = home_val_n + purchase_val_n)


  
  






count_fooditem <- DataA %>% 
  mutate(consYN = if_else(daysconsumed>0, 1, 0)) %>% 
  group_by(foodname) %>% 
  count(consYN) %>% 
  mutate(per=(n/4500*100)) %>%  # get percentage
  mutate(per = round(per, 1)) # decimal place is 1

