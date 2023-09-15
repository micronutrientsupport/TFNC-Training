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

View(df_wheatA)

# get households that spent money on wheat flour in the last 7 days

df_wheatA_con <- df_wheatA %>% 
  filter(total_val_n >= 1)

# 2319HHs were consumed wheat flour
# mean within the households was 138.5 Rupees, median 100 Rupees

m <- mean(df_wheatA_con$total_val_n, na.rm = TRUE)
print(m)

med <- median(df_wheatA_con$total_val_n, na.rm = TRUE)
print(med)

hist(df_wheatA_con$total_val_n)

df_monetary <- df_wheatA_con %>%
  summarise(m = mean(total_val_n, na.rm = TRUE),
            med = median(total_val_n, na.rm = TRUE),
            sdiv = sd(total_val_n, na.rm = TRUE),
            quantile = quantile(total_val_n, na.rm = TRUE),
            IQR = IQR(total_val_n, na.rm = TRUE))

# consumption in weight (g)

df_wheatA_con <- df_wheatA_con %>% 
  mutate(home_g = ifelse(home_u==2, home_q, home_q),
         home_g = ifelse(home_u==1, home_q*1000, home_g))

df_wheatA_con <- df_wheatA_con %>% 
  mutate(purchase_g = ifelse(purchase_u==2, purchase_q, purchase_q),
         purchase_g = ifelse(purchase_u==1, purchase_q*1000, purchase_g))

df_wheatA_con <- df_wheatA_con %>% 
  mutate(home_g = ifelse(is.na(home_g), 0, home_g),
         purchase_g = ifelse(is.na(purchase_g), 0, purchase_g),
         cons_g = home_g + purchase_g)


hist(df_wheatA_con$cons_g)
boxplot(df_wheatA_con$cons_g)

df_wheatA_con <- df_wheatA_con %>% 
  mutate(cons_g = ifelse(cons_g>25000, 0, cons_g))

df_cons <- df_wheatA_con %>%
  summarise(m = mean(cons_g, na.rm = TRUE),
            med = median(cons_g, na.rm = TRUE),
            sdiv = sd(cons_g, na.rm = TRUE),
            quantile = quantile(cons_g, na.rm = TRUE),
            IQR = IQR(cons_g, na.rm = TRUE))


# Q3 how many days did households consumed wheat flour?

hist(df_wheatA_con$daysconsumed)
boxplot(df_wheatA_con$daysconsumed)

df_days <- df_wheatA_con %>%
  summarise(m = mean(daysconsumed, na.rm = TRUE),
            med = median(daysconsumed, na.rm = TRUE),
            sdiv = sd(daysconsumed, na.rm = TRUE),
            quantile = quantile(daysconsumed, na.rm = TRUE),
            IQR = IQR(daysconsumed, na.rm = TRUE))

# Q4 - 41% of households were consumed wheat flour from own products, whereas 59% of households purchased wheat flour from the market.

df_wheatA_home <- df_wheatA_con %>% 
  filter(home_g>0)

answer <- (951/2319)*100
View(answer)

df_wheatA_pur <- df_wheatA_con %>% 
  filter(purchase_g>0)

answer2 <- (1372/2319)*100
View(answer2)

# Q5 

# open dataB

DataB <- read.csv("~/MAPS/TFNC/TFNCassingment/Data_file_B.csv")

# select variables to make a sub-dataset

names(DataB)

DataB_res <- DataB %>% 
  select(psu, hhno, urbrur)

# merge files

DataAB <- left_join(df_wheatA_con, DataB_res, by = c("psu", "hhno"))

# calculate price paid for 100g of wheat flour

DataAB <- DataAB %>% 
  mutate(per100g = (purchase_val_n/cons_g)*100,
         per100g = round(per100g, 2))

DataAB_u <- DataAB %>% 
  filter(urbrur==1)

boxplot(DataAB_u$per100g)

DataAB_r <- DataAB %>% 
  filter(urbrur==2)
  
boxplot(DataAB_r$per100g)




