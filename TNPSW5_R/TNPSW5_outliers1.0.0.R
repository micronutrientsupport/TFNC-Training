# Tanzania National Panel Survey Wave 5 data analysis - 2
# R. Goto

# Checking outliers

# Settings

install.packages("tidyverse")
install.packages("summarytools")
library(tidyverse)
library(summarytools)

# To check the outliers, food consumption amount per person per day will be used to identify the odd values
# First, calculate number of household members who consumed foods in the last 7 days (hh_b07) from HH_SEC_B.csv

# open HH_SEC_B.csv

dataB <- read_csv("HH_SEC_B.csv")

# change variable names for age (hh_b04), sex (hh_b02) and eating household members (hh_b07)

dataB <- dataB %>%
  rename(sex = hh_b02, age = hh_b04, eat7d = hh_b07) %>% 
  mutate(
    sex = as.factor(sex),
    eat7d = as.factor(eat7d),
    sdd_hhid = as.factor(sdd_hhid)
  )

# save dataB

write_csv(dataB, 'dataB.csv')

# Count the number of people who consumed food in the household in the last 7 days (count Yes in eat7d) - if there are no members who eat in the HH (YES=0), the household will be omitted. therefore used .drop = FALSE

df_eat7d <- 
  count(dataB, sdd_hhid, eat7d, .drop = FALSE, name = "eat7d_p")

df_eat7d <- df_eat7d %>%
  filter(eat7d =="YES")

#omit eat7d

df_eat7d <- df_eat7d %>%
  select(-eat7d)

# open data J1 to merge

dataJ1 <- read_csv("dataJ1.csv")

#merge df_eat7d with the dataJ1.csv

dataJ1_pc<- left_join(dataJ1, df_eat7d, by = "sdd_hhid")

View(dataJ1_pc)
summary(dataJ1_pc)
view(dfSummary(dataJ1_pc))

names(dataJ1)

#calculate per capita per day of consumption and adjust decimal place as 1

dataJ1_pc <- dataJ1_pc %>% 
  mutate(cons_g_pc = cons_g_hh/eat7d_p/7) 
dataJ1_pc <- dataJ1_pc %>% 
  mutate(cons_g_pc = round(cons_g_pc, 1))

View(dataJ1_pc)
summary(dataJ1_pc)
view(dfSummary(dataJ1_pc))

#change Inf to NA - use na_if (if Inf, change to NA)

dataJ1_pc <- dataJ1_pc %>%
  mutate(cons_g_pc = na_if(cons_g_pc, Inf))

view(dfSummary(dataJ1_pc))

# checking outliers - log10

hist(dataJ1_pc$cons_g_pc)

dataJ1_pc <- dataJ1_pc %>% 
  mutate(cons_g_pc_lg10=log10(cons_g_pc))

hist(dataJ1_pc$cons_g_pc_lg10)

# calculate mean, median, sd and maximum values in each food item

df_cutoffs <- dataJ1_pc %>% 
  group_by(item_id, itemcode) %>%
  summarise(
    mean = mean(cons_g_pc_lg10, na.rm = TRUE),
    median = median(cons_g_pc_lg10, na.rm = TRUE),
    sd = sd(cons_g_pc_lg10, na.rm = TRUE),
    max = max(cons_g_pc_lg10, na.rm = TRUE)
  )

View(df_cutoffs)

# calculate cut-off points at +3SDs, +4SDs, +5SDs

df_cutoffs <- df_cutoffs %>% 
  mutate(sd3cf=sd*3+mean) %>%
  mutate(sd4cf=sd*4+mean) %>% 
  mutate(sd5cf=sd*5+mean)

names(df_cutoffs)

df_cutoffs <- df_cutoffs %>% 
  select(item_id, sd3cf, sd4cf, sd5cf)

#merge these cut-off points in dataJ1_pc
# but before merging the variable, omit itemcode in df_cutoff

dataJ1_cf <- dataJ1_pc %>% 
  left_join(., df_cutoffs, by=("item_id"))

View(dataJ1_cf)

# marking the variables within the cutoff points - ifelse(test, yes, no)

dataJ1_cf <- dataJ1_cf %>% mutate(sd3cut = cons_g_pc_lg10-sd3cf)
dataJ1_cf <- dataJ1_cf %>% mutate(in3sd = ifelse(sd3cut<0, 1, NA)) 

dataJ1_cf <- dataJ1_cf %>% mutate(sd4cut = cons_g_pc_lg10-sd4cf)
dataJ1_cf <- dataJ1_cf %>% mutate(in4sd = ifelse(sd4cut<0, 1, NA))

dataJ1_cf <- dataJ1_cf %>% mutate(sd5cut = cons_g_pc_lg10-sd5cf)
dataJ1_cf <- dataJ1_cf %>% mutate(in5sd = ifelse(sd5cut<0, 1, NA))

view(dfSummary(dataJ1_cf))

# using cut-off at 3SD, 4SD and 5SD is N=15963, 15989, 15992 - so difference between 3 and 5SD is only 29
# decision - use +3SDs as cut-off points
# get cleaned household consumption in g

dataJ1_cf <- dataJ1_cf %>% 
  mutate(hh_cons = ifelse(in3sd == 1, cons_g_hh, NA))

# get cleaned per capita consumption

dataJ1_cf <- dataJ1_cf %>% 
  mutate(hh_cons_pc = ifelse(in3sd == 1, cons_g_pc, NA))

view(dfSummary(dataJ1_cf))


# draw histgram of food comsumption per capita per day

# salt comsumption pcpd (1003 is SALT)

df_salt <- dataJ1_cf %>% 
  filter(item_id == 1003) 
hist(df_salt$hh_cons_pc, xlab="salt intake (g/c/d)")

# repeat for cooking oil (1001), maize flour (105), wheat flour (1081), sugar (301), rice (husked)

df_oil <- dataJ1_cf %>% 
  filter(item_id == 1001) 
hist(df_oil$hh_cons_pc, xlab="oil intake (g/c/d)")

df_maizef <- dataJ1_cf %>% 
  filter(item_id == 105) 
hist(df_maizef$hh_cons_pc, xlab="maize flour intake (g/c/d)")

df_wheatf <- dataJ1_cf %>% 
  filter(item_id == 1081) 
hist(df_wheatf$hh_cons_pc, xlab="wheat flour intake (g/c/d)")

df_cake <- dataJ1_cf %>% 
  filter(item_id == 110) 
hist(df_cake$hh_cons_pc, xlab="buns, cakes and biscuits intake (g/c/d)")

df_spa <- dataJ1_cf %>% 
  filter(item_id == 111) 
hist(df_spa$hh_cons_pc, xlab="macaroni and spaghetti intake (g/c/d)")

df_bread <- dataJ1_cf %>% 
  filter(item_id == 109) 
hist(df_bread$hh_cons_pc, xlab="bread intake (g/c/d)")

df_riceh <- dataJ1_cf %>% 
  filter(item_id == 102) 
hist(df_riceh$hh_cons_pc, xlab="rice (husked) intake (g/c/d)")

df_riceh <- dataJ1_cf %>% 
  filter(item_id == 301) 
hist(df_riceh$hh_cons_pc, xlab="sugar intake (g/c/d)")

df_riceh <- dataJ1_cf %>% 
  filter(item_id == 1104) 
hist(df_riceh$hh_cons_pc, xlab="bottled/ canned soft drinks (soda, juice, water) intake (g/c/d)")

df_riceh <- dataJ1_cf %>% 
  filter(item_id == 901) 
hist(df_riceh$hh_cons_pc, xlab="fresh milk intake (g/c/d)")

df_riceh <- dataJ1_cf %>% 
  filter(item_id == 902) 
hist(df_riceh$hh_cons_pc, xlab="milk products (cream, cheese, yoghurt) intake (g/c/d)")

df_riceh <- dataJ1_cf %>% 
  filter(item_id == 301) 
hist(df_riceh$hh_cons_pc, xlab="sugar intake (g/c/d)")


#save data for household consumption in gram after considering conversion factors and cleaned by +3SDs cut-off

write_csv(dataJ1_cf, 'dataJ1_hhcons.csv')











# select without outliers - check

df_cut5 <- df_clean1 %>% 
  filter(in5sd==1) %>% 
  group_by(item_id, itemcode) %>% 
  summarise(
    mean5 = mean(cons_g_pc, na.rm = TRUE),
    median5 = median(cons_g_pc, na.rm = TRUE),
    sd5 = sd(cons_g_pc, na.rm = TRUE),
    max5 = max(cons_g_pc, na.rm = TRUE)
  )

df_cut4 <- df_clean1 %>% 
  filter(in4sd==1) %>% 
  group_by(item_id, itemcode) %>% 
  summarise(
    mean4 = mean(cons_g_pc, na.rm = TRUE),
    median4 = median(cons_g_pc, na.rm = TRUE),
    sd4 = sd(cons_g_pc, na.rm = TRUE),
    max4 = max(cons_g_pc, na.rm = TRUE)
  )

df_cut3 <- df_clean1 %>% 
  filter(in3sd==1) %>% 
  group_by(item_id, itemcode) %>% 
  summarise(
    mean3 = mean(cons_g_pc, na.rm = TRUE),
    median3 = median(cons_g_pc, na.rm = TRUE),
    sd3 = sd(cons_g_pc, na.rm = TRUE),
    max3 = max(cons_g_pc, na.rm = TRUE)
  )

#omit itemcode
df_cut4 <- df_cut4 %>%
  select(-itemcode)
df_cut5 <- df_cut5 %>%
  select(-itemcode)

df_cut_all <- df_cut3 %>% 
  left_join(., df_cut4, by=("item_id")) %>% 
  left_join(., df_cut5, by=("item_id"))

names(df_cut_all)

df_cut_all <- relocate(df_cut_all, max4, .after=max5)
df_cut_all <- relocate(df_cut_all, max3, .after=max4) 
  
write.csv(df_cut_all, 'cutoffs_max.csv')


#check the distribution
#millet

millet <- dataJ1_v3 %>% 
  filter(item_id == 107) 


millet3sd <- dataJ1_v3 %>% 
  filter(item_id == 107 & outlier3 == 1)

hist(millet3sd$cons_g_pppd_lg10)
boxplot(millet3sd$cons_g_pppd_lg10)

millet4sd <- dataJ1_v3 %>% 
  filter(outlier4 == 1)

hist(millet4sd$cons_g_pppd_lg10)
boxplot(millet4sd$cons_g_pppd_lg10)

# check the outliners by food items

df_out <- dataJ1_v2 %>%
  describeBy(dataJ1_v2, itemcode)

dataA5 <- read_csv("hh_sec_a_5.csv")
str(dataA5)
view(dfSummary(dataA5))
