# Tanzania National Panel Survey Wave 5 data analysis - 3
# R. Goto

# Adult Female Equivalent

# Settings

install.packages("tidyverse")
install.packages("summarytools")
library(tidyverse)
library(summarytools)

# open dataB - contains sex (MALE, FEMALE), age, eat7d (NO, YES)

dataB <- read_csv("dataB.csv")

# data B - change names of hh_b15_1 (biological mother is HH?, 'mother is the member of HH') and hh_b15_2 (mother's Roster ID)

dataB_lact <- dataB %>% 
  rename(mo = hh_b15_1,
         mo_id = hh_b15_2
         )

# dataB_lact to identify lactated children

dataB_lact <- dataB_lact %>%
  mutate(lact_c = case_when(
    (age<=1 & mo == 'MOTHER IS A MEMBER OF HOUSEHOLD' & eat7d == 'YES') ~ 1))

# identify lactating mothers with lactated children

dataB_lactc <- dataB_lact %>% 
  filter(lact_c == 1)
view(dataB_lactc)

dataB_lactc1 <- dataB_lactc %>% 
  select(sdd_hhid, mo_id, lact_c)

dataB_lactc1 <- dataB_lactc1 %>% 
  rename(lact_m = lact_c) # make code as lactating mother before merging

# making lactating mother (lact_m = 1) and lactated children (lact_c = 1)
dataB_lactm <- left_join(dataB_lact, dataB_lactc1, by = c("sdd_hhid" = "sdd_hhid", "sdd_indid" = "mo_id"))


# to check - found max 5 mothers and 5 children in household 0610-103-002
lact_checkc <- dataB_lactm %>% 
  count(sdd_hhid, lact_c) %>% 
  filter(lact_c>0)

lact_checkm <- dataB_lactm %>% 
  count(sdd_hhid, lact_m) %>% 
  filter(lact_m>0)


# AFE factor

# 0-11.9m - 0.29
# 1-3.9y - 0.44
# 4-6.9y - 0.58
# 7-10.9y - 0.73
# 11-14.9y & M - 0.93
# 11-14.9y & F - 0.87
# 15-17.9y & M - 1.14
# 15-17.9y & F & NA - 1.05
# 15-17.9y & F & lactating - 1.29
# 18-29.9y & M - 1.20
# 18-29.9y & F & NA - 1.00
# 18-29.9y & F & lactating - 1.24
# 30-59.9y & M - 1.22
# 30-59.9y & F & NA - 1.01
# 30-59.9y & F & lactating - 1.25
# >=60y & M - 1.00
# >=60y & F - 0.89

#create a trial file
dfB_check <- dataB_lactm %>% 
  select(sdd_hhid, sdd_indid, age, sex, lact_c, lact_m, eat7d)

dataB_AFE <- dataB_lactm %>%
  mutate(AFE = case_when(
    (age==0 & eat7d=='YES') ~ 0.29,
    (age>=1 & age<4 & eat7d=='YES') ~ 0.44,
    (age>=4 & age<7 & eat7d=='YES') ~ 0.58,
    (age>=7 & age<11 & eat7d=='YES') ~ 0.73,
    (age>=11 & age<15 & eat7d=='YES' & sex=='MALE') ~ 0.93,
    (age>=11 & age<15 & eat7d=='YES' & sex=='FEMALE') ~ 0.87,
    (age>=15 & age<18 & eat7d=='YES' & sex=='MALE') ~ 1.14,
    (age>=15 & age<18 & eat7d=='YES' & sex=='FEMALE' & is.na(lact_m)) ~ 0.93,
    (age>=15 & age<18 & eat7d=='YES' & sex=='FEMALE' & lact_m==1) ~ 1.29,
    (age>=18 & age<30 & eat7d=='YES' & sex=='MALE') ~ 1.20,
    (age>=18 & age<30 & eat7d=='YES' & sex=='FEMALE' & is.na(lact_m)) ~ 1.00,
    (age>=18 & age<30 & eat7d=='YES' & sex=='FEMALE' & lact_m==1) ~ 1.24,
    (age>=30 & age<60 & eat7d=='YES' & sex=='MALE') ~ 1.22,
    (age>=30 & age<60 & eat7d=='YES' & sex=='FEMALE' & is.na(lact_m)) ~ 1.01,
    (age>=30 & age<60 & eat7d=='YES' & sex=='FEMALE' & lact_m==1) ~ 1.25,
    (age>=60 & eat7d=='YES' & sex=='MALE') ~ 1.00,
    (age>=60 & eat7d=='YES' & sex=='FEMALE') ~ 0.89))

view(dfSummary(dataB_AFE))
    
# calculate household AFE

dataB_AFE_hh <- dataB_AFE %>%
  group_by(sdd_hhid) %>%
  summarise(AFE_hh = sum(AFE))

# save AFE file

write_csv(dataB_AFE_hh, 'df_AFE.csv')

