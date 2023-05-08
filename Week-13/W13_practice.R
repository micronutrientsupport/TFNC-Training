# TFNC training on 02/05/23 by RG
# Checking outliers

##calculate number of household members who eat food in the last 7 days

# open HH_SEC_B.csv

library(tidyverse)

dataB <- read_csv("HH_SEC_B.csv")

# change variable names for age, sex and eating household members

dataB <- dataB %>%
  rename(sex = hh_b02, age = hh_b04, eat7d = hh_b07)

str(dataB)

dataB <- dataB %>%
  mutate(
    sex = as.factor(sex),
    eat7d = as.factor(eat7d),
    sdd_hhid = as.factor(sdd_hhid)
  )

# Get the number of people who consumed food in the last 7 days - if there are no members who eat in the HH, there is no YES count - so if I choose only Yes, the household will be omitted.

#for example;
df_eat7d <- 
  count(dataB, sdd_hhid, eat7d, name = "eat7d_p")

df_eat7d <- 
  count(dataB, sdd_hhid, eat7d, .drop = FALSE, name = "eat7d_p")
df_eat7d <- df_eat7d %>%
  filter(eat7d =="YES")
#omit eat7d
df_eat7d <- df_eat7d %>%
  select(-eat7d)



#merge with the dataJ1 with EP and actual amount consumed in grams (cons_g_hh)

dataJ1_v2<- left_join(dataJ1, df_eat7d, by = "sdd_hhid")

View(dataJ1_v2)
summary(dataJ1_v2)
view(dfSummary(dataJ1_v2))


#calculate per capita per day of consumption

dataJ1_v2 <- dataJ1_v2 %>% 
  mutate(cons_g_pppd = cons_g_hh/eat7d_p/7) 
dataJ1_v2 <- dataJ1_v2 %>% 
  mutate(cons_g_pppd = round(cons_g_pppd, 1))

# new tip! controlling the digit numbers of numeric variables 
options(digits=1)

# scipen - controlling for the length - the default 100000 (1e+5)
options(scipen = 4, digits=3) 

View(dataJ1_v2)
summary(dataJ1_v2)
view(dfSummary(dataJ1_v2))

#change Inf to NA - use na_if (if Inf, change to NA)

dataJ1_v2 <- dataJ1_v2 %>%
  mutate(cons_g_pppd = na_if(cons_g_pppd, Inf))

view(dfSummary(dataJ1_v2))



##check outliers by each food item 
#107 millet & sorghum

millet <- dataJ1_v2 %>% 
  filter(item_id == 107) %>% 
  select(sdd_hhid, item_id, itemcode, cons_unit, cons_quant, cons_g_hh, eat7d_p, cons_g_pppd)

hist(millet$cons_g_pppd)
boxplot(millet$cons_g_pppd)
qqnorm(millet$cons_g_pppd)
qqline(millet$cons_g_pppd)

#log10

millet <- millet %>% 
  mutate(millet_lg10=log10(cons_g_pppd))

hist(millet$millet_lg10)
qqnorm(millet$millet_lg10)
qqline(millet$millet_lg10)

summary(millet$millet_lg10)

# SD
sd(millet$millet_lg10) # including NA, it does not work!
sd(millet$millet_lg10, na.rm = TRUE)

#skewness - remove NA using na.rm
moments::skewness(millet$millet_lg10, na.rm = TRUE)
sur::se.skew(millet$millet_lg10)

#summary - package 'psych', function 'describe'

install.packages("psych")
library(psych)
describe(millet)

summillet <- describe(millet$millet_lg10)

#get cut-off points at +3SD, +4SD and +5SD
summillet <- summillet %>% 
  mutate(sd3cf=sd*3+mean) %>% 
  mutate(sd4cf=sd*4+mean) %>% 
  mutate(sd5cf=sd*5+mean)


#practice - try;
#102 rice (husked)
#601 onion
#1001 oil
#1003 salt