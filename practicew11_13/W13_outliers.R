# TFNC training on 02/05/23 by RG
# Checking outliers

#continue from W11_unitconversion.R


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

#102 rice (husked)

riceh <- dataJ1_v2 %>% 
  filter(item_id == 102)%>% 
  select(sdd_hhid, item_id, itemcode, cons_unit, cons_quant, cons_g_hh, eat7d_p, cons_g_pppd)

View(riceh)

hist(riceh$cons_g_pppd)
boxplot(riceh$cons_g_pppd)
qqnorm(riceh$cons_g_pppd)
qqline(riceh$cons_g_pppd)

#log10

riceh <- riceh %>% 
  mutate(riceh_lg10=log10(cons_g_pppd))

hist(riceh$riceh_lg10)
qqnorm(riceh$riceh_lg10)
qqline(riceh$riceh_lg10)

View(riceh)

# SD
sd(riceh$riceh_lg10, na.rm = TRUE)

#skewness - remove NA using na.rm
moments::skewness(riceh$riceh_lg10, na.rm = TRUE)
sur::se.skew(riceh$riceh_lg10)

#summary
sumriceh <- describe(riceh$riceh_lg10)

#get cut-off points at +3SD, +4SD and +5SD
sumriceh <- sumriceh %>% 
  mutate(sd3cf=sd*3+mean) %>% 
  mutate(sd4cf=sd*4+mean) %>% 
  mutate(sd5cf=sd*5+mean)



#601 onion

onion <- dataJ1_v2 %>% 
  filter(item_id == 601)%>% 
  select(sdd_hhid, item_id, itemcode, cons_unit, cons_quant, cons_g_hh, eat7d_p, cons_g_pppd)

View(onion)
summary(onion$cons_g_pppd, na.rm = TRUE)

hist(onion$cons_g_pppd)
boxplot(onion$cons_g_pppd)
qqnorm(onion$cons_g_pppd)
qqline(onion$cons_g_pppd)

#log10

onion <- onion %>% 
  mutate(onion_lg10=log10(cons_g_pppd))

hist(onion$onion_lg10)
qqnorm(onion$onion_lg10)
qqline(onion$onion_lg10)

summary(onion$onion_lg10)

# SD
sd(onion$onion_lg10, na.rm = TRUE)

#skewness - remove NA using na.rm
moments::skewness(onion$onion_lg10, na.rm = TRUE)
sur::se.skew(onion$onion_lg10)

#summary
sumonion <- describe(onion$onion_lg10)

#get cut-off points at +3SD, +4SD and +5SD
sumonion <- sumonion %>% 
  mutate(sd3cf=sd*3+mean) %>% 
  mutate(sd4cf=sd*4+mean) %>% 
  mutate(sd5cf=sd*5+mean)



#1001 oil

oil <- dataJ1_v2 %>% 
  filter(item_id == 1001)%>% 
  select(sdd_hhid, item_id, itemcode, cons_unit, cons_quant, cons_g_hh, eat7d_p, cons_g_pppd)

View(oil)
summary(oil$cons_g_pppd, na.rm = TRUE)

hist(oil$cons_g_pppd)
boxplot(oil$cons_g_pppd)
qqnorm(oil$cons_g_pppd)
qqline(oil$cons_g_pppd)

#log10

oil <- oil %>% 
  mutate(oil_lg10=log10(cons_g_pppd))

hist(oil$oil_lg10)
qqnorm(oil$oil_lg10)
qqline(oil$oil_lg10)

summary(oil$oil_lg10)

# SD
sd(oil$oil_lg10, na.rm = TRUE)

#skewness - remove NA using na.rm
moments::skewness(oil$oil_lg10, na.rm = TRUE)
sur::se.skew(oil$oil_lg10)

#summary
sumoil <- describe(oil$oilh_lg10)

#get cut-off points at +3SD, +4SD and +5SD
sumoil <- sumoil %>% 
  mutate(sd3cf=sd*3+mean) %>% 
  mutate(sd4cf=sd*4+mean) %>% 
  mutate(sd5cf=sd*5+mean)


#1003 salt

salt <- dataJ1_v2 %>% 
  filter(item_id == 1003)
View(salt)
summary(salt)

hist(salt$cons_g_pppd)
boxplot(salt$cons_g_pppd)
qqnorm(salt$cons_g_pppd)
qqline(salt$cons_g_pppd)

#log10

salt <- salt %>% 
  mutate(salt_lg10=log10(cons_g_pppd))%>% 
  select(sdd_hhid, item_id, itemcode, cons_unit, cons_quant, cons_g_hh, eat7d_p, cons_g_pppd)

hist(salt$salt_lg10)
qqnorm(salt$salt_lg10)
qqline(salt$salt_lg10)

summary(salt$salt_lg10)

# SD
sd(salt$salt_lg10) # including NA, it does not work!
sd(salt$salt_lg10, na.rm = TRUE)

#skewness - remove NA using na.rm
moments::skewness(salt$salt_lg10, na.rm = TRUE)
sur::se.skew(salt$salt_lg10)

#summary
sumsalt <- describe(salt$salt_lg10)

#get cut-off points at +3SD, +4SD and +5SD
sumsalt <- sumsalt %>% 
  mutate(sd3cf=sd*3+mean) %>% 
  mutate(sd4cf=sd*4+mean) %>% 
  mutate(sd5cf=sd*5+mean)


#which variables have outliers?

# calculate log10 cons_g_pppd for all

dataJ1_v2 <- dataJ1_v2 %>% 
  mutate(cons_g_pppd_lg10=log10(cons_g_pppd))

summary(dataJ1_v2$cons_g_pppd_lg10)

# WILDBIRD and INSECTS cases was 0 so one -INF (minus infinity) in cons_g_pppd_log10 - delete -INF

dataJ1_v2 <- dataJ1_v2 %>%
  mutate(cons_g_pppd_lg10 = na_if(cons_g_pppd_lg10, -Inf))

#get summary data by food items

df_cutoff <- dataJ1_v2 %>% 
  group_by(item_id, itemcode) %>%
  summarise(
    mean = mean(cons_g_pppd_lg10, na.rm = TRUE),
    median = median(cons_g_pppd_lg10, na.rm = TRUE),
    sd = sd(cons_g_pppd_lg10, na.rm = TRUE),
    max = max(cons_g_pppd_lg10, na.rm = TRUE)
  )
View(df_cutoff)

# calculate cut-off points using +3SDs, +4SDs, +5SDs
df_cutoff <- df_cutoff %>% 
  mutate(sd3cf=sd*3+mean) %>%
  mutate(sd4cf=sd*4+mean) %>% 
  mutate(sd5cf=sd*5+mean)

#merge these cut-off points in dataJ1_v2
# but before merging the variable, omit itemcode in df_cutoff
df_cutoff <- df_cutoff %>% 
  select(-itemcode)

dataJ1_v3 <- dataJ1_v2 %>% 
  left_join(., df_cutoff, by=("item_id"))

View(dataJ1_v3)

# marking the variables outside of the cutoff points
#ifelse(test, yes, no)


dataJ1_v3 <- dataJ1_v3 %>% mutate(sd3out = sd3cf-max)
dataJ1_v3 <- dataJ1_v3 %>% mutate(outlier3 = ifelse(sd3out<0, 1, NA)) 

dataJ1_v3 <- dataJ1_v3 %>% mutate(sd4out = sd4cf-max)
dataJ1_v3 <- dataJ1_v3 %>% mutate(outlier4 = ifelse(sd4out<0, 1, NA))

dataJ1_v3 <- dataJ1_v3 %>% mutate(sd5out = sd5cf-max)
dataJ1_v3 <- dataJ1_v3 %>% mutate(outlier5 = ifelse(sd5out<0, 1, NA))

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

