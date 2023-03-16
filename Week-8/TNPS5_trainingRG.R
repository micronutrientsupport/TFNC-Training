##TFNC training 21 Feb 2023 by RG

##Part 1. Identify the dataset and variables to use in the household food consumption analysis

#1.1 Pick up and discuss the data;
# HH_SEC_J1.csv - 60 food items
# HH_SEC_J3.csv - 10 food categories
# HH_SEC_J4.csv - share food with any non-HH members in the last 7 days
# HH_SEC_A.csv - HH location, data of interview
# HH_SEC_B.csv - individual id, age, sex, question 'did you eat meals in this HH in the last 7 days?'
# HH_SEC_C.csv - education
# HH_SEC_F.csv - food consumption outside HH (Y/N)
# HH_SEC_M.csv - assets
# HH_SEC_V.csv - anthropometry (weight, height, MUAC)

#1.2 First, focus on HH_SEC_J1


#Part 2. Data cleaining on HH_SEC_J1 - working on the food consumption data

#2.1. import HH_SEC_J1.csv

library(tidyverse)

dataJ1 <- read_csv("HH_SEC_J1.csv") 

#2.2. change variable names

#starting from a simple example
dataJ1 <- dataJ1 %>% 
  rename(cons_yn=hh_j01)

#change other variables

dataJ1 <- dataJ1 %>% 
  rename(cons_yn=hh_j01, 
       cons_unit=hh_j02_1,
       cons_quant=hh_j02_2,
       pur_unit=hh_j03_1,
       pur_quant=hh_j03_2,
       pur_THS=hh_j04,
       prod_unit=hh_j05_1,
       prod_quant=hh_j05_2,
       gift_unit=hh_j06_1,
       gift_quant=hh_j06_2
       )

names(dataJ1)
str(dataJ1)

#2.3. In pur_quant, prod_quant, gift_quant - there are NONE rather using NA. Therefore the variable type becomes character. It should be converted from NONE to NA.
# introduce function na_if

dataJ1 <- dataJ1 %>%
  mutate(pur_quant = na_if(pur_quant, "NONE"),
         prod_quant = na_if(prod_quant, "NONE"),
         gift_quant = na_if(gift_quant, "NONE"),
         )

#2.4. change variable types

dataJ1 <- dataJ1 %>% 
  mutate(itemcode=as.factor(itemcode),
         cons_yn=as.factor(cons_yn),
         cons_unit=as.factor(cons_unit),
         cons_quant=as.numeric(cons_quant),
         pur_unit=as.factor(pur_quant),
         pur_quant=as.numeric(pur_quant),
         prod_unit=as.factor(prod_unit),
         prod_quant=as.numeric(prod_quant),
         gift_unit=as.factor(gift_unit),
         gift_quant=as.numeric(gift_quant)
         )

str(dataJ1)

#2.5 counting frequency

food_freq <- count(dataJ1,itemcode,cons_yn)
food_freq

#select only Yes cases in cons_yn
food_freq_Y <- filter(food_freq,cons_yn=="YES")
food_freq_Y

#calculate and add percentage in food_freq_Y
food_freq_Y <- food_freq_Y %>% 
  mutate(percentage=n/1184*100)

#adjust decimal places
??

#2.6 unit check by food items

food_unit <- count(dataJ1,itemcode,cons_unit)
food_unit

#2.7 check unit - select the food items using PIECES, or Litre and Millilitre - need to discuss how to calculated them.

count(dataJ1, itemcode, cons_unit)

food_unit_P <- 
  count(dataJ1, itemcode, cons_unit)%>% 
  filter(cons_unit=="PIECES")

food_unit_L <- 
  count(dataJ1, itemcode, cons_unit)%>% 
  filter(cons_unit=="LITRE")     

food_unit_mL <- 
  count(dataJ1, itemcode, cons_unit) %>% 
  filter(cons_unit=="MILLILITRE")

##count household members who eat/drink in the HH in the last 7 days

#open HH_SEC_B.csv

dataB <- read_csv("HH_SEC_B.csv") 

#change variable names for age, sex and eating household members

dataB <- dataB %>% 
  rename(sex=hh_b02, age=hh_b04, eat7d=hh_b07)

str(dataB)

dataB <- dataB %>% 
  mutate(sex=as.factor(sex),
         eat7d=as.factor(eat7d),
         sdd_hhid=as.factor(sdd_hhid))

str(dataB)

#counting number of members who eat in this HH in the last 7 days

eat7d_Y <- count(dataB,sdd_hhid,eat7d)
summary(eat7d_Y)
#there are 3 NA in eat7d, max 34 (outliner?) in the number
#if there are no members who eat in the HH, there is no YES count - so if I choose only Yes, the household will be omitted.

hh_eat7d_all <- count(dataB,sdd_hhid, eat7d,.drop = FALSE,name="hh_pop")

hh_eat7d_all <- hh_eat7d_all[hh_eat7d_all$eat7d=="YES",]
summary(hh_eat7d_all)

hh_eat7d_all[hh_eat7d_all$eat7d=="YES",c(1,3)]
summary(hh_eat7d_all)

#what do you think? Any other sophisticated way??






