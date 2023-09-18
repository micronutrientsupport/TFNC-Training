# Tanzania National Panel Survey Wave 5 data analysis - 1
# R. Goto

# Download the files 'National Panel Survey 2019-2020 - Extended Panel with Sex Disaggregated Data, Tanzania, 2019-2020'
# https://microdata.worldbank.org/index.php/catalog/3885

# Datasets

# HH_SEC_J1.csv - 1184 Household food consumption in the last 7 days with key 60 food items (1184 x 60 = 71040 obs.)
# HH_SEC_B.csv - Roster of household members and individual characteristics including: sex, age, eating food in the household in the last 7 days? (5587 obs)

# Household food consumption data

# Settings

install.packages("tidyverse")
install.packages("summarytools")
library(tidyverse)
library(summarytools)


# open HH_SEC_J1.csv

dataJ1 <- read_csv("HH_SEC_J1.csv")

view(dfSummary(dataJ1))

# change variable names

dataJ1 <- dataJ1 %>%
  rename(
    cons_yn = hh_j01,
    cons_unit = hh_j02_1,
    cons_quant = hh_j02_2,
    pur_unit = hh_j03_1,
    pur_quant = hh_j03_2,
    pur_TSH = hh_j04,
    prod_unit = hh_j05_1,
    prod_quant = hh_j05_2,
    gift_unit = hh_j06_1,
    gift_quant = hh_j06_2
  )

# change variable types (numeric removed NONE in cons_quant, pur_quant, gift_quant)

str(dataJ1)

dataJ1 <- dataJ1 %>% 
  mutate(
    pur_quant = as.numeric(pur_quant),
    prod_quant = as.numeric(prod_quant),
    gift_quant = as.numeric(gift_quant)
  )

view(dfSummary(dataJ1))

# add item_id for 60 food items so merge food-id.csv

# open food-id.csv

foodid <- read_csv("food-id.csv")

dataJ1 <- left_join(dataJ1, foodid, by = 'itemcode')
View(dataJ1)

# move id number before itemcode
dataJ1 <- relocate(dataJ1, item_id, .before = itemcode)

# identify food items using PIECES, or Litre and Millilitre

count(dataJ1, itemcode, cons_unit)

food_unit_P <-
  count(dataJ1, item_id, itemcode, cons_unit) %>%
  filter(cons_unit == "PIECES") %>%
  arrange(desc(n))

food_unit_LmL <-
  count(dataJ1, item_id, itemcode, cons_unit) %>%
  filter(cons_unit == "LITRE" | cons_unit == "MILLILITRE") %>% 
  arrange(desc(n))

# create unit conversation factor (unit_conv)

dataJ1 <- dataJ1 %>%
  mutate(unit_conv = case_when(
    (cons_unit == 'MILLILITRE' & item_id == 1001) ~ 0.91, # cooking oil
    (cons_unit == 'LITRE' & item_id == 1001) ~ 910,
    (cons_unit == 'MILLILITRE' & item_id == 901) ~ 1.03, # Fresh milk
    (cons_unit == 'LITRE' & item_id == 901) ~ 1030,
    (cons_unit == 'MILLILITRE' & item_id == 902) ~ 0.81, # Milk products
    (cons_unit == 'LITRE' & item_id == 902) ~ 820,
    (cons_unit == 'MILLILITRE' & item_id == 303) ~ 1.32, # Honey, syrups, etc.
    (cons_unit == 'LITRE' & item_id == 303) ~ 1320,     
    (cons_unit == 'MILLILITRE' & item_id == 1107) ~ 0.95, # Local brews
    (cons_unit == 'LITRE' & item_id == 1107) ~ 950,
    (cons_unit == 'LITRE' & item_id == 1104) ~ 1000, # Bottled/canned soft drinks
    (cons_unit == 'LITRE' & item_id == 1106) ~ 1000, # Bottled beer
    (cons_unit == 'LITRE' & item_id == 1105) ~ 1000, # prepared tea, coffee
    (cons_unit == 'PIECES' & item_id == 807) ~ 70, # eggs
    (cons_unit == 'PIECES' & item_id == 502) ~ 400, # coconuts
    (cons_unit == 'PIECES' & item_id == 804) ~ 200, # chicken
    (cons_unit == 'PIECES' & item_id == 302) ~ 50, # sweets
    (cons_unit == 'PIECES' & item_id == 702) ~ 100, # citrus fruits
    (cons_unit == 'PIECES' & item_id == 103) ~ 350, # maize (green, cob)
    (cons_unit == 'PIECES' & item_id == 703) ~ 175, # mangoes, avocado, etc.
    (cons_unit == 'PIECES' & item_id == 701) ~ 115, # ripe banana
    (cons_unit == 'PIECES' & item_id == 805) ~ 50, # wild bird and insects
    (cons_unit == 'KILOGRAMS') ~ 1000, # any food items used Kilograms
    TRUE ~ 1)) # any food items used GRAMS

view(dfSummary(dataJ1))

# calculate the reported food amount in grams (cons_g)

dataJ1 <- dataJ1 %>%
  mutate(cons_g = cons_quant*unit_conv)

view(dfSummary(dataJ1))

#check the output

df_cons_g <- dataJ1 %>% 
  select(item_id, itemcode, cons_yn, cons_unit, cons_quant, unit_conv, cons_g)

View(df_cons_g)

# the edible portion factor was calculated using the foods in the list: https://github.com/dzvoti/TFNC-Training/blob/main/Week-10/Materials/data/tz_food-matches_v.1.0.0.csv

# merge edible portion function in TNPS_EP.csv (see: https://github.com/dzvoti/TFNC-Training/blob/main/Week-10/Materials/TNPS_foodmatching_EPlist.csv)

df_EP <- read_csv("TNPS_EP.csv")

View(df_EP)

#omit itemcode
df_EP <- df_EP %>% 
  select(-itemcode)

#merge edible portion by 60 food items

dataJ1 <- left_join(dataJ1, df_EP, by = 'item_id')

View(dataJ1)
view(dfSummary(dataJ1))

#calculate estimate amount of food consumed in the HH (cons_g_hh)

dataJ1 <- dataJ1 %>% 
  mutate(cons_g_hh = cons_g*mean_EP)
View(dataJ1)
view(dfSummary(dataJ1))

# save dataJ1 (write_csv which does not create extra column (X with serial numbers) automatically)

write_csv(dataJ1, 'dataJ1.csv')

