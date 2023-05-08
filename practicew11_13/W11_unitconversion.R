# Week 11 TFNC training RG
# UNIT CONVERSION in GRAMS

## 1) PREPARATION

#start from a scratch
library(tidyverse)
install.packages("here")
library(here)

dataJ1 <- read_csv("HH_SEC_J1.csv")

#from your RStudio after 'pulling' from GitHub.
dataJ1 <- read_csv(here("hces-data", "HH_SEC_J1.csv"))

install.packages("summarytools")
library(summarytools)
view(dfSummary(dataJ1))

# change variable names

dataJ1 <- dataJ1 %>%
  rename(
    cons_yn = hh_j01,
    cons_unit = hh_j02_1,
    cons_quant = hh_j02_2,
    pur_unit = hh_j03_1,
    pur_quant = hh_j03_2,
    pur_THS = hh_j04,
    prod_unit = hh_j05_1,
    prod_quant = hh_j05_2,
    gift_unit = hh_j06_1,
    gift_quant = hh_j06_2
  )

# Change NONE to NA - na_if

dataJ1 <- dataJ1 %>%
  mutate(
    pur_quant = na_if(pur_quant, "NONE"),
    prod_quant = na_if(prod_quant, "NONE"),
    gift_quant = na_if(gift_quant, "NONE"),
  )

# change variable types

dataJ1 <- dataJ1 %>% 
  mutate(
    itemcode = as.factor(itemcode),
    cons_yn = as.factor(cons_yn),
    cons_unit = as.factor(cons_unit),
    cons_quant = as.numeric(cons_quant),
    pur_unit = as.factor(pur_quant),
    pur_quant = as.numeric(pur_quant),
    pur_THS = as.numeric(pur_THS),
    prod_unit = as.factor(prod_unit),
    prod_quant = as.numeric(prod_quant),
    gift_unit = as.factor(gift_unit),
    gift_quant = as.numeric(gift_quant)
  )

view(dfSummary(dataJ1))
str(dataJ1)

# add item_id for 60 food items

dataJ1 <- dataJ1 %>%
  mutate(item_id = case_when(
    (itemcode == 'RICE (PADDY)') ~ 101,
    (itemcode == 'RICE (HUSKED)') ~ 	102	,
    (itemcode == 'MAIZE (GREEN, COB)') ~ 	103	,
    (itemcode == 'MAIZE (GRAIN)') ~ 	104	,
    (itemcode == 'MAIZE (FLOUR)') ~ 	105	,
    (itemcode == 'MILLET AND SORGHUM (GRAIN)') ~ 	106	,
    (itemcode == 'MILLET AND SORGHUM (FLOUR)') ~ 	107	,
    (itemcode == 'BREAD') ~ 	109	,
    (itemcode == 'BUNS, CAKES AND BISCUITS') ~ 	110	,
    (itemcode == 'MACARONI, SPAGHETTI') ~ 	111	,
    (itemcode == 'OTHER CEREAL PRODUCTS') ~ 	112	,
    (itemcode == 'CASSAVA FRESH') ~ 	201	,
    (itemcode == 'CASSAVA DRY/FLOUR') ~ 	202	,
    (itemcode == 'SWEET POTATOES') ~ 	203	,
    (itemcode == 'YAMS/COCOYAMS') ~ 	204	,
    (itemcode == 'IRISH POTATOES') ~ 	205	,
    (itemcode == 'COOKING BANANAS, PLANTAINS') ~ 	206	,
    (itemcode == 'OTHER STARCHES') ~ 	207	,
    (itemcode == 'SUGAR') ~ 	301	,
    (itemcode == 'SWEETS') ~ 	302	,
    (itemcode == 'HONEY, SYRUPS, JAMS, MARMALADE, JELLIES, CANNED FRUITS') ~ 	303	,
    (itemcode == 'PEAS, BEANS, LENTILS AND OTHER PULSES') ~ 	401	,
    (itemcode == 'GROUNDNUTS IN SHELL/SHELLED') ~ 	501	,
    (itemcode == 'COCONUTS (MATURE/IMMATURE)') ~ 	502	,
    (itemcode == 'CASHEW, ALMONDS AND OTHER NUTS') ~ 	503	,
    (itemcode == 'SEEDS AND PRODUCTS FROM NUTS/SEEDS (EXCL. COOKING OIL)') ~ 	504	,
    (itemcode == 'ONIONS, TOMATOES, CARROTS AND GREEN PEPPER, OTHER VIUNGO') ~ 	601	,
    (itemcode == 'SPINACH, CABBAGE AND OTHER GREEN VEGETABLES') ~ 	602	,
    (itemcode == 'CANNED, DRIED AND WILD VEGETABLES') ~ 	603	,
    (itemcode == 'RIPE BANANAS') ~ 	701	,
    (itemcode == 'CITRUS FRUITS (ORANGES, LEMON, TANGERINES, ETC.)') ~ 	702	,
    (itemcode == 'MANGOES, AVOCADOES AND OTHER FRUITS') ~ 	703	,
    (itemcode == 'SUGARCANE') ~ 	704	,
    (itemcode == 'GOAT MEAT') ~ 	801	,
    (itemcode == 'BEEF INCLUDING MINCED SAUSAGE') ~ 	802	,
    (itemcode == 'PORK INCLUDING SAUSAGES AND BACON') ~ 	803	,
    (itemcode == 'CHICKEN AND OTHER POULTRY') ~ 	804	,
    (itemcode == 'WILD BIRDS AND INSECTS') ~ 	805	,
    (itemcode == 'OTHER DOMESTIC/WILD MEAT PRODUCTS') ~ 	806	,
    (itemcode == 'EGGS') ~ 	807	,
    (itemcode == 'FRESH FISH AND SEAFOOD (INCLUDING DAGAA)') ~ 	808	,
    (itemcode == 'DRIED/SALTED/CANNED FISH AND SEAFOOD (INCL. DAGAA)') ~ 	809	,
    (itemcode == 'PACKAGE FISH') ~ 	810	,
    (itemcode == 'FRESH MILK') ~ 	901	,
    (itemcode == 'MILK PRODUCTS (LIKE CREAM, CHEESE, YOGHURT ETC)') ~ 	902	,
    (itemcode == 'CANNED MILK/MILK POWDER') ~ 	903	,
    (itemcode == 'COOKING OIL') ~ 	1001	,
    (itemcode == 'BUTTER, MARGARINE, GHEE AND OTHER FAT PRODUCTS') ~ 	1002	,
    (itemcode == 'SALT') ~ 	1003	,
    (itemcode == 'OTHER SPICES') ~ 	1004	,
    (itemcode == 'WHEAT FLOUR') ~ 	1081	,
    (itemcode == 'WHEAT, BARLEY GRAIN AND OTHER CEREALS') ~ 	1082	,
    (itemcode == 'TEA DRY') ~ 	1101	,
    (itemcode == 'COFFEE AND COCOA') ~ 	1102	,
    (itemcode == 'OTHER RAW MATERIALS FOR DRINKS') ~ 	1103	,
    (itemcode == 'BOTTLED/CANNED SOFT DRINKS (SODA, JUICE, WATER)') ~ 	1104	,
    (itemcode == 'PREPARED TEA, COFFEE') ~ 	1105	,
    (itemcode == 'BOTTLED BEER') ~ 	1106	,
    (itemcode == 'LOCAL BREWS') ~ 	1107	,
    (itemcode == 'WINE AND SPIRITS') ~ 	1108	,
  ))

dataJ1 <- relocate(dataJ1, item_id, .before = itemcode)


# 2) Unit conversion

#check unit - select the food items using PIECES, or Litre and Millilitre - need to discuss how to calculated them.

count(dataJ1, itemcode, cons_unit)

food_unit_P <-
  count(dataJ1, item_id, itemcode, cons_unit) %>%
  filter(cons_unit == "PIECES") %>%
  arrange(desc(n))

food_unit_L <-
  count(dataJ1, item_id, itemcode, cons_unit) %>%
  filter(cons_unit == "LITRE") %>% 
  arrange(desc(n))

food_unit_mL <-
  count(dataJ1, item_id, itemcode, cons_unit) %>%
  filter(cons_unit == "MILLILITRE") %>% 
  arrange(desc(n))

food_unit_LmL <-
  count(dataJ1, item_id, itemcode, cons_unit) %>%
  filter(cons_unit == "LITRE" | cons_unit == "MILLILITRE") %>% 
  arrange(desc(n))


# create a variable of conversation factor (unit_conv)

dataJ1 <- dataJ1 %>%
  mutate(unit_conv = case_when(
    (cons_unit == 'MILLILITRE' & item_id == 1001) ~ 0.91, # cooking oil
    (cons_unit == 'LITRE' & item_id == 1001) ~ 910,
    (cons_unit == 'MILLILITRE' & item_id == 901) ~ 1.06, # Fresh milk
    (cons_unit == 'LITRE' & item_id == 901) ~ 1060,
    (cons_unit == 'MILLILITRE' & item_id == 902) ~ 0.83, # Milk products
    (cons_unit == 'LITRE' & item_id == 902) ~ 830,
    (cons_unit == 'MILLILITRE' & item_id == 303) ~ 1.32, # Honey, syrups, etc.
    (cons_unit == 'LITRE' & item_id == 303) ~ 1320,     
    (cons_unit == 'MILLILITRE' & item_id == 1107) ~ 1.09, # Local brews
    (cons_unit == 'LITRE' & item_id == 1107) ~ 1090,
    (cons_unit == 'MILLILITRE' & item_id == 1104) ~ 1.04, # Bottled/canned soft drinks
    (cons_unit == 'LITRE' & item_id == 1104) ~ 1040, # Bottled/canned soft drinks
    (cons_unit == 'LITRE' & item_id == 1106) ~ 1000, # Bottled beer
    (cons_unit == 'LITRE' & item_id == 1105) ~ 1000, # prepared tea, coffee
    (cons_unit == 'PIECES' & item_id == 807) ~ 70, #eggs
    (cons_unit == 'PIECES' & item_id == 502) ~ 300, #coconuts
    (cons_unit == 'PIECES' & item_id == 804) ~ 100, #chicken
    (cons_unit == 'PIECES' & item_id == 302) ~ 50, #sweets
    (cons_unit == 'PIECES' & item_id == 702) ~ 85, #citrus fruits
    (cons_unit == 'PIECES' & item_id == 103) ~ 350, #maize (green, cob)
    (cons_unit == 'PIECES' & item_id == 703) ~ 170, #mangoes, avocado, etc.
    (cons_unit == 'PIECES' & item_id == 701) ~ 115, #ripe banana
    (cons_unit == 'PIECES' & item_id == 805) ~ 0, #wild bird and insects
    (cons_unit == 'KILOGRAMS') ~ 1000,
    TRUE ~ 1))

summary(dataJ1)
str(dataJ1)


# get in grams! (cons_g)

dataJ1 <- dataJ1 %>%
  mutate(cons_g = cons_quant*unit_conv)

view(dfSummary(dataJ1))

#check the output

df_cons_g <- dataJ1 %>% 
  select(item_id, itemcode, cons_unit, cons_quant, unit_conv, cons_g)

view(dfSummary(df_cons_g))


# get edible portion
# organise edible function in TNPS_EP.csv

df_EP <- read_csv("TNPS_EP.csv")

#omit itemcode
df_EP <- df_EP %>% 
  select(-itemcode)

#merge edible portion by 60 food items

dataJ1 <- left_join(dataJ1, df_EP, by = 'item_id')
View(dataJ1)
view(dfSummary(dataJ1))

# now, dataJ1 file contains 1) amount of foods consumed in grams and 2) factors of edible portion (mean_EP)

#calculate estimate amount of food consumed in the HH (cons_g_hh)

dataJ1 <- dataJ1 %>% 
  mutate(cons_g_hh = cons_g*mean_EP)
View(dataJ1)
view(dfSummary(dataJ1))



