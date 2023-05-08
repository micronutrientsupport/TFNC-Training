# Week 11 TFNC training - practice material
# Write the scripts by yourself, filling XXXXX and use examples

# UNIT CONVERSION in GRAMS

## 1) PREPARATION

# Ready for 'tidyverse' and 'here' function
XXXXXXX(tidyverse)
XXXXXXX(here)

# Open HH_SEC_J1.csv (located in 'hces-data' folder) as 'dataJ1'
# from your RStudio after 'pulling' from GitHub.

dataJ1 <- XXXXXXXX(here("hces-data", "HH_SEC_J1.csv"))

# Download new package - summarytools
install.packages("summarytools")
library(summarytools)

# Use dfSummary - result in Console (but not easy to look)
dfSummary(dataJ1)

# Show better output - read the output using web browser (see Viewer and click a box with arrow at the left corner)
view(dfSummary(dataJ1))


# Change variable names - which function in XXXXXX?

dataJ1 <- dataJ1 %>%
  XXXXXX(
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

# There are NONE in pur_quant, prod_quant and gift_quant. Change NONE to NA

dataJ1 <- dataJ1 %>%
  XXXXXX(
    pur_quant = XXXXX(pur_quant, "NONE"),
    prod_quant = XXXXX(prod_quant, "NONE"),
    gift_quant = XXXXX(gift_quant, "NONE"),
  )

# Change variable types
# Try here to change itemcode as factor
dataJ1 <- dataJ1 %>% 
  XXXXXX(itemcode = XXXXXXXX(itemcode))

# then repeat for all variables

















# answers are here

dataJ1 <- dataJ1 %>% 
  mutate(
    itemcode = as.factor(itemcode),
    cons_yn = as.factor(cons_yn),
    cons_unit = as.factor(cons_unit),
    cons_quant = as.numeric(cons_quant),
    pur_unit = as.factor(pur_quant),
    pur_quant = as.numeric(pur_quant),
    pur_TSH = as.numeric(pur_TSH),
    prod_unit = as.factor(prod_unit),
    prod_quant = as.numeric(prod_quant),
    gift_unit = as.factor(gift_unit),
    gift_quant = as.numeric(gift_quant)
  )

# check your work - see structure
XXX(dataJ1)


# add food ID in dataJ1 - merge item_id in a prepared file 'food-id.csv'
# open food-id.csv as foodid
foodid <- read_csv(here("Week-10", "Materials", "data", "food-id.csv"))

# merge item_id in foodid with dataJ1, and locate item_id before itemcode
dataJ1 <- left_join(dataJ1, foodid, by = 'itemcode') %>% 
  relocate(item_id, .before = itemcode)

# check your data
View(dataJ1)

# OR if there is no prepared file - use mutate + case_when
# example - itemcode=RICE(PADDY) -> create item_id=101
dataJ1 <- dataJ1 %>%
  mutate(item_id = case_when(
    (itemcode == 'RICE (PADDY)') ~ 101)
    
# using the example, write script for changes in all variables
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
# answer is here
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
    
# move 'item_id' before 'itemcode'
dataJ1 <- relocate(dataJ1, item_id, .before = itemcode)
    

    
    
# 2) UNIT CONVERSION INTO GRAMS
    
#check unit - select the food items using PIECES, or Litre and Millilitre - need to discuss how to calculated them.
    
countunit <- count(dataJ1, itemcode, cons_unit)
    
# see the list - new package 'knitr'
install.packages("knitr")
library(knitr)
kable(countunit)
    
food_unit_P <-
  count(dataJ1, item_id, itemcode, cons_unit) %>%
  filter(cons_unit == "PIECES") %>%
  arrange(desc(n))
    
kable(food_unit_P)
    
# write script to pick up food items using LITRE and MILLILITRE
# hint - use | as OR
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
# answer
food_unit_LmL <-
  count(dataJ1, item_id, itemcode, cons_unit) %>%
  filter(cons_unit == "LITRE" | cons_unit == "MILLILITRE") %>% 
  arrange(desc(n))

# Create a variable of unit conversation factor (unit_conv)
# e.g cooking oil (ML and L to GRAM) - conversion factor 0.91, item_id = 1001
# example - modify the script we used previously
dataJ1 <- dataJ1 %>%
  mutate(item_id = case_when(
        (itemcode == 'RICE (PADDY)') ~ 101))
    
    # Unit conversion factor
    # 1001 cooking oil - 0.91
    # 901 fresh milk - 1.03
    # 902 milk product - 0.81
    # 303 honey, syrups etc. - 1.32
    # 1107 local brews - 0.95
    # 1104 Bottled/canned soft drinks - 1.0 so x1000 in LITRE
    # 1106 Bottled beer - 1.0
    # 1105 prepared tea, coffee - 1.0
    # 807 eggs - 70g per PIECES
    # 502 coconuts - 400
    # 804 chicken - 200
    # 302 sweets - 50
    # 702 citrus fruits - 100
    # 103 maize (green, cob) - 350
    # 703 mangoes, avocado, etc.- 175
    # 701 ripe banana - 115
    # 805 wild bird and insects - 0 (cancelled)
    
    #For all food items using KILOGRAM - x1000
    (cons_unit == 'KILOGRAMS') ~ 1000
    # and others are all 1
    TRUE ~ 1
    
    



# Answer is here
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
    (cons_unit == 'PIECES' & item_id == 807) ~ 70, #eggs
    (cons_unit == 'PIECES' & item_id == 502) ~ 400, #coconuts
    (cons_unit == 'PIECES' & item_id == 804) ~ 200, #chicken
    (cons_unit == 'PIECES' & item_id == 302) ~ 50, #sweets
    (cons_unit == 'PIECES' & item_id == 702) ~ 100, #citrus fruits
    (cons_unit == 'PIECES' & item_id == 103) ~ 350, #maize (green, cob)
    (cons_unit == 'PIECES' & item_id == 703) ~ 175, #mangoes, avocado, etc.
    (cons_unit == 'PIECES' & item_id == 701) ~ 115, #ripe banana
    (cons_unit == 'PIECES' & item_id == 805) ~ 0, #wild bird and insects
    (cons_unit == 'KILOGRAMS') ~ 1000, # all food items used KILOGRAMS
    TRUE ~ 1)) # Others using GRAMS

# check your work
summary(dataJ1)
str(dataJ1)
view(dfSummary(dataJ1))

# get amount of food consumed in hh in grams! (cons_g = cons_quant x unit_conv)

dataJ1 <- dataJ1 %>%
  mutate(cons_g = XXXXXXXXXXXXX)

view(dfSummary(dataJ1))

#check the output -- select variables only needed

df_cons_g <- dataJ1 %>% 
  select(item_id, itemcode, cons_unit, cons_quant, unit_conv, cons_g)

view(dfSummary(df_cons_g))


# get edible portion
# organise edible function in TNPS_EP.csv

df_EP <- read_csv(here("Week-11", "TNPS_EP.csv")
                  
# omit itemcode - use 'minus sign' before itemcode
df_EP <- df_EP %>%
  select(-itemcode)
                  
# merge edible portion by 60 food items
dataJ1 <- XXXXXXXX(dataJ1, df_EP, XX = 'item_id')
View(dataJ1)
view(dfSummary(dataJ1))
                  
# now, dataJ1 file contains 1) amount of foods consumed in grams (cons_g), and 2) the factors of edible portion (mean_EP)
                  
# calculate estimate amount of food consumed in the HH (cons_g_hh)
dataJ1 <- dataJ1 %>% 
  XXXXXX(cons_g_hh = XXXXXXXXXXX)

# check your work
View(dataJ1)
view(dfSummary(dataJ1))
                  
                  
                  