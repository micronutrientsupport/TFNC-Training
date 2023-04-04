# TFNC training on 04/04/23 - practice
# UNIT CONVERSION in GRAMS

#1 TO start, we could like to work on tidyverse


#2 Open HH_SEC_J1.csv as dataJ1 - it is in the folder "hces-data"


#3 change variable names - which function should we use?

???? <- ???? %>%
  ????(
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

#4 There were NONE in pur_quant, prod_quant, gift_quant and need to change NONE to NA - which function should we use? 

dataJ1 <- dataJ1 %>%
  mutate(
    pur_quant = ????(pur_quant, "NONE"),
    prod_quant = ????(prod_quant, "NONE"),
    gift_quant = ????(gift_quant, "NONE"),
  )

#5 change variable types

dataJ1 <- dataJ1 %>%
  mutate(
    itemcode = ????(itemcode),
    cons_yn = ????(cons_yn),
    cons_unit = ????(cons_unit),
    cons_quant = ????(cons_quant),
    pur_unit = ????(pur_quant),
    pur_quant = ????(pur_quant),
    prod_unit = ????(prod_unit),
    prod_quant = ????(prod_quant),
    gift_unit = ????(gift_unit),
    gift_quant = ????(gift_quant)
  )

#6 create item_id for 60 food items

#case_when

dataJ1 <- dataJ1 %>%
  mutate(item_id = case_when(
    (itemcode == 'RICE (PADDY)') ~ 101,
  ))

101	RICE (PADDY)
102	RICE (HUSKED)
103	MAIZE (GREEN, COB)
104	MAIZE (GRAIN)
105	MAIZE (FLOUR)
106	MILLET AND SORGHUM (GRAIN)
107	MILLET AND SORGHUM (FLOUR)
109	BREAD
110	BUNS, CAKES AND BISCUITS
111	MACARONI, SPAGHETTI
112	OTHER CEREAL PRODUCTS
201	CASSAVA FRESH
202	CASSAVA DRY/FLOUR
203	SWEET POTATOES
204	YAMS/COCOYAMS
205	IRISH POTATOES
206	COOKING BANANAS, PLANTAINS
207	OTHER STARCHES
301	SUGAR
302	SWEETS
303	HONEY, SYRUPS, JAMS, MARMALADE, JELLIES, CANNED FRUITS
401	PEAS, BEANS, LENTILS AND OTHER PULSES
501	GROUNDNUTS IN SHELL/SHELLED
502	COCONUTS (MATURE/IMMATURE)
503	CASHEW, ALMONDS AND OTHER NUTS
504	SEEDS AND PRODUCTS FROM NUTS/SEEDS (EXCL. COOKING OIL)
601	ONIONS, TOMATOES, CARROTS AND GREEN PEPPER, OTHER VIUNGO
602	SPINACH, CABBAGE AND OTHER GREEN VEGETABLES
603	CANNED, DRIED AND WILD VEGETABLES
701	RIPE BANANAS
702	CITRUS FRUITS (ORANGES, LEMON, TANGERINES, ETC.)
703	MANGOES, AVOCADOES AND OTHER FRUITS
704	SUGARCANE
801	GOAT MEAT
802	BEEF INCLUDING MINCED SAUSAGE
803	PORK INCLUDING SAUSAGES AND BACON
804	CHICKEN AND OTHER POULTRY
805	WILD BIRDS AND INSECTS
806	OTHER DOMESTIC/WILD MEAT PRODUCTS
807	EGGS
808	FRESH FISH AND SEAFOOD (INCLUDING DAGAA)
809	DRIED/SALTED/CANNED FISH AND SEAFOOD (INCL. DAGAA)
810	PACKAGE FISH
901	FRESH MILK
902	MILK PRODUCTS (LIKE CREAM, CHEESE, YOGHURT ETC)
903	CANNED MILK/MILK POWDER
1001	COOKING OIL
1002	BUTTER, MARGARINE, GHEE AND OTHER FAT PRODUCTS
1003	SALT
1004	OTHER SPICES
1081	WHEAT FLOUR
1082	WHEAT, BARLEY GRAIN AND OTHER CEREALS
1101	TEA DRY
1102	COFFEE AND COCOA
1103	OTHER RAW MATERIALS FOR DRINKS
1104	BOTTLED/CANNED SOFT DRINKS (SODA, JUICE, WATER)
1105	PREPARED TEA, COFFEE
1106	BOTTLED BEER
1107	LOCAL BREWS
1108	WINE AND SPIRITS



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


#7 Unit conversion into gram - select the food items using PIECES, or Litre and Millilitre - need to discuss how to calculated them.

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


#create a variable for conversation factor

#Litre and Millilitre
1001  COOKING OIL
901  FRESH MILK
902  MILK PRODUCTS (LIKE CREAM, CHEESE, YOGHURT ETC)
1107  LOCAL BREWS
1104  BOTTLED/CANNED SOFT DRINKS (SODA, JUICE, WATER)
303  HONEY, SYRUPS, JAMS, MARMALADE, JELLIES, CANNED FRUITS
1106  BOTTLED BEER
1105  PREPARED TEA, COFFEE
1108  WINE AND SPIRITS

# Pieces conversion

807  EGGS
502  COCONUTS (MATURE/IMMATURE)
804  CHICKEN AND OTHER POULTRY
302  SWEETS
702  CITRUS FRUITS (ORANGES, LEMON, TANGERINES, ETC.)
103  MAIZE (GREEN, COB)
703  MANGOES, AVOCADOES AND OTHER FRUITS
701  RIPE BANANAS
805  WILD BIRDS AND INSECTS


dataJ1 <- dataJ1 %>%
  mutate(unit_conv = case_when(
    (cons_unit == 'KILOGRAMS') ~ 1000,
    (cons_unit == 'MILLILITRE' & item_id == 1001) ~ 0.90, #cooking oil
    (cons_unit == 'LITRE' & item_id == 1001) ~ 900,

    TRUE ~ 1))

# get in grams!

dataJ1 <- dataJ1 %>%
  ????(cons_g = ????*????)

str(dataJ1)