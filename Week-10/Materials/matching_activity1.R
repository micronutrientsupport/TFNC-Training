
# Loading library
library(tidyverse)

# 1.1 Loading the apparent food consumption data 

dataJ1 <- read.csv(here::here("..", "tz_2020", "data", "HH_SEC_J1.csv"))
item_code <- read.csv(here::here("Week-10", "Materials",  "data", "food-id.csv"), header = TRUE)

# check the data
head(dataJ1)
names(dataJ1)
str(dataJ1)
dim(dataJ1)

head(item_code)
names(item_code)

# 1.2  Generating the food list of food consumed by the HH in Tanzania

(food_consumed <-  dataJ1 %>% 
    rename(cons_yn = hh_j01) %>%  # renaming variable
    mutate(                                # changing variable class
      itemcode = as.factor(itemcode),
      cons_yn = as.factor(cons_yn)) %>% 
    filter(cons_yn == "YES") %>%     # filtering food reported as consumed 
    count(itemcode) %>%           # counting number of HH consuming a food
    arrange(desc(n))) # sorting foods by most HH reporting consuming that food

# 1.3 Adding the food id to the food consumed in Tanzania 

(food_consumed <- food_consumed %>% left_join(., item_code) %>%  #Merging food ids. to the food consumed (food list)
    relocate(item_id, .before = itemcode)) # Changing the order of the columns

# 2.1 Loading the food composition data

tzfct <- read.csv(here::here("..",  "data", "TZ08_tidied_FCT_v2.CSV"))

# check the data

head(tzfct)
names(tzfct)
str(tzfct)
dim(tzfct)

# 2.2 Evaluating the Tanzania FCT, 2008 

# Number of food items in TZ FCT

length(tzfct$food_description)

# Number of food items per food group in TZ FCT

tzfct %>% group_by(Food_Group) %>%  # grouping food items by food groups
  count() %>%               # counting no. of food items per food group
  arrange(desc(n))         # sorting by most to least reported foods

# Checking food categories
tzfct %>% filter(Food_Group == "Cereal and Cereal products") 

# Checking Food Components and units

names(tzfct)

# What important food components are missing? 

tzfct %>% 
  select(fdc_id, food_description, EDIBLE, WATERg, VITA_RAEmcg)

# Checking missing values for all variables

tzfct %>% naniar::vis_miss()

# Can we visualise it only for vitamins?

# Checking missing values for all variables by food groups

tzfct %>% naniar::gg_miss_fct(., Food_Group)

# Exploring composition of the foods groups

tzfct %>% filter(Food_Group == "Cereal and Cereal products") %>% 
  select(fdc_id, food_description, ENERCkcal, VITA_RAEmcg)

# what components are needed to calculate Energy?
tzfct %>% 
  select(fdc_id, food_description, )


# 2.3. Exploring FCT for food matching

# Let's have a look at the food list...

View(food_consumed)

# Filtering of FCT, for food groups and food items.

# Finding food items: salt, rice, tomato
tzfct %>% filter(grepl("tomato", food_description, ignore.case = TRUE))

# Exploring composition of filtered foods: salt, rice, tomato
tzfct %>% filter(grepl("tomato", food_description, ignore.case = TRUE)) %>% 
  select(fdc_id, food_description,  VITA_RAEmcg )

# What nutrients are important for each of the foods selected? 

# What do you think about the Vitamin A content of green tomato vs red tomato?

# Activity: Thinking about food matches between HCES food list and food composition.

matches <- read.csv(here::here("Week-10", "Materials",  "data", "tz_food-matches_v.1.0.0.csv"))

View(matches)

# 1. Do you think that the food matches selected represents well what it is consumed in Tanzania? 
# 2. If not, what other foods would you add/remove/change to? 
# 3. For the foods without matches, can you think of what food items you would use?
# 4. Could you identify any food matches in the Tanzania FCT, 2008 that will be a good match for those foods that have no matches? 


# 2.4. Introduction to Quality Assessment of the FCT 

# Calculating Vitamin A (RAE) mg - It's not possible!

# Calculating Energy in Kcal

# What components are needed: Protein, Fat, Carbohydrates, Fibre, and Alcohol (if alcoholic beverage)

# Checking the components: !ALCg !CHOAVLDF

tzfct %>% select(PROCNTg, FAT_g, CHOAVLDFg, FIB_g, ALCg)

tzfct %>% select(PROCNTg, FAT_g, CHOCDFg, FIB_g)


# Ideally we would re-calculate CHOCDF and/or CHOAVLDFg but it's not possible. Why?


# Calculating CHOAVLDFg_standardised from CHOCDFg

# 1. Setting variables as.numeric 

tzfct  %>% 
    mutate(across(c(PROCNTg, FAT_g, CHOCDFg, FIB_g), as.numeric))

# There is a problem, let's check the values
test <- tzfct  %>% 
  mutate(across(c(PROCNTg, FAT_g, CHOCDFg, FIB_g), as.numeric))

test %>% select(PROCNTg, FAT_g, CHOCDFg, FIB_g) %>%  naniar::vis_miss()

# Identify the row where the values were coerced into NA in FAT_g
which(is.na(test$FAT_g))

# Identify the values that are coerced into NA in FAT_g
tzfct[322, "FAT_g"]
# Fixing the values that are coerced into NA in FAT_g
tzfct[322, "FAT_g"] <- "7.8"

# Identify the row where the values were coerced into NA in CHOCDFg
which(is.na(test$CHOCDFg))

# Identify the values that are coerced into NA in CHOCDFg
tzfct[181, "CHOCDFg"] 

# Fixing the values that are coerced into NA in CHOCDFg
tzfct[181, "CHOCDFg"] <- "1.9"

# 1. Setting variables as.numeric 
(tzfct <- tzfct  %>% 
    mutate(across(c(PROCNTg, FAT_g, CHOCDFg, FIB_g), as.numeric)))

# 2. Calculating CHOAVLDFg from CHOCDFg & FIB_g
(tzfct <- tzfct %>% mutate(
  CHOAVLDFg_standardised = CHOCDFg - FIB_g))

# 3. Calculating Energy in kcal
tzfct %>% mutate(
  ENERCkcal_standardised = ENERCKj_standardised(PROCNTg, FAT_g, CHOAVLDFg_standardised, FIB_g, ALCg=0)
)
