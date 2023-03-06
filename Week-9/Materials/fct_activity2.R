

# Activity 2: Let's recap from last training.

# 1.1. Loading libraries and importing the data


# Loading library


# Loading the data


# check the data


### 1.2. Cleaning the data

# change variable names


# Check variable names


# Checking consumption variables


# Changing NONE to NA

# Changing variable types

### 1.3 Exploring the data



# Counting frequency of food consumed in the HH


# Filtering only consumed


# Filtering only consumed foods, and ordering from by most to least consumed


# Creating a dataframe with: food items, count of HH consuming each food item, 
#and the percentage of HH consuming each foods. 

n_HH <- length(unique(dataJ1$sdd_hhid))



# check the data


# Let's have a look at the FCT...

# 2.1 Loading the data

tzfct <- read.csv(here::here( "data", "TZ08_tidied_FCT_v2"))

# check the data

head(tzfct)
names(tzfct)
str(tzfct)
dim(tzfct)

# 2.2 Exploring FCT for food matching

# Filtering of FCT, for food groups and food items.

# Checking food categories
tzfct %>% filter(foodcategory == "Cereals and others") %>% 
  select(fcd_id, fooditem, EDIBLE, WATERg, VITA_RAEmcg )

# Exploring composition of the foods
tzfct %>% filter(fooditem == "Cereals and others") %>% 
  select(fcd_id, fooditem, EDIBLE, WATERg, VITA_RAEmcg )

# Finding food items
tzfct %>% filter(grepl("tomato", fooditem, case.ignore = TRUE))

# Exploring composition of filtered foods
tzfct %>% filter(grepl("tomato", fooditem, case.ignore = TRUE)) %>% 
  select(fcd_id, fooditem, EDIBLE, WATERg, VITA_RAEmcg )



# Thinking about food matches between HCES food list and food composition.

