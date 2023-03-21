

# Welcome to Week 9! 


# Loading libraries
library(tidyverse)

# Loading the data
df <-read.csv("Week-9/Materials/data/TZ08_FCT_Partial.csv")

# Checking the loaded data

## How many rows has the data?
nrow(df)

## How many columns has the data?
ncol(df)
## What are the variables names? 
names(df)
# Tidying the data

## 0) Visually checking the data
head(df)


## 1) Trimming the dataframe vertically
slice(c(1:378))
df <- df %>% slice(c(1:378))

## 2) Reorder/select the columns to be kept. For this task we aren't interested in the Vitamins; please do not include the Vitamin A's, Vitamin D, Vitamin E.
#Please order the columns so the metadata comes first, followed by macronutrients, then micronutrients.
names(df)


## 3) Changing variable names. Please rename the variable to the FAO INFOODS Tagnames - fdc_id, PROCNTg, FAT_g, FASATg, CHOCDFg, food_description, FIB_g, PHYTACmg,
# CAmg, CUmg, MNmg, MGmg, Food_Group, Kmg, ENERCkcal, NAmg, FEmg, ZNmg, Pmg



## What are the variables names now?



## 4) Recalculate columns - Please convert the Iron column from mcg to mg, and the Protein column from mg to g.



## 5) Saving the data

