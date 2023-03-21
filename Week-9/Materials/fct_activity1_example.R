
# Loading libraries
library(tidyverse)

# Loading the data
df <- read.csv("data/example_data.csv")

# Checking the loaded data
## How many rows has the data?

nrow(df)

message(paste0("This dataset has ", nrow(df), " rows of data"))

## How many columns has the data?

length(df)

ncol(df)

message(paste0("This dataset has ", ncol(df), " rows of data"))


## What are the variables names? 

names(df)

colnames(df)

# Tidying the data

## Visually checking the data
head(df)
tail(df)
View(df)

## 1) Trimming the dataframe vertically

df <- df %>% slice(c(1:10)) #This selects only the rows 1:10 to keep. 
#We can do a number of selections like this; e.g. slice(c(1:4, 6:10)) 
#would keep rows 1 to 4 and 6 to 10, but miss out 5, 11, 12, 13, 14.


## 2) Reorder/select the columns to be kept

df <- df %>% select(ID, comments, f_name, Energy.in.KCalories, Protein.in.grams, Retinol..mcg., Magnesium..Mg..in.mg, Iron..Fe..in.mg, Iodine..I..in.mg, NA..Sodium..in.mg, Calcium.in.mcg)


## Checking the column order
colnames(df)

## 3) Changing variable names

df <- df %>% rename(
  fcd_id = ID,
  food_name = f_name,
  ENERCKcal = Energy.in.KCalories,
  PROTg = Protein.in.grams,
  CAmcg = Calcium.in.mcg,
  NAmg = NA..Sodium..in.mg,
  Img = Iodine..I..in.mg,
  FEmg = Iron..Fe..in.mg,
  MGmg = Magnesium..Mg..in.mg,
  RETOLmcg = Retinol..mcg.
)

## What are the variables names now?

names(df)
colnames(df)

## 4) Recalculate columns

df <- df %>% mutate(CAmg = CAmcg/1000)
#df$CAmg <- df$CAmg/1000

#Then we need to remove the incorrect column
df <- df %>% select(-CAmcg)

write.csv(df, "data/cleaned_example_dataset.csv")
