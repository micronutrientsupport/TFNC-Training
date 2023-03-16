
# Loading libraries
library(tidyverse)



# Loading the data
df <- read.csv(here::here("Week-9", "Materials", "data", "TZ08_FCT_Partial.csv"))

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

df <- df %>% slice(c(1:378)) #This selects only the rows 1:10 to keep. 
#We can do a number of selections like this; e.g. slice(c(1:4, 6:10)) 
#would keep rows 1 to 4 and 6 to 10, but miss out 5, 11, 12, 13, 14.


## 2) Reorder/select the columns to be kept

df <- df %>% select(food.ID, 
                    food_description, 
                    Food.group.descriptor, 
                    energy..kcal., 
                    protein..mg., 
                    Fat..in.g., 
                    Fat..total.saturated..g., 
                    Total.Carbohydrate.by.difference, 
                    Fibre..in.grams, 
                    Phytic.acid..mcg.,
                    Calcium..mg.,
                    Copper..Cu..mg,
                    Manganese..MNmg.,
                    Magnesium..Mg...mg,
                    Potassium..K...mg.,
                    Sodium..Na..mg,
                    Iron.mcg,
                    Zinc.mg,
                    Phosphorus..mg.)


## Checking the column order
colnames(df)

## 3) Changing variable names

df <- df %>% rename(food_ID = food.ID, 
                    Food_Name = food_description, 
                    Food_Group = Food.group.descriptor, 
                    ENERCkcal = energy..kcal., 
                    PROCNTmg = protein..mg., 
                    FAT_g = Fat..in.g., 
                    FASATg = Fat..total.saturated..g., 
                    CHOCDFg = Total.Carbohydrate.by.difference, 
                    FIB_g = Fibre..in.grams, 
                    PHYTACmg = Phytic.acid..mcg.,
                    CAmg = Calcium..mg.,
                    CUmg = Copper..Cu..mg,
                    MNmg = Manganese..MNmg.,
                    MGmg = Magnesium..Mg...mg,
                    Kmg = Potassium..K...mg.,
                    NAmg = Sodium..Na..mg,
                    FEmcg = Iron.mcg,
                    ZNmg = Zinc.mg,
                    Pmg = Phosphorus..mg.
)

## What are the variables names now?

names(df)
colnames(df)

## 4) Recalculate columns

df <- df %>% mutate(PROCNTg = PROCNTmg/1000)
df <- df %>% mutate(FEmg = FEmcg/1000)

#Then we need to remove the incorrect column
df <- df %>% select(-c(PROCNTmg, FEmcg))


#bonus point - rearrange the protein column to the macronutrient section]
df <- df %>% relocate(PROCNTg, .after = ENERCkcal)

write.csv(df, here::here("Week-9", "Materials", "data", 
                         "TZ08_FCT_Partial_Cleaned.csv"))
