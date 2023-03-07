

# Loading libraries
library(tidyverse)


# Loading the data
df <- read.csv("Week-9/Materials/data/TZ08_FCT_Partial.csv")

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




## 2) Reorder/select the columns to be kept. For this task we aren't interested in the Vitamins; please do not include the Vitamin A's, Vitamin D, Vitamin E.
#Please order the columns so the metadata comes first, followed by macronutrients, then micronutrients.



## 3) Changing variable names. Please select logical names for the columns, and keep the format regular across the entire dataset - e.g. [Variable]_[unit] (NA_mg)



## What are the variables names now?



## 4) Recalculate columns - Please convert the Iron column from mcg to mg, and the Protein column from mg to g.



## 5) Saving the data

