# Week 6

# Install tidyverse is only done once
install.packages("tidyverse")

# View installed
View(installed.packages())

# This is done everytime you open R so that you have the packages available to you.
library(tidyverse)


# Importing data
hces_data <- read_csv("./Week-4/demo_hces_survey_data_v2.csv",col_names = FALSE)

# Explore
str(hces_data)

head(hces_data,20)

summary(hces_data)

# Pipes
## |> "and then"

## |> Base R pipe
## %>% Tidyverse Magrittr pipe 

hces_subset <- hces_data |>  head(20)
 
hces_data |> head(1000) |> summary()
  
# Cleaning column names
# rename - renames columns
# mutate - changes columns
hces_subset_columns <- hces_data |> 
  rename(food_source = gifted,province = Masvingo) |> 
  mutate(food_source = as.factor(food_source),province = as.factor(province)) |> 


# Subsetting rows and columns
hces_subset_columns <- hces_subset_columns |> 
  #subsetting columns and remaining with food_source and provinces
  select(food_source,province)  |> 
  # Subset rows and remain with gifted
  filter(food_source == "gifted" ) |> summary()

# Class exercise

