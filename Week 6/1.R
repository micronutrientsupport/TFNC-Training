install.packages("tidyverse")
View(installed.packages())
library(tidyverse)
hces_data<-read_csv("./Week-4/demo_hces_survey_data_v2.csv")
head(hces_data)
str(hces_data)
summary(hces_data)
head(hces_data,20)
str(hces_data,20)
summary(hces_data,20)
hces_data |> summary()
hces_data |> head()
hces_data |> str()
hces_data |> summary(,20)
hces_data |> head(,20)
hces_data |> str(,20)
hces_data |> head(1000) |> summary()
hces_subset_columns <- hces_data |> 
  rename(food_source = gifted) |> 
  mutate(food_source = as.factor(food_source)) |> 
  rename(province = Masvingo) |> 
  mutate(province = as.factor(province))
?read_csv
?rename
?as.factor

## 28-02-2023
library(tidyverse)
data <- read_csv("hces-data/4.  hces_data_csv/hh_sec_j1.csv")
