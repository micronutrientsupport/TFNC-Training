# Load required libraries
library(tidyverse)

# Load the HCES data
cons_data <- read_csv("hces-data/HH_SEC_J1.csv")

# Check names of the data
names(cons_data)

# Select required columns
# cons_data <- select(cons_data,sdd_hhid,itemcode,hh_j01)

# Check the names again
names(cons_data)

# Visualise the data
View(cons_data)

# Filter the consumption data
cons_data <- filter(cons_data,hh_j01 == "YES")

# Visualise the data again
View(cons_data)

# Rename the data variables to familiar names
cons_data <-rename(cons_data,
                   HH = sdd_hhid,
                   food_item = itemcode,
                   consYN = hh_j01,
                   total_cons_unit = hh_j02_1,
                   total_cons_qnty = hh_j02_2,
                   purc_unit = hh_j03_1,
                   purc_qnty = hh_j03_2,
                   cost_TSH = hh_j04,
                   ownProd_unit = hh_j05_1,
                   ownProd_qnty = hh_j05_2,
                   giftOth_unit = hh_j06_1,
                   giftOth_qnty = hh_j06_2)


# Check the consumption units in the data
unique(cons_data$total_cons_unit)

# Change KGs to Grams
df_2 <- mutate(cons_data,kg_to_gram =total_cons_qnty*1000) # Produces wrong result

df_3 <- mutate(cons_data,kg_to_gram = case_when(
  total_cons_unit == "KILOGRAMS" ~ total_cons_qnty*1000,
  TRUE ~ total_cons_qnty))
View(df_3)
