library(tidyverse)

# Generate a random HCES dataset

# set the number of households
n <- 100

# create an empty data frame
df <- tibble()
food_entries <- tibble()

# create a character vector with the provinces of Zimbabwe
provinces <-
  c(
    "Bulawayo",
    "Harare",
    "Manicaland",
    "Mashonaland Central",
    "Mashonaland East",
    "Mashonaland West",
    "Masvingo",
    "Matabeleland North",
    "Matabeleland South",
    "Midlands"
  )

residence_types <- c("Rural", "Urban")

food_groups <-
  read.csv("./Week 0/MWI_food_list.csv", header = TRUE) %>% 
  select(code, Category)

# HHID generator character range
char_pool <- c(0:9, letters, LETTERS)

# generate a random data frame with HHID, province, and residence type
for (i in 1:n) {
  df_row <-
    tibble(
      hhid = (paste0(
        paste(sample(char_pool, 4, replace = TRUE), collapse = ""),
        "-",
        paste(sample(char_pool, 4, replace = TRUE), collapse = ""),
        "-",
        paste(sample(char_pool, 4, replace = TRUE), collapse = ""),
        "-",
        paste(sample(char_pool, 4, replace = TRUE), collapse = "")
      )),
      province = sample(provinces, 1),
      residence_type = sample(residence_types, 1)
    )
  df <- rbind(df_row, df)
}




# Generate a food entry list
for (hh in df$hhid) {
  for (food_code in food_groups$code) {
    food_entry_row <- tibble(
      hhid = hh,
      code = food_code,
      consumed = sample(c("yes", "no"), 1),
      source = ifelse(consumed == "yes", (sample(
        c("gifted", "purchased", "ownproduced"), 1
      )), "")
    )
    food_entries <- rbind(food_entries, food_entry_row)
  }
}

food_entries %>% 
  left_join(., food_groups) %>% 
  left_join(., df) %>% 
  filter(consumed == "yes") %>%
  write_csv("./Week 0/demo_hces_survey_data.csv", progress = show_progress())
  