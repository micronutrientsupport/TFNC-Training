# Load necessary libraries
# Install the 'writexl' package if it is not already installed
# install.packages("writexl")

library(tidyverse) # for data manipulation and visualization
library(readxl) # for reading Excel files
library(writexl) # for writing Excel files

# Read the Excel file and skip the first row
sr <- read_excel("Week-15/Student Results.xlsx")

# View the data in a new window
View(sr)

# Rename the columns for easier reference
sr <- rename(sr,StudentName = Name,
             Mark= "TOTAL (100)",
             UE = "UE (60)",
             CW = "CW (40)")

# Create a new column 'Grade' based on the 'Mark' column
sr <- mutate(sr,Grade=case_when(
  Mark>=70 ~ "A", # if Mark is greater than or equal to 70, assign 'A'
  Mark>=60 & Mark<70 ~ "B+", # if Mark is between 60 and 70 (exclusive), assign 'B+'
  Mark>=50 & Mark<60 ~ "B", # if Mark is between 50 and 60 (exclusive), assign 'B'
  Mark>=40 & Mark<50 ~ "C", # if Mark is between 40 and 50 (exclusive), assign 'C'
  Mark>=35 & Mark<40 ~ "D", # if Mark is between 35 and 40 (exclusive), assign 'D'
  Mark<35 ~ "F", # if Mark is less than 35, assign 'F'
  TRUE ~ "UNGRADED" # if none of the above conditions are met, assign 'UNGRADED'
))

# If the 'UE' column is empty, set the 'Grade' column to 'Postponed'
sr <- mutate(sr,Grade = case_when(is.na(UE)~"Postponed",
                                    TRUE~Grade))

# Convert the 'Grade' column to a factor
sr <- mutate(sr,Grade = as_factor(Grade))

# Display summary statistics of the data
summary(sr)

# Write the data to a CSV file
write_csv(sr,"Week-15/Student_Records.csv")

# Write the data to an Excel file
write_xlsx(sr,"Week-15/Student_Records.xls")

## BONUS Content

# Create a histogram of the 'Mark' column
ggplot(sr, aes(x = Mark)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Student Marks", x = "Mark", y = "Frequency")

# Create a scatterplot of the 'Mark' column against the 'UE' column
ggplot(sr, aes(x = UE, y = Mark)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot of Marks vs. UE", x = "UE", y = "Mark")

# Create a bar graph of the 'Grade' column
ggplot(sr, aes(x = Grade)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Student Grades", x = "Grade", y = "Count")






