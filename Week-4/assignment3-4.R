

# 1) Creating a data.frame

# Admin. Region: Creating character vector
admin_regions <-   c("Arusha", "Dar es Salaam", "Dodoma", "Geita", "Iringa"	,
                     "Kagera", "Katavi"	, "Kigoma"	, "Kilimanjaro", "Lindi")

# Changing character vector to factor
admin_regions <- as.factor(admin_regions)

# Populations: Creating numeric vector
population <- c(1000000, 2000000,	3000000, 1500000,	1200000, 1100000, 
                800000, 600000,	700000, 500000)

# Households: Creating a numeric vector
households <- c(100000, 150000, 200000, 75000, 60000, 55000, 
                40000, 30000, 35000, 25000) 

# Creating data.frame

df <- data.frame(admin_regions, population, households)

# 2) Checking a data.frame

# Checking the data.frame structure
str(df)

# Checking the data.frame variables
head(df)
