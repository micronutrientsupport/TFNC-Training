x<-"dataset"
is character(x<-"dataset")
is.character(x)
y<-c("aleswa", "hoyce","alex","rie")
is.character(y)
x<-c("1","23","32")
is.charecter(x)
is.character(x)
x<-(1,23,32)
is.character(x)
x<-c("1","23","32")
mean(x)
meanx
is.numeric(x)
hoyce<-c(12,13,14)
is.numeric(hoyce)
mean(hoyce)
inclass(hoyce)
class(hoyce)
class(y)
typeof(hoyce)
liberty(0.4,0.9,1.2)
typeof(liberty)
x<-5
x+5
x+7.5
x-13
(x*100)/2
y<-(x*100)/2
class(y)
example<-data.frame(indiv=c("Alex", "Aleswa","Edward","Chris","Hoyce","Liberty","Rie"),
                    height=c(176,160,178,172,161,161,153),
                    fav.col=c("blue","green","blue","red","maroon","black","pink"),
                    sex=c("Male","Female","Male","Male","Female","Male","Female"))



print(example)

example$sex<-as.factor(example$sex)
str(example)
summary(example)
table(example$sex)
barplot(table(example$sex))
example$fav.col<-as.factor(example$fav.col)

str(example)
summary(example)
plot(x=example$sex, y=example$height,ylab="Height(cm)",xlab="Sex")
instaexample
summary(example)
plot(example$height)
install.packages("psych")

install.packages('psych')
library("psych")

describe(example$height)

install.packages('psych')
library("psych")
describe(example$height)
rie<-("TFNC Training")
rie<-"TFNC"
print(rie)


# rie "Aleswa"
rie <-
x<- c("1","4","5","7","8")
is.character(x)
str(x)
as.numeric(x)
print(x)
x <- as.numeric(x)
x <- c("1","4","5","7","8")
x <- as.numeric(x)
x <- as.character(x)
x <- as.factor(x)
sex <- c("Male","Female","Male","Male","Female","Male","Female")
sex <- c("Male","Female","Male","Male","Female","Male","Female")
sex <- as.factor(sex)
str(sex)
summary

admin_reg <-c("Arusha", "Dar es salaam", "Dodoma", "Geita", "Iringa", "Kagera", "Katavi", "Kigoma", "Kilimanjaro", "Lindi")
admin_reg <- as.numeric(admin_reg)
admin_reg <- as.factor(admin_reg)
admin_reg <- as.character(admin_reg)
str(admin_reg)
population <- c(1000000,2000000,3000000,1500000,1200000,110000,800000,600000,700000,500000)
population <- as.numeric(population)
population <- as.character(population)
str(population )
households <- c(100000,150000,200000,75000,60000,55000,40000,30000,35000,25000)
str(households)
households <- as.numeric(households)
households <- as.character(households)
households <- as.factor(households)
#example<-data.frame(admin_reg=c("Arusha", "Dar es salaam", "Dodoma", "Geita", "Iringa", "Kagera", "Katavi", "Kigoma", "Kilimanjaro", "Lindi"),
#print(example),
#population=c(1000000,2000000,3000000,1500000,1200000,110000,800000,600000,700000,500000), households=c(100000,150000,200000,75000,60000,55000,40000,30000,35000,25000)
#print(example)
#mean(population)
#mean(households)
#example<-data.frame(households=c(100000,150000,200000,75000,60000,55000,40000,30000,35000,25000)
#Assignment # 4  
#Construction of data framework                    
admin_reg <-c("Arusha", "Dar es salaam", "Dodoma", "Geita", "Iringa", "Kagera", "Katavi", "Kigoma", "Kilimanjaro", "Lindi")
str(admin_reg)
population <- c(1000000,2000000,3000000,1500000,1200000,110000,800000,600000,700000,500000)
str(population )
households <- c(100000,150000,200000,75000,60000,55000,40000,30000,35000,25000)
households <- as.factor(households)
households <- as.numeric(households)
households <- is.character(households)
str(households)
example<-data.frame(admin_reg=c("Arusha", "Dar es salaam", "Dodoma", "Geita", "Iringa", "Kagera", "Katavi", "Kigoma", "Kilimanjaro", "Lindi"),
population=c(1000000,2000000,3000000,1500000,1200000,110000,800000,600000,700000,500000),
households=c(100000,150000,200000,75000,60000,55000,40000,30000,35000,25000))
print(example)
mean(population)
mean(households)
                    
admin_regions <- c("Arusha","Dar es Salaam","Dodoma","Geita","Iringa","Kagera","Katavi","Kigoma","Kilimanjaro","Lindi") 
str(admin_regions)
population <- c(1000000,2000000,3000000,1500000,1200000,1100000,800000,600000,700000,500000)  
str(population)
mean(population)
households <- c(100000,150000,200000,75000,60000,55000,40000,30000,35000,25000)
str(households)
mean(households)
example <- df(admin_regions=c("Arusha","Dar es Salaam","Dodoma","Geita","Iringa","Kagera","Katavi","Kigoma","Kilimanjaro","Lindi"),                    
population=c(1000000,2000000,3000000,1500000,1200000,1100000,800000,600000,700000,500000),
households=c(100000,150000,200000,75000,60000,55000,40000,30000,35000,25000))                    
print(example)
summary(example)
Q()

install.packages("tidyverse")                
View(installed.packages())                   
library(tidyverse)                    
hces.data <- read_csv("./Week-4/demo_hces_survey_data_v2.csv")   
head(hces.data)
str(hces.data)                    
summary(hces.data) 
#Rename
#Mutating data
hces_subset_columns <- hces_data |> 
rename(food_source = gifted) |> 
mutate(food_source = as.factor(food_source)) |> 
rename(province = ) |> 
(province = as.factor( ))


view(installed.packages())
library(tidyverse)
#hces.data <- read_csv("./Week-4/demo_hces_survey_data_v2.csv") 
hces_data <- read_csv("./TFNC-Training_hces_data_HH_sec_J1")
hces_data <- read_csv("hces-data_hh_sec_j1.csv")
hces_data <- read_csv("./hces-data_hh_sec_j1.csv")
dataJ1 <- read_csv("HH_SEC_J1.csv")
library(tidyverse)
hces.data <- read_csv("./hces-data/hh_sec_j1.csv")
hces.data <- read_csv("./TFNC-Training/hces-data_TZA_NPS-R5_v01_M_csv/hh_sec_j1.csv")
hces.data <- read_csv("hh_sec_j1")
hces.data <- ("./hces-data/hh_sec_j1,csv")
library(tidyverse)
###importing data self exercise
library(tidyverse)
data <- read_csv("hces-data/TZA_2020_NPS-5_v01_M_csv/hh_sec_j1.csv")

# Loading libraries
library(tidyverse)


# Loading the data
df <- read.csv("Week-9/Materials/data/TZ08_FCT_Partial.csv")

# Checking the loaded data
## How many rows has the data?

df <- read.csv("Week-9/Materials/data/TZ08_FCT_Partial.csv")

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

# Welcome to Week 9! 


# Loading libraries


# Loading the data


# Checking the loaded data

## How many rows has the data?


## How many columns has the data?


## What are the variables names? 


# Tidying the data

## 0) Visually checking the data



## 1) Trimming the dataframe vertically


slice(df)

## 2) Reorder/select the columns to be kept. For this task we aren't interested in the Vitamins; please do not include the Vitamin A's, Vitamin D, Vitamin E.
#Please order the columns so the metadata comes first, followed by macronutrients, then micronutrients.



## 3) Changing variable names. Please rename the variable to the FAO INFOODS Tagnames - fdc_id, PROCNTg, FAT_g, FASATg, CHOCDFg, food_description, FIB_g, PHYTACmg,
# CAmg, CUmg, MNmg, MGmg, Food_Group, Kmg, ENERCkcal, NAmg, FEmg, ZNmg, Pmg



## What are the variables names now?



## 4) Recalculate columns - Please convert the Iron column from mcg to mg, and the Protein column from mg to g.



## 5) Saving the data
##Loading the library
library(tidyverse)
df <- read.csv("Week-9/Materials/data/TZ08_FCT_Partial.csv")
df <- read.csv("Week-9/Materials/data/TZ08_FCT_Partial.csv")
##Checking the number rows
nrow(df)
length(df)
##Checking the number of columns
col(df)
##Column names
names(df)
colnames(df)
##Tidying or visualizing the data
head(df)
tail(df)
view(df)
#### 1) Trimming the dataframe vertically

df <- df |> slice(c(1:379))
slice(df)

## 2) Reorder/select the columns to be kept. For this task we aren't interested in the Vitamins; please do not include the Vitamin A's, Vitamin D, Vitamin E.
#Please order the columns so the metadata comes first, followed by macronutrients, then micronutrients.
df |> df(select(metadata,macronutrients, micronutrients))
df <- df |> select(energy..kcal., 
protein..mg., 
Fat..total.saturated..g., 
Total.Carbohydrate.by.difference, 
Fibre..in.grams, 
Phytic.acid..mcg.,
Vitamin.A.RAE..mcg., 
Vitamin.E..mcg.,
A.Vitamin.A..mcg.,
Vitamin.D..mcg.,
Calcium..mg.,
Manganese..MNmg.,
Prosphorus..mg.,
Zinc.mg., 
Sodium..Na..mg.,
Potassium..K...mg.,
Magnesium..Mg...mg.,
Copper..Cu..mg)


## 3) Changing variable names. Please rename the variable to the FAO INFOODS Tagnames - fdc_id, PROCNTg, FAT_g, FASATg, CHOCDFg, food_description, FIB_g, PHYTACmg,
# CAmg, CUmg, MNmg, MGmg, Food_Group, Kmg, ENERCkcal, NAmg, FEmg, ZNmg, Pmg

df <- df |> rename(fdc_id=food.ID,
PROCNTg=protein..mg.,
Fat_g=Fat..in.g., 
FASATg=Fat..total.saturated..g., 
food_description=Food.group.descriptor,
FIB_g=Fibre..in.grams,
PHYTACmg=Phytic.acid..mcg.,
CAmg=Calcium..mg.,
CUmg=Copper..Cu..mg,
MNmg=Manganese..MNmg.,
MGmg=Magnesium..Mg...mg,
Food_Group=Food.group.descriptor,
Kmg=Potassium..K...mg.,
ENERCkcal=energy..kcal.,
NAmg=Sodium..Na..mg,
FEmg=Iron.mcg,
ZINmg=Zinc.mg,
Pmg=Potassium..K...mg.)

## What are the variables names now?
##fdc_id, PROCNTg,Fat_g, FASATg, Food_description, FIB_g, PHYTACmg, CAmg, CUmg, MNmg, MGmg, Kmg, ENERCkal, NAmg, FEmg, ZINmg, Pmg)


## 4) Recalculate columns - Please convert the Iron column from mcg to mg, and the Protein column from mg to g.
df <- df |>mutate (PHYTACmg=PHYTACmcg/1000,
PROCNTg=PROCNTgm/1000)
df <- df |> mutate(Femg=Femcg/1000, PROCNTg=PROCNTmg/1000)
df <- df |> mutate(PROCNTg=PROCNTmg/1000)
df <- df |> mutate(CAmg=CAcmg)

## 5) Saving the data




