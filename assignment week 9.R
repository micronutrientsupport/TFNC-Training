# Loading libraries
library(tidyverse)
#loading data
df<-read.csv("Week-9/Materials/data/TZ08_FCT_Partial.csv")
#checking the loaded data
view(df)
#how many rows has the data?
nrow(df)
[1] 382
#what are the variables names?
names(df)
"X"                               
[2] "food.ID"                         
[3] "protein..mg."                    
[4] "Fat..in.g."                      
[5] "Fat..total.saturated..g."        
[6] "Total.Carbohydrate.by.difference"
[7] "food_description"                
[8] "Fibre..in.grams"                 
[9] "Phytic.acid..mcg."               
[10] "Vitamin.A.RAE..mcg."             
[11] "A.Vitamin.A..mcg."               
[12] "Vitamin.D..mcg."                 
[13] "Vitamin.E..mcg."                 
[14] "Calcium..mg."                    
[15] "Copper..Cu..mg"                  
[16] "Manganese..MNmg."                
[17] "Magnesium..Mg...mg"              
[18] "Food.group.descriptor"           
[19] "Potassium..K...mg."              
[20] "energy..kcal."                   
[21] "Sodium..Na..mg"                  
[22] "Iron.mcg"                        
[23] "Zinc.mg"                         
[24] "Phosphorus..mg."    
colnames(df)
#tidying the data, Visually checking the data
head(df)
tail(df)
view(df)
## 1.Trimming the data set vertically
df <- df %>% slice(c(1:378))
## 2.Reorder/select the colomn to be kept.
df <- df %>% select(food.ID,protein..mg.,Fat..in.g.,Fat..total.saturated..g.,Total.Carbohydrate.by.difference,food_description,Fibre..in.grams,Phytic.acid..mcg.,Calcium..mg.,Copper..Cu..mg,Manganese..MNmg.,Magnesium..Mg...mg,Food.group.descriptor,Potassium..K...mg.,energy..kcal.,Sodium..Na..mg,Iron.mcg,Zinc.mg,Phosphorus..mg.)
df <- df %>% rename(ID=food.ID,PROCNTmg=protein..mg.,FAT_g=Fat..in.g.,FASATg=Fat..total.saturated..g.,CHOCDFg=Total.Carbohydrate.by.difference,food_description=food_description,FIB_g=Fibre..in.grams,PHYTACmg=Phytic.acid..mcg.,CAmg=Calcium..mg.,CUmg=Copper..Cu..mg,MNmg=Manganese..MNmg.,MGmg=Magnesium..Mg...mg,Food_Group=Food.group.descriptor,Kmg=Potassium..K...mg.,ENERCkcal=energy..kcal.,NAmg=Sodium..Na..mg,FEmcg=Iron.mcg,ZNmg=Zinc.mg,Pmg=Phosphorus..mg.)
names(df)
colnames(df)
#reculculate colomn, 
#df <- df %>% mutate(CAmg = CAmcg/1000)
df <- df %>% mutate(FEmg = FEmcg,PROCNTmg=PROCNTmg)
names(df)
colnames(df)
#rlang::last_trace(df)`
