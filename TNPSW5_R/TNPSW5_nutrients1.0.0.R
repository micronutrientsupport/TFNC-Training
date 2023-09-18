# Tanzania National Panel Survey Wave 5 data analysis - 4
# R. Goto

# merging FCT and calculate apparent micronutrient intake per AFE

# Settings

install.packages("tidyverse")
install.packages("summarytools")
library(tidyverse)
library(summarytools)

# open estimated food consumption data (dataJ1_hhcons.csv)

dataJ1_hhcons <- read_csv("dataJ1_hhcons.csv")

# get summary of estimated food consumption by food items per capita

df_hhcons <- dataJ1_hhcons %>%
  group_by(itemcode) %>%
  summarise(mean = mean(hh_cons_pc, na.rm = TRUE),
            median = median(hh_cons_pc, na.rm = TRUE),
            q25 = quantile(hh_cons_pc, probs = 0.25, na.rm = TRUE),
            q75 = quantile(hh_cons_pc, probs = 0.75, na.rm = TRUE))

df_hhcons <- df_hhcons %>% 
  mutate(mean = round(mean, 1),
         median = round(median, 1),
         q25 = round(q25, 1),
         q75 = round(q75, 1))

# save the output

View(df_hhcons)
write_csv(df_hhcons, 'hhcons_pc_summary.csv')


# open Tanzania FCT (TFNC_NCT_NTPS20_v.1.0.0.csv) and AFE on each household (df_AFE.csv)


TZFCT <- read_csv("TFNC_NCT_NTPS20_v.1.0.0.csv")

# organise TZFCT
names(TZFCT)
#omit a variable from column #2-9
TZFCT <- TZFCT %>% 
  select(-c(2:9))

#merge TZFCT with estimated food consumption in g

dataJ1_nut <- left_join(dataJ1_hhcons, TZFCT, by = "item_id")

# calculate nutrients intake using household food comsumption (hh_con) and TZFCT nutrients per 100g - the calculated value was not displayed as the data length is too wide. So cut some of the columns.

names(dataJ1_nut)

dataJ1_nut <- dataJ1_nut %>% 
  select(-c(8:14,23:25))

dataJ1_nut <- dataJ1_nut %>% 
  mutate(hh_kcal = hh_cons*0.01*ENERCkcal,
         hh_water = hh_cons*0.01*WATERg,
         hh_ash = hh_cons*0.01*ASHg,
         hh_prot = hh_cons*0.01*PROCNTg,
         hh_fat = hh_cons*0.01*FAT_g_standardised,
         hh_fib = hh_cons*0.01*FIBTGg,
         hh_alc = hh_cons*0.01*ALCg,
         hh_choavl = hh_cons*0.01*CHOAVLg,
         hh_choavldf = hh_cons*0.01*CHOAVLDFg,
         hh_fe = hh_cons*0.01*FEmg,
         hh_zn = hh_cons*0.01*ZNmg,
         hh_va = hh_cons*0.01*VITA_RAEmcg,
         hh_thi = hh_cons*0.01*THIAmg,
         hh_rib = hh_cons*0.01*RIBFmg,
         hh_nia = hh_cons*0.01*NIAmg,
         hh_fol = hh_cons*0.01*FOLmcg_standardised,
         hh_vb12 = hh_cons*0.01*VITB12mcg,
         hh_ret = hh_cons*0.01*RETOLmcg,
         hh_bcar = hh_cons*0.01*CARTBmcg,
         hh_acar = hh_cons*0.01*CARTAmcg,
         hh_bcryp = hh_cons*0.01*CRYPXBmcg,
         hh_bcareq = hh_cons*0.01*CARTBEQmcg)

names(dataJ1_nut)

# save dataJ1_nut

write_csv(dataJ1_nut, 'dataJ1_nut.csv')

# sum of all nutrients in household consumption

names(dataJ1_nut)

df_hhnut <- dataJ1_nut %>%
  group_by(sdd_hhid) %>%
  summarise(kcal_t = sum(hh_kcal, na.rm = TRUE),
            water_t = sum(hh_water, na.rm = TRUE),
            ash_t = sum(hh_ash, na.rm = TRUE),
            prot_t = sum(hh_prot, na.rm = TRUE),
            fat_t = sum(hh_fat, na.rm = TRUE),
            fib_t = sum(hh_fib, na.rm = TRUE),
            alc_t = sum(hh_alc, na.rm = TRUE),
            choavl_t = sum(hh_choavl, na.rm = TRUE),
            choavldf_t = sum(hh_choavldf, na.rm = TRUE),
            fe_t = sum(hh_fe, na.rm = TRUE),
            zn_t = sum(hh_zn, na.rm = TRUE),
            va_t = sum(hh_va, na.rm = TRUE),
            thi_t = sum(hh_thi, na.rm = TRUE),
            rib_t = sum(hh_rib, na.rm = TRUE),
            nia_t = sum(hh_nia, na.rm = TRUE),
            fol_t = sum(hh_fol, na.rm = TRUE),
            vb12_t = sum(hh_vb12, na.rm = TRUE),
            ret_t = sum(hh_ret, na.rm = TRUE),
            bcar_t = sum(hh_bcar, na.rm = TRUE),
            acar_t = sum(hh_acar, na.rm = TRUE),
            bcryp_t = sum(hh_bcryp, na.rm = TRUE),
            bcareq_t = sum(hh_bcareq, na.rm = TRUE))


# Exclude the households with 0 kcal - became total 1105 HHs
View(df_hhnut)
df_hhnut_c <- df_hhnut %>% 
  filter(kcal_t > 0)


# open df_AFE.csv

df_AFE <- read_csv("df_AFE.csv")

# merge with df_hhnut and AFE

df_nutAFE <- left_join(df_hhnut_c, df_AFE, by = "sdd_hhid")

# calculate apparent micronutrient intake per AFE/d with clearning data

names(df_nutAFE)

df_nutAFE <- df_nutAFE %>% 
  mutate(kcal_AFE = kcal_t/AFE_hh/7,
         water_AFE = water_t/AFE_hh/7,
         ash_AFE = ash_t/AFE_hh/7,
         prot_AFE = prot_t/AFE_hh/7,
         fat_AFE = fat_t/AFE_hh/7,
         fib_AFE = fib_t/AFE_hh/7,
         alc_AFE = alc_t/AFE_hh/7,
         choavl_AFE = choavl_t/AFE_hh/7,
         choavldf_AFE = choavldf_t/AFE_hh/7,
         fe_AFE = fe_t/AFE_hh/7,
         zn_AFE = zn_t/AFE_hh/7,
         va_AFE = va_t/AFE_hh/7,
         thi_AFE = thi_t/AFE_hh/7,
         rib_AFE = rib_t/AFE_hh/7,
         nia_AFE = nia_t/AFE_hh/7,
         fol_AFE = fol_t/AFE_hh/7,
         vb12_AFE = vb12_t/AFE_hh/7,
         ret_AFE = ret_t/AFE_hh/7,
         bcar_AFE = bcar_t/AFE_hh/7,
         acar_AFE = acar_t/AFE_hh/7,
         bcryp_AFE = bcryp_t/AFE_hh/7,
         bcareq_AFE = bcareq_t/AFE_hh/7)

names(df_nutAFE)
View(df_nutAFE)

hist(df_nutAFE$kcal_AFE)

hist(df_nutAFE$kcal_AFE, xlab="kcal/AFE/d", ylab="number of household", main="kcal intake per AFE")

hist(df_nutAFE$prot_AFE, xlab="protein in g/AFE/d", main=" ")
hist(df_nutAFE$fe_AFE, xlab="iron in mg/AFE/d", main=" ")
hist(df_nutAFE$zn_AFE, xlab="zinc in mg/AFE/d", main=" ")
hist(df_nutAFE$va_AFE, xlab="VA RAE mcg/AFE/d", main=" ")
hist(df_nutAFE$thi_AFE, xlab="thiamine (B1) in mg/AFE/d", main=" ")
hist(df_nutAFE$rib_AFE, xlab="riboflavin (B2) in mg/AFE/d", main=" ")
hist(df_nutAFE$nia_AFE, xlab="niacin (B3) in mg/AFE/d", main=" ")
hist(df_nutAFE$fol_AFE, xlab="folate in mcg/AFE/d", main=" ")
hist(df_nutAFE$vb12_AFE, xlab="cyanobalamin (B12) in mcg/AFE/d", main=" ")

boxplot(df_nutAFE$kcal_AFE, xlab="kcal/AFE/d", main=" ")
boxplot(df_nutAFE$prot_AFE, xlab="protein in g/AFE/d", main=" ")
boxplot(df_nutAFE$fe_AFE, xlab="iron in mg/AFE/d", main=" ")
boxplot(df_nutAFE$zn_AFE, xlab="zinc in mg/AFE/d", main=" ")
boxplot(df_nutAFE$va_AFE, xlab="VA RAE mcg/AFE/d", main=" ")
boxplot(df_nutAFE$thi_AFE, xlab="thiamine (B1) in mg/AFE/d", main=" ")
boxplot(df_nutAFE$rib_AFE, xlab="riboflavin (B2) in mg/AFE/d", main=" ")
boxplot(df_nutAFE$nia_AFE, xlab="niacin (B3) in mg/AFE/d", main=" ")
boxplot(df_nutAFE$fol_AFE, xlab="folate in mcg/AFE/d", main=" ")
boxplot(df_nutAFE$vb12_AFE, xlab="cyanobalamin (B12) in mcg/AFE/d", main=" ")


# kcal_AFE contains some 0 values - change to NA (NA_real_ (TRUE), kcal_AFE (FALSE))
df_nutAFE <- df_nutAFE %>%
  mutate(kcal_AFE = if_else(kcal_AFE == 0, NA_real_, kcal_AFE))



# calculate lower and upper quatile range and median - decimal point 1
round(quantile(df_nutAFE$kcal_AFE, na.rm = TRUE), 1) 
round(quantile(df_nutAFE$prot_AFE, na.rm = TRUE), 1) 
round(quantile(df_nutAFE$va_AFE, na.rm = TRUE), 1)
round(quantile(df_nutAFE$thi_AFE, na.rm = TRUE), 1) 
round(quantile(df_nutAFE$rib_AFE, na.rm = TRUE), 1) 
round(quantile(df_nutAFE$nia_AFE, na.rm = TRUE), 1) 
round(quantile(df_nutAFE$fol_AFE, na.rm = TRUE), 1) 
round(quantile(df_nutAFE$vb12_AFE, na.rm = TRUE), 1) 
round(quantile(df_nutAFE$fe_AFE, na.rm = TRUE), 1) 
round(quantile(df_nutAFE$zn_AFE, na.rm = TRUE), 1) 

kcal prot fe zn va thi rib nia fol vb12



View(df_AFEsummary)

dataJ1_pc <- dataJ1_pc %>% 
  mutate(cons_g_pc = round(cons_g_pc, 1))
            
            
            )
max(df_nut$kcal_AFE, na.rm = TRUE)
min(df_nut$kcal_AFE, na.rm = TRUE)




install.packages("skimr")
skimr::skim(df_nut)



# log?

df_nut <- df_nut %>% 
  mutate(kcal_AFE_lg10 = log10(kcal_AFE))
hist(df_nut$kcal_AFE_lg10)
sd(df_nut$kcal_AFE_lg10, na.rm=TRUE)
# +/- 2SD? 3SD?


