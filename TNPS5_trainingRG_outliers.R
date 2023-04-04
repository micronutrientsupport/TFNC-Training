# TFNC training on 04/04/23 by RG
# Checking outliers

#calculate number of household members who eat food in the last 7 days

# open HH_SEC_B.csv

dataB <- read_csv("HH_SEC_B.csv")

# change variable names for age, sex and eating household members

dataB <- dataB %>%
  rename(sex = hh_b02, age = hh_b04, eat7d = hh_b07)

str(dataB)

dataB <- dataB %>%
  mutate(
    sex = as.factor(sex),
    eat7d = as.factor(eat7d),
    sdd_hhid = as.factor(sdd_hhid)
  )

str(dataB)

# counting number of members who eat in this HH in the last 7 days

eat7d_Y <- count(dataB, sdd_hhid, eat7d)
summary(eat7d_Y)
# there are 3 NA in eat7d, max 34 (outliner?) in the number
# if there are no members who eat in the HH, there is no YES count - so if I choose only Yes, the household will be omitted.

hh_eat7d_all <- count(dataB, sdd_hhid, eat7d, .drop = FALSE, name = "hh_pop")

hh_eat7d_all <- hh_eat7d_all[hh_eat7d_all$eat7d == "YES", ]
summary(hh_eat7d_all)

hh_eat7d_all <- hh_eat7d_all[hh_eat7d_all$eat7d == "YES", c(1, 3)]
summary(hh_eat7d_all)
