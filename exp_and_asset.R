##SETTING A WORKING DIRECTORY##
setwd("*your working directory here")

library(readstata13) #Function to read and write the 'Stata' file format
library(NbClust) #Function to cluster
library(mclust) #Function to cluster
library(gmodels) #Function to cluster
library(dplyr)

##GENERATING HOUSEHOLD-LEVEL MONTHLY FOOD EXPENDITURE VARIABLE
#Food expenditure data is available in "b1_ks1.dta"
b1_ks1 <- read.dta13("b1_ks1.dta")
#Note: The food data is captured in "ks1type" and "ks02" columns -- to simplify the data frame, the "b1_ks1" variable will be restructured to only reflect household ID ("hhid14_9"), food item type ("ks1type"), and weekly food expenditure ("ks02")

b1_ks1 <- read.dta13("b1_ks1.dta")[,c("hhid14_9", "ks1type", "ks02")]
#Note: The data is currently structured in a long format. For easier variable generation, this needs to be restructured into a wide format where each household ID's response will only be recorded in a single row
food_exp <- reshape(b1_ks1, idvar="hhid14_9",timevar="ks1type",direction="wide")
attach(food_exp)

#Now that the data is already in a long format, food expenditure is basically calculated as the sum of expenditure for each of the food item

food_exp$food <- (rowSums(food_exp[,-1]))*52/12 #calculating monthly expenditure
food_exp <- food_exp[,c("hhid14_9","food")]

##GENERATING HOUSEHOLD-LEVEL MONTHLY NON-FOOD EXPENDITURE VARIABLE
#Non-food expenditure data is available in "b1_ks0", "b1_ks2", "b1_ks3", and "b2_kr"

b1_ks2 <- read.dta13("b1_ks2.dta")
#Note: The non-food data is captured in "ks2type" and "ks06" columns -- to simplify the data frame, the "b1_ks1" variable will be restructured to only reflect household ID ("hhid14_9"), non-food item type ("ks2type"), and monthly non-food expenditure ("ks06")

b1_ks2 <- read.dta13("b1_ks2.dta")[,c("hhid14_9", "ks2type", "ks06")]
#Note: The data is currently structured in a long format. For easier variable generation, this needs to be restructured into a wide format where each household ID's response will only be recorded in a single row
nonfood_exp_1 <- reshape(b1_ks2, idvar="hhid14_9",timevar="ks2type",direction="wide")
attach(nonfood_exp_1)

#Now that the data is already in a long format, non-food expenditure is basically calculated as the sum of expenditure for each of the non-food item

nonfood_exp_1$nonfood1 <- (rowSums(nonfood_exp_1[,2:13])) #calculating monthly non-food expenditure
nonfood_exp_1 <- nonfood_exp_1[,c("hhid14_9","nonfood1")]

b1_ks3 <- read.dta13("b1_ks3.dta")
#Note: The non-food data is captured in "ks3type" and "ks08" columns -- to simplify the data frame, the "b1_ks3" variable will be restructured to only reflect household ID ("hhid14_9"), non-food item type ("ks3type"), and annual non-food expenditure ("ks08")

b1_ks3 <- read.dta13("b1_ks3.dta")[,c("hhid14_9", "ks3type", "ks08")]
#Note: The data is currently structured in a long format. For easier variable generation, this needs to be restructured into a wide format where each household ID's response will only be recorded in a single row
nonfood_exp_2 <- reshape(b1_ks3, idvar="hhid14_9",timevar="ks3type",direction="wide")
attach(nonfood_exp_2)

#Now that the data is already in a long format, non-food expenditure is basically calculated as the sum of expenditure for each of the non-food item

nonfood_exp_2$nonfood2 <- (rowSums(nonfood_exp_2[,-1]))/12 #calculating monthly non-food expenditure
nonfood_exp_2 <- nonfood_exp_2[,c("hhid14_9","nonfood2")]

#Non-food (Housing Rent)
b2_kr <- read.dta13("b2_kr.dta")
#Note: The non-food data (housing rent) is captured in "kr04a" -- to simplify the data frame, the "b2_kr" variable will be restructured to only reflect household ID ("hhid14_9"), monthly/annual rent indicator ("kr04ax"), and housing rent expenditure ("kr04a")

rent_exp <- read.dta13("b2_kr.dta")[,c("hhid14_9","kr04ax","kr04a")]
rent_exp$kr04a[is.na(rent_exp$kr04a)] <- 0 #fill in the NAs with 0
rent_exp$kr04ax[is.na(rent_exp$kr04ax)] <- 0 #fill in the NAs with 0

#if kr04ax == 1, kr04a is an annual housing rent expenditure -- need to be standardized into monthly housing rent expenditure
#if kr04ax == 2, kr04a is a monthly housing rent expenditure
rent_exp$rent <- ifelse(rent_exp$kr04ax == 1,(rowSums(rent_exp[-1:-2])/12),rent_exp$kr04a)

rent_exp <- rent_exp[,c("hhid14_9","rent")

#Non-food (Education-related expenditure)
b1_ks0 <- read.dta13("b1_ks0.dta")
#Education-related expenditure (annual) is captured in "ks10aa", "ks11aa", "ks12aa"
#The data is already in a wide format. 

edu_exp <- read.dta13("b1_ks0.dta")[,c("hhid14_9", "ks10aa", "ks11aa", "ks12aa")]
#Since it is an annual expenditure, the data needs to be standardized into monthly education expenditure
edu_exp$edu <- (rowSums(edu_exp[,-1]))/12

edu_exp <- edu_exp[,c("hhid14_9","edu")]

##AGGREGATING THE NON-FOOD EXPENDITURES##
#NON-FOOD = nonfood_exp_1+nonfood_exp_2+rent_exp+edu_exp

#merging nonfood_exp_1 and nonfood_exp_2
nf12 <- merge(nonfood_exp_1,nonfood_exp_2,by="hhid14_9")

#merging nf12 and rent_exp
nf_rent <- merge(nf12,rent_exp,by="hhid14_9")

#merging nf_rent and edu_exp
nonfood_exp <- merge(nf_rent, edu_exp, by="hhid14_9")
attach(nonfood_exp)

#calculating total non food expenditure
nonfood_exp$sum_nonfood_exp <- nonfood1 + nonfood2 + rent + edu

##AGGREGATING THE NON-FOOD AND FOOD EXPENDITURES##
household_exp <- merge(nonfood_exp, food_exp, by="hhid14_9")
attach(household_exp)
household_exp$sum_exp <- sum_nonfood_exp + food

