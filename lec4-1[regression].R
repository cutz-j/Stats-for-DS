### lec3. Regression ###
install.packages("MASS")
library(MASS)
install.packages("lubridate")
library(lubridate)

lung = read.csv("d:/data/sds/LungDisease.csv")
model <- lm(PEFR ~ Exposure, data=lung)
model
scatter(lung)
scatter(model)

#  residual #
fitted <- predict(model)
resid <- residuals(model)

## Polynomial LR ##
house <- read.csv("d:/data/sds/house_sales.csv", sep='\t')
head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms",
               "Bedrooms", "BldgGrade")])
house_lm = lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                Bedrooms + BldgGrade, data=house, na.action=na.omit)
house_lm
summary(house_lm)
house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                   Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                   SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                 data=house, na.action=na.omit)
step <- stepAIC(house_full, direction='both')

house$year = year(house$DocumentDate)
house$Weight = house$year - 2005

house_wt <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,
               data=house, weight=Weight, na.action=na.omit)
house_wt
` `