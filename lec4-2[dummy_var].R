library(dplyr)
library(MASS)
library(ggplot2)
install.packages("splines")
library(splines)

house <- read.csv("d:/data/sds/house_sales.csv", sep='\t')
head(house[,'PropertyType'])

## dummy화 ##
# p-1개 --> p개는 다중공선성 가능성 #
prop_type_dummies <- model.matrix(~PropertyType-1, data=house)
head(prop_type_dummies)

# 회귀 모델 #
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms +
     BldgGrade + PropertyType, data=house)

table(house$ZipCode)

# 요인변수 통합 #
zip_groups <- house %>% 
  mutate(resid = residuals(house_lm)) %>% 
  group_by(ZipCode) %>% 
  summarize(med_resid=median(resid), cnt=n()) %>% 
  arrange(med_resid) %>% 
  mutate(cum_cnt=cumsum(cnt), ZipGroup=ntile(cum_cnt, 5))

house <- house %>% 
  left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')
head(house)

house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                   Bedrooms + BldgGrade + PropertyType + 
                   NbrLivingUnits +
                   SqFtFinBasement + YrBuilt + YrRenovated + 
                   NewConstruction, data=house, na.action=na.omit)
step_lm <- stepAIC(house_full, direction='both')
# 예측변수간 상관 #
step_lm$coefficients
update(step_lm, . ~ . -SqFtTotLiving - SqFtFinBasement - Bathrooms)

# zipgroup 변수 추가 #
lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot +
     Bathrooms + Bedrooms + BldgGrade +
     PropertyType + ZipGroup, data=house, na.action=na.omit)

# 상호 작용 #
lm(AdjSalePrice ~ SqFtTotLiving*ZipGroup + SqFtLot + Bathrooms +
     Bedrooms + BldgGrade + PropertyType, data=house, na.action=na.omit)

house_98105 <- house[house$ZipCode == 98105,]
lm_98105 <- lm(AdjSalePrice~SqFtTotLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade, data=house_98105)
sresid <- rstandard(lm_98105)
idx <- order(sresid)
std_resid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105)
hat_values <- hatvalues(lm_98105)
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)

df <- data.frame(resid=residuals(lm_98105),
                 pred=predict(lm_98105))
ggplot(df, aes(pred, abs(resid)))+
  geom_point() + geom_smooth()

terms <- predict(lm_98105, type='terms')
partial_resid <- resid(lm_98105) + terms
df<- data.frame(SqFtTotLiving = house_98105[,'SqFtTotLiving'],
                Terms = terms[,'SqFtTotLiving'],
                PartialResid = partial_resid[, 'SqFtTotLiving'])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2)+
  geom_line(aes(SqFtTotLiving, Terms))

## 다항회귀 ##
lm(AdjSalePrice ~ poly(SqFtTotLiving, 2) + SqFtLot +
     BldgGrade + Bathrooms + Bedrooms, data=house_98105)

# GAM #
library(mgcv)
lm_gam <- gam(AdjSalePrice ~ s(SqFtTotLiving) + SqFtLot +
                Bathrooms + Bedrooms + BldgGrade, data=house_98105)
lm_gam
