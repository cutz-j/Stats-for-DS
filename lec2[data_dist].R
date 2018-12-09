### 2장 분포 ###
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(boot)

loans_income <- read.csv("c:/data/sds/loans_income.csv")
x <- seq(from=-3, to=3, length=300)
stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R = 1000, statistic=stat_fun)

samp_data <- data.frame(income=sample(loans_income, 1000, replace=TRUE),
                        type='data_dist')
samp_mean_05 <- data.frame(
  income=tapply(sample(loans_income, 1000*5, replace=TRUE), 
                rep(1:1000, rep(5, 1000)), FUN=mean), 
  type='mean_of_5')

samp_mean_20 <- data.frame(
  income = tapply(sample(loans_income, 1000*20, replace=TRUE),
                  rep(1:1000, rep(20, 1000)), FUN=mean),
  type='mean_of_20')

# rbind 후 factor type 변환 #
income <- rbind(samp_data, samp_mean_05, samp_mean_20)
income$type = factor(income$type,
                     levels=c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels=c('Data', 'Mean of 5', 'Mean of 20'))

ggplot(income, aes(x=income)) +
  geom_histogram(bins=40) +
  facet_grid(type ~ .)




