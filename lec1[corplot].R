library(dplyr)
library(corrplot)
library(ggplot2)
install.packages("descr")
library(descr)

### 1.7 ������� ###

data <- read.csv("d:/data/sds/sp500_px.csv", stringsAsFactors = FALSE)
head(data)

# corrplot(cor(data[1,]), method='ellipse')

data$T
data$VZ


plot(data$T, data$VZ, xlab="T", ylab="VZ")

### 1.8 �ٺ��� �м� ###
kc_tax <- read.csv("d:/data/sds/kc_tax.csv", stringsAsFactors=FALSE)

# subset ����-> �̻�ġ ���� #
kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving > 100 & SqFtTotLiving < 3500)
nrow(kc_tax0)

## ������ ���� ##
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) +
  stat_binhex(colour='white') + theme_bw() +
  scale_fill_gradient(low="white", high="black")
## ����� ##
ggplot(kc_tax0, aes(x=SqFtTotLiving, y=TaxAssessedValue)) +
         theme_bw() + 
         geom_point(alpha=0.1) + 
         geom_density2d(colour='white')

## ������ vs ������ ##
lc_loans = read.csv("d:/data/sds/lc_loans.csv", stringsAsFactors=FALSE)
x_tab <- CrossTable(lc_loans$grade, lc_loans$status,
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)
x_tab

## ������ vs ��ġ�� ##
airline_stats <- read.csv("d:/data/sds/airline_stats.csv", stringsAsFactors=FALSE)
head(airline_stats)
ggplot(airline_stats, aes(airline, pct_carrier_delay)) +
  ylim(0, 50) +
  geom_violin()



