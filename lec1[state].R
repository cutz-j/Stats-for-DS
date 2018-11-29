install.packages("matrixStats")
library(dplyr)
library(matrixStats)

state <- read.csv(file="d:/data/sds/state.csv", stringsAsFactors = FALSE)
head(state)
mean(state[["Population"]], trim=0.1)

weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])
class(state["Murder.Rate"])

## 변위 추정 ##
sd(state[["Population"]])
IQR(state[["Population"]])
mad(state[["Population"]])

## 분위 ##
quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95))
boxplot(state[["Population"]]/1000000, ylab="Population")

## histogram ##
breaks <- seq(from=min(state[["Population"]], to=max(state[["Population"]], length=11)))
# hist(state[["Population"]], breaks=breaks)

hist(state[["Murder.Rate"]], freq=FALSE)
class(state[["Murder.Rate"]])
lines(density(state[["Murder.Rate"]]), lwd=3, col='blue')
