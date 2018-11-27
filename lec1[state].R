install.packages("matrixStats")
library(dplyr)
library(matrixStats)

state <- read.csv(file="d:/data/sds/state.csv")
head(state)
mean(state[["Population"]], trim=0.1)

weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])
class(state["Murder.Rate"])
