library(dplyr)

dfw <- read.csv("d:/data/sds/dfw_airline.csv", stringsAsFactors=FALSE)
head(dfw)

barplot(as.matrix(dfw)/6)
