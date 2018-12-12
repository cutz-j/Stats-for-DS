library(ggplot2)

session_times = read.csv("c:/data/sds/web_page_data.csv")
ggplot(session_times, aes(x=Page, y=Time))+
  geom_boxplot()

mean_a <- mean(session_times[session_times['Page']=='Page A', 'Time'])
mean_b <- mean(session_times[session_times['Page']=='Page B', 'Time'])
mean_b - mean_a

perm_fun <- function(x, n1, n2)
{
  n <- n1 + n2
  idx_b <- sample(1:n, n1)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

perm_diffs <- rep(0, 1000)
for (i in 1:1000)
  perm_diffs[i] = perm_fun(session_times[,'Time'], 21, 15)
hist(perm_diffs)
abline(v=mean_b - mean_a)
