library(ggplot2)

session_times = read.csv("d:/data/sds/web_page_data.csv")
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

## conversion rate ##
obs_pct_diff <- 100*(200/23739 - 182/22588)
conversion <- c(rep(0, 45945), rep(1, 382))
perm_diffs <- rep(0, 1000)
for (i in 1:1000)
  perm_diffs[i] = 100 * perm_fun(conversion, 23739, 22588)
hist(perm_diffs, main='')
abline(v = obs_pct_diff, lty=2, lwd=1.5)

# 정규분포 사용해 p근사 #
prop.test(x=c(200,182), n=c(23739, 22588), alternative='greater')

## t-test ##
t.test(Time ~ Page, data=session_times, alternative='less')

## ANOVA ##
install.packages("lmPerm")
library(lmPerm)
four_sessions <- read.csv("d:/data/sds/four_sessions.csv")
summary(aovp(Time ~ Page, data=four_sessions))
summary(aov(Time ~ Page, data=four_sessions))

## chi-sq ##
clicks <- read.csv("d:/data/sds/click_rates.csv")
chisq.test(clicks, simulate.p.value=TRUE)

