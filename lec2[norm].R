norm_samp <- rnorm(100)
qqnorm(norm_samp)
abline(a=0, b=1, col='grey')

sp500_px <- read.csv("c:/data/sds/sp500_px.csv")
nflx <- sp500_px[,'NFLX']
nflx <- diff(log(nflx[nflx>0]))
qqnorm(nflx)
abline(a=0, b=1, col='grey')

## binorm ##
dbinom(2, 5, 0.1)
pbinom(2, 5, 0.1)
pbinom(0, 200, 0.02) # �ѹ��� Ŭ���� ���� 0.02�� ��, 200ȸ Ŭ������ 0ȸ ���� Ȯ��

## Ǫ�Ƽ� ���� ##
rpois(100, lambda=2)

## ���̺� ���� ##
rweibull(100, 1.5, 5000)