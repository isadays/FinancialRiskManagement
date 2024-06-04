library(quantmod)

load('FRED_gold')
head(gold)


# Restrict dates
gold <- gold["1979-12-31/2017-12-31"]
gold <-na.omit(gold)
log_returns <- diff(log(gold))[-1]
library(MASS)
rvec <- as.vector(log_returns)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6) 
install.packages('metRology')
alpha <- 0.05
RNGkind(sample.kind="Rounding")

set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6) 
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6) #-0.016094
round(ES,6) #-0.023371

