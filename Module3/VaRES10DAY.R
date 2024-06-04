library(quantmod)

load('FRED_gold')
head(gold)


# Restrict dates
gold <- gold["1979-12-31/2017-12-31"]
gold <-na.omit(gold)
log_returns <- diff(log(gold))[-1]


#method 1 - 
library(MASS)
rvec <- as.vector(log_returns)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6) 

alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
print(VaR)
print(ES)


#method 2 - 

alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(log_returns),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
print(VaR)
print(ES)


#method 3 - 

alpha <- 0.05
set.seed(123789)
rdat <- as.vector(log_returns)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
print(VaR)
print(ES)
