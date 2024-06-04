# Load necessary libraries
library(quantmod)

# Retrieve the data on the exchange rate between the Swiss Franc and the US Dollar from FRED
getSymbols("DEXSZUS", src = "FRED")

# Remove the “NA” observations of this series
chf_usd <- na.omit(DEXSZUS)

# Restrict the dates of this series from 1979-12-31 to 2017-12-31
chf_usd <- chf_usd["1979-12-31/2017-12-31"]

# Invert the exchange rate to get the number of US Dollars per Swiss Franc
usd_chf <- 1 / chf_usd

# Calculate daily log returns
log_returns <- diff(log(usd_chf))[-1]

#Skeweness 
library(moments)
rvec <- as.vector(log_returns)
round(skewness(rvec),2) #-0.09 vs -0.91

#kurtosis
round(kurtosis(rvec),2) #15.43 vs 21.80

#jarque-bera test 

jarque.test(rvec) #61330 vs 142510


library(MASS)
rvec <- as.vector(log_returns)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6) 
install.packages('metRology')
alpha <- 0.01
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


#method 1 - 
library(MASS)
rvec <- as.vector(log_returns)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6) 

alpha <- 0.01
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

alpha <- 0.01
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

alpha <- 0.01
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

