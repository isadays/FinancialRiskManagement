library(quantmod)

load('FRED_gold')
head(gold)


# Restrict dates
gold <- gold["1979-12-31/2017-12-31"]

# Calculate daily log returns and discrete returns
log_returns <- diff(log(gold))[-1]

#Skeweness 
library(moments)
rvec <- as.vector(log_returns)
round(skewness(rvec),2) #-0.09 vs -0.91

#kurtosis
round(kurtosis(rvec),2) #15.43 vs 21.80

#jarque-bera test 

jarque.test(rvec) #61330 vs 142510
