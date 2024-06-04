library(quantmod)
library(xts)

# Retrieve the data from FRED
getSymbols("DEXJPUS", src = "FRED")

# Remove NA observations
yen_usd <- na.omit(DEXJPUS)

# Restrict dates
yen_usd <- yen_usd["1979-12-31/2017-12-31"]

usd_yen <- 1 / yen_usd

log_returns <- diff(log(usd_yen))[-1]

# Estimate the parameters of the normal distribution: mean and standard deviation
mu <- mean(log_returns)
sig <- sd(log_returns)


print(paste("The mean: ", mu))
print(paste("the standard deviation ", sig))

##USing the equation


#estimating VaR (value at risk)

var <- qnorm(0.01,mu,sig)
print(var)

#Estimating ES (cVaR) of the normal distribution
es <- mu-sig*dnorm(qnorm(0.01,mean=0,sd=1),mean=0,sd=1)/0.01
#HFvares <- 1000*(exp(es)-1) # in millions of dollars 
print(es)

# Method 1: Assuming normal distribution
RNGkind(sample.kind=”Rounding”) 
# Set seed for reproducibility
set.seed(123789)

# Simulate 100,000 outcomes from the normal distribution
rvec <- rnorm(100000, mu, sig)

# Calculate VaR and ES at the 99% confidence level
VaR_normal <- quantile(rvec, 0.01)
ES_normal <- mean(rvec[rvec < VaR_normal])

# Print the results
print(paste("VaR (Normal) at 99% confidence level: ", VaR_normal))
print(paste("ES (Normal) at 99% confidence level: ", ES_normal))




RNGkind(sample.kind="Rounding")
set.seed(123789)
rvec <- sample(as.vector(log_returns),100000,replace=TRUE) 
VaR <- quantile(rvec,0.01)
ES <- mean(rvec[rvec<VaR])
print(VaR)#-0.01778064
print(ES) #-0.02901677



HF_ES_normal <- 1000 * (exp(ES_normal) - 1)
HF_ES <- 1000*(exp(ES)-1)
print(HF_ES_normal)
print(HF_ES)

most_conservative_ES <- min(HF_ES_normal, HF_ES)
print(most_conservative_ES)
