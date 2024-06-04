load('FRED_gold')
head(gold)


# Restrict dates
gold <- gold["1979-12-31/2017-12-31"]

# Calculate daily log returns and discrete returns
log_returns <- diff(log(gold))[-1]
dis_returns <-exp(log_returns)-1


#estimating two parameters of normal distribution: mean and std
mu <- mean(log_returns)
sig <-sd(log_returns)


#estimating VaR (value at risk)

var <- qnorm(0.05,mu,sig)
print(var)

#Suppose the hedge fund invested the entire $1000 million in gold instead of US 
#equities. What is the VaR of the daily changes in the assets of the hedge fund,
#at the 95% confidence level? 
HFvar <- 1000*(exp(var)-1) # in millions of dollars
print(HFvar)
