library(quantmod)

load('FRED_gold')
head(gold)


# Restrict dates
gold <- gold["1979-12-31/2017-12-31"]
gold <-na.omit(gold)
log_returns <- diff(log(gold))[-1]

library(rugarch)
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = log_returns[,1]) 
fit.garch@fit$coef 
save1 <- cbind( log_returns[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c( "log_returns", "s", "z" ) 
set.seed(123789) 
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp") 
rvec <- boot.garch@fseries
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])
print(VaR)
print(ES)

#-0.009215796,-0.01324071, -0.04806968, Yes, -0.02851939,No
#####################
# Restrict dates
gold <- gold["1979-12-31/1987-10-19"]
gold <-na.omit(gold)
log_returns <- diff(log(gold))[-1]

library(rugarch)
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = log_returns[,1]) 
fit.garch@fit$coef 
save1 <- cbind( log_returns[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c( "log_returns", "s", "z" ) 
set.seed(123789) 
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp") 
rvec <- boot.garch@fseries
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])
print(VaR)
print(ES)


###############

# Restrict dates
gold <- gold["1979-12-31/2008-09-15"]
gold <-na.omit(gold)
log_returns <- diff(log(gold))[-1]

library(rugarch)
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = log_returns[,1]) 
fit.garch@fit$coef 
save1 <- cbind( log_returns[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c( "log_returns", "s", "z" ) 

RNGkind(sample.kind="Rounding")


set.seed(123789) 
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp") 
rvec <- boot.garch@fseries
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])
print(VaR)
print(ES)
