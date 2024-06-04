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
#no, yes
