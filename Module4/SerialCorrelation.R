library(quantmod)

load('FRED_gold')
head(gold)


# Restrict dates
gold <- gold["1979-12-31/2017-12-31"]
gold <-na.omit(gold)
log_returns <- diff(log(gold))[-1]
acf(log_returns)
acf(abs(log_returns))
library(rugarch)
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = log_returns[,1]) 
fit.garch@fit$coef
save1 <- cbind( log_returns[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c("log_returns", "s", "z") 
acf(save1$z)
acf(abs(save1$z))

