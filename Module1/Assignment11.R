library(quantmod)
library(xts)

# Retrieve the data from FRED
getSymbols("DEXUSAL", src = "FRED")

# Remove NA observations
DEXUSAL <- na.omit(DEXUSAL)

# Restrict dates
DEXUSAL <- DEXUSAL["1979-12-31/2017-12-31"]

# Calculate daily log returns and discrete returns
log_returns <- diff(log(DEXUSAL))[-1]
dis_returns <-exp(log_returns)-1

log_returns.w <- apply.weekly(log_returns,sum)
log_returns.m <- apply.monthly(log_returns,sum)
log_returns.q <- apply.quarterly(log_returns,sum)
log_returns.y <- apply.yearly(log_returns,sum)

dis_returns.w <- apply.weekly(dis_returns,sum)
dis_returns.m <- apply.monthly(dis_returns,sum)
dis_returns.q <- apply.quarterly(dis_returns,sum)
dis_returns.y <- apply.yearly(dis_returns,sum)

index(log_returns.q)[length(index(log_returns.q))]

coredata(log_returns.q)[nrow(log_returns.q)]

index(dis_returns.y)[length(index(dis_returns.y))]

coredata(dis_returns.y)[nrow(dis_returns.y)]




#1)
log_returns


#2)
log_returns

#3)

dis_returns.m
#4)

dis_returns.m
#5)

index(log_returns.q)[length(index(log_returns.q))]

#6)
coredata(log_returns.q)[nrow(log_returns.q)]

#7)
index(dis_returns.y)[length(index(dis_returns.y))]

#8)
coredata(dis_returns.y)[nrow(dis_returns.y)]

