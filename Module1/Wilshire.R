library(quantmod); library(ggplot2); library(dygraphs); options(scipen = 999)


fetch_data <- function(symbol, source) {
  tryCatch({
    cat("Attempting to fetch data for symbol:", symbol, "from source:", source, "\n")
    data <- getSymbols(Symbols = symbol, src = source, auto.assign = FALSE)
    data <- na.omit(data)
    return(data)
  }, error = function(e) {
    cat("Error: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

wilsh <- getSymbols("^W5000", src="yahoo",auto.assign = FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]

names(wilsh) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
head(wilsh,3)
tail(wilsh,3)

# Convert to data frame for ggplot
wilsh_df <- data.frame(Date = index(wilsh), coredata(wilsh))

# Plot the data using ggplot
ggplot(wilsh_df, aes(x = Date, y = wilsh_df$Close)) + 
  geom_line() + 
  labs(title = "Wilshire 5000 Index", x = "Date", y = "Close Value") +
  theme_minimal()

######################

load('FRED_gold')
head(gold)


first_date <- index(gold)[1]
print(first_date)

first_observation<-coredata(gold)[1]
print(first_observation)


last_date <- index(gold)[length(index(gold))]
print(last_date)

last_observation <- coredata(gold)[nrow(gold)]
print(last_observation)


logret <- diff(log(gold$TR))[-1]
head(logret,3)

ret <-exp(logret)-1

index(ret)[length(index(ret))]
coredata(ret)[nrow(ret)]

round(head(ret,3),6)


logret.w <-apply.weekly(logret,sum)
round(head(logret,3),6)
logret.m <-apply.monthly(logret,sum)

logret.q <-apply.quarterly(logret,sum)
logret.y <-apply.yearly(logret,sum)


ret.w <- exp(logret.w)-1
ret.m <- exp(logret.m)-1
ret.q <- exp(logret.q)-1
ret.y <- exp(logret.y)-1 

coredata(ret.y)[nrow(ret.y)]
