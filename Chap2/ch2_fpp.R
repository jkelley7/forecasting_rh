# Chapter 2 FOrecasting: Principles and Practice
library(fpp2)

# 2.1
y <- ts(c(123,39,78,52,110), start=2012)

# 2.2
autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

# 2.6 ScatterPlots
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

autoplot(visnights[,1:5], facets=TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

GGally::ggpairs(as.data.frame(visnights[,1:5]))

# 2.7 Lag Plots
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

# 2.8 Autocorrelation
ggAcf(beer2)

# 2.9
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
ggAcf(y)

##########################################################
# Exercises
##########################################################
#1a.
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)
#1b.
frequency(gas)
# 1c.
gold[which.max(gold)]




