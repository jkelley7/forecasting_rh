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

#2
df.path <- paste0(Sys.getenv("USERPROFILE"),"\\AnacondaProjects\\forecasting\\Chap2\\data")
tute1 <- read.csv(paste0(df.path, '\\tute1.csv'))
View(tute1)

mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)
autoplot(mytimeseries, facets=TRUE)

#3
retaildata <- readxl::read_excel(paste0(df.path,"\\retail.xlsx"), skip=1)
head(retaildata)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))

myts %>% autoplot()
myts %>% ggseasonplot()
myts %>% ggsubseriesplot
myts %>% gglagplot()
myts %>% ggAcf()

#There is definite seasonality in this data as well as an increasing trend over time

#4
help(bicoal)
myts <- ts(bicoal, frequency = 1, start = (1920))
autoplot(myts)+
  ylab('Yearly Coal Production')

# Documentation on this data series is terrible
help(chicken)
View(chicken)
myts <- ts(chicken, frequency = 1, start = (1920))
autoplot(myts)+
  ylab('Yearly Coal Production')

# 5
# Looks like there is a STRONG dip across all years in August
# Other than that it's pretty stable turnout the year
# There's also an increasing trend
writing %>% ggseasonplot()
writing %>% ggsubseriesplot()

# Jan is the bottom then each month increase sequentially with Dec being highest
# each year is higher than previous as well
fancy %>% ggseasonplot()
fancy %>% ggsubseriesplot()

# Jan is the highest, Feb is the bottom then there's a steady increase acorss the months
# each year is higher than previous as well so a definite increasing trend
a10 %>% ggseasonplot()
a10 %>% ggsubseriesplot()

# Dec/Jan are the peaks feb is the bottom then each month increase sequentially
# For the most part each year is higher than previous as well
h02 %>% ggseasonplot()
h02 %>% ggsubseriesplot()

# 6
# Seasonality, looks like March - May are the peak months with Dec/Jan being low months
# It lags with itself nicely at 1 but outside of that doesn't look so great
# Not a clear trend it's all over the place thru the years
hsales %>% autoplot()
hsales %>% ggseasonplot()
hsales %>% ggsubseriesplot()
hsales %>% gglagplot()
hsales %>% ggAcf


# There was an increasing trend until the 80's where it began to level out
# Seasonality, looks to peak in Q2/Q3 then fall in Q4 & Q1this was until the 80's when thinks go haywire
bricksq %>% autoplot()
bricksq %>% ggseasonplot()
bricksq %>% ggsubseriesplot()
bricksq %>% gglagplot()
bricksq %>% ggAcf()

# 7
# arrivals from Japan increased until the mid to late 90s until it began to experience a slow and steady decline
arrivals  %>% autoplot()
arrivals  %>% ggseasonplot()
arrivals  %>% ggsubseriesplot()

# 8
# 3-> D
# 4 -> C
# 1 -> B
# 2 -> A

usdeaths %>% autoplot()
usdeaths  %>% ggseasonplot()
usdeaths %>% ggAcf()

# 9
mypigs <- window(pigs, start=1990)
mypigs %>% autoplot()
mypigs %>% ggAcf()

# 10
# There is a slowly declining trend on the ACF Plot, there doesn't appear to be any seasonality with it
dj %>% ggAcf()
dj %>% autoplot()
ddj <- diff(dj)
# Yes, differencing by one created a white noise plot, which implies a random walk
ddj%>% ggAcf()
