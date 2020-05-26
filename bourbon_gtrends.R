##########################################################
#
#
#
# Bourbon Google Trends
#
#
#
##########################################################
# Let's look at some bourbon search Data
df.path <- paste0(Sys.getenv("USERPROFILE"),"\\AnacondaProjects\\forecasting\\data")
df <- read.csv(paste0(df.path, '\\bourbon.csv'))
dfb <- ts(df$bourbon, start = c(2004,1), frequency = 12)


# The ACF plot shows a slow decreasing trend which a rythmic pattern, 
# this is indicitive of an increasing trend and seasonality
##########################################################
# ACF
##########################################################
ggAcf(dfb)

# Look at the plot
autoplot(dfb)

# Fit an ETS Model and look at results
fit <- ets(dfb, restrict = F)
# Very small beta means the trend hardly changes with time
# there's effectively 0 gamma which means the seasonality hardly changes with time
# PHI is super high which while it is using damping it's actuing more like 
# the holt winters method. THe fact that it's .9799, effectively at its maximum
# restriction tells us that it's pretty linear in nature
summary(fit)


autoplot(fit)
cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

##########################################################
# Forecast 1 year in the future
##########################################################
# Looks like impact from Covid is relatively significant
# Peak demand in Nov/Dec Time frame is projected to be similar to 2017
fit %>% forecast(h=12) %>%
  autoplot() +
  ylab("Bourbon Search Trends via Google") +
  ggtitle('Bourbon Search Trends via Google Trends')