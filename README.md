# stat184_finalproject
R code for my STAT 184 final project 

library(data.table)
library(car)

weather = read.csv("weatherHistory.csv")
weather$temperature = weather$Temperature..C.
weather$humidity = weather$Humidity
weather$windspeed = weather$Wind.Speed..km.h.
weather$visbility = weather$Visibility..km.

#find difference between temp and apparent temp:
weather$difference = (weather$Temperature..C. - weather$Apparent.Temperature..C.)

#find mean difference between temp and average temp:
avg_diff = mean(weather$difference)
avg_diff

#run linear regression:
lm_weather = lm(weather$temperature~weather$humidity+weather$windspeed+weather$visbility)
summary(lm_weather)

#diagnostics on the model:
qqnorm(lm_weather$residuals)
qqline()

plot(lm_weather$fitted.values, lm_weather$residuals, xlab = "Fitted Values", ylab = "Residuals")
plot(lm_weather$residuals)

vif(lm_weather)

#overall, no collinearity problem, no issue with equivariance of errors aside from possible 
#outliers, but there is a clear issue with dependence of the errors

