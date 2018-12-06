# stat184_finalproject
R code for my STAT 184 final project 

library(data.table)
library(car)
library(ggplot2)

weather = read.csv("weatherHistory.csv")
weather$temperature = weather$Temperature..C.
weather$humidity = weather$Humidity
weather$windspeed = weather$Wind.Speed..km.h.
weather$visbility = weather$Visibility..km.
weather$pressure = weather$Pressure..millibars.

#find difference between temp and apparent temp:
weather$difference = (weather$Temperature..C. - weather$Apparent.Temperature..C.)


#find mean difference between temp and average temp:
avg_diff = mean(weather$difference)
avg_diff

#run linear regression:
lm_weather = lm(weather$temperature~weather$humidity+weather$windspeed+weather$visbility + weather$pressure)
summary(lm_weather)

#diagnostics on the model:
qqnorm(lm_weather$residuals)
qqline()

plot(lm_weather)

vif(lm_weather)

#pick the best model using best subset selection:
require(leaps)
subset=regsubsets(temperature~humidity+windspeed+visbility+pressure,data=weather)
sum_subset=summary(subset)
sum_subset$which
sum_subset$rss

#adj R-squared
p_full = 5
p=2:p_full
RSS_p = sum_subset$rss
totalSS = sum((weather$temperature-mean(weather$temperature))^2)
n=nrow(weather)
R2_adj=1-(RSS_p/(n-p))/(totalSS/(n-1))
plot(p,R2_adj,xlab="Number of betas",ylab="Adjusted R-squared")
R2_adj

#mallows cp
sigma_hat_full=summary(lm_weather)$sigma
sigma_hat_full
C_p=RSS_p/(sigma_hat_full^2)+2*p-n
C_p
plot(p,C_p,xlab="Number of betas",ylab="Mallow's Cp")
abline(0,p)

#AIC
aic_p=n*log(RSS_p/n)+2*p
aic_p
plot(p,aic_p,xlab="Number of betas",ylab="AIC")

#BIC
bic_p=n*log(RSS_p/n)+p*log(n)
bic_p
plot(p,bic_p,xlab="Number of betas",ylab="BIC")

cbind(sum_subset$which,R2_adj,C_p,aic_p,bic_p)


#overall, no collinearity problem, no issue with equivariance of errors aside from possible 
#outliers, but there is a clear issue with dependence of the errors

#try a smaller sample of data, might be an overfitting probelm

weather_small = weather[sample(1:nrow(weather), 1000, replace=FALSE),]

lm_small = lm(weather_small$temperature~weather_small$humidity+weather_small$windspeed+weather_small$visbility + weather_small$pressure)
summary(lm_small)

#diagnostics on the model:
plot(lm_small)

vif(lm_small)

#adj R-squaredrequire(leaps)
subset=regsubsets(temperature~humidity+windspeed+visbility+pressure,data=weather_small)
sum_subset=summary(subset)
sum_subset$which
sum_subset$rss

ws = weather_small$temperature
p_full = 5
p=2:p_full
RSS_p = sum_subset$rss
totalSS = sum(((ws)-mean(ws))^2)
n=1000
R2_adj=1-((RSS_p/(n-p))/(totalSS/(n-1)))
plot(p,R2_adj,xlab="Number of betas",ylab="Adjusted R-squared")
R2_adj

#mallows cp
sigma_hat_full=summary(lm_small)$sigma
sigma_hat_full
C_p=RSS_p/(sigma_hat_full^2)+2*p-n
C_p
plot(p,C_p,xlab="Number of betas",ylab="Mallow's Cp")
abline(0,p)

#AIC
aic_p=n*log(RSS_p/n)+2*p
aic_p
plot(p,aic_p,xlab="Number of betas",ylab="AIC")

#BIC
bic_p=n*log(RSS_p/n)+p*log(n)
bic_p
plot(p,bic_p,xlab="Number of betas",ylab="BIC")

cbind(sum_subset$which,R2_adj,C_p,aic_p,bic_p)

#graph 3 kept predictors (humidity, windspeed, visibility) individually 

ggplot(weather_small, aes(humidity)) + geom_density()
ggplot(weather_small, aes(windspeed))+ geom_density()
ggplot(weather_small, aes(visbility)) + geom_density()

#graph 3 kept predictors (humidity, windspeed, visibility) individually against temp
ggplot(weather_small, aes(humidity, temperature)) + geom_line(col = "blue") + labs(x="Humidity", y="Temperature (Cº)",title ="Humidity vs Temperature")
ggplot(weather_small, aes(windspeed, temperature)) + geom_line(col = "magenta") + labs(x="Wind Speed (km/hr)", y="Temperature (Cº)", title = "Wind Speed vs Temperature")
ggplot(weather_small, aes(visbility, temperature)) + geom_line(col = "lime green") + labs(x="Visibility (km)", y="Temperature (Cº)", title = "Visibility vs Temperature")


