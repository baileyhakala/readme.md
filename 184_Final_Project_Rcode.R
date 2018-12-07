library(data.table)
library(car)
library(ggplot2)
library(lubridate)

weather = read.csv("weatherHistory.csv")
weather$temperature = weather$Temperature..C.
weather$humidity = weather$Humidity
weather$windspeed = weather$Wind.Speed..km.h.
weather$visbility = weather$Visibility..km.
weather$pressure = weather$Pressure..millibars.

#create year column:
weather$year <- year(weather$Formatted.Date)

avg_yearly_temp <- dcast(weather, year ~., mean, value.var = c("temperature"))
setnames(avg_yearly_temp,".","Avg_Temp_for_Year")

ggplot(avg_yearly_temp, aes(year, Avg_Temp_for_Year)) + geom_col(fill="sky blue") + labs(x="Year", y="Average Temp for the Year", title = "Average Yearly Temperature")
ggsave("Average Yearly Temperature.png", width = 5, height = 5)

#find difference between temp and apparent temp:
weather$difference = (weather$Temperature..C. - weather$Apparent.Temperature..C.)

#find mean difference between temp and average temp:
avg_diff = mean(weather$difference)
avg_diff

#run linear regression:
lm_weather = lm(weather$temperature~weather$humidity+weather$windspeed+weather$visbility + weather$pressure)
summary(lm_weather)

#diagnostics on the model:
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

avg_yearly_temp_small <- dcast(weather_small, year ~., mean, value.var = c("temperature"))
setnames(avg_yearly_temp_small,".","Avg_Temp_for_Year_Small")
ggplot(avg_yearly_temp_small, aes(year, Avg_Temp_for_Year_Small)) + geom_col(fill="orange")
ggsave("Average Yearly Temperature for Sample.png", width = 5, height = 5)

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

ggplot(weather_small, aes(humidity)) + geom_density(col = "blue") + labs(x="Humidity", title = "Density of Humidity")
ggsave("HumidityDensity.png", width = 5, height = 5)
ggplot(weather_small, aes(windspeed))+ geom_density(col = "magenta") + labs(x="Wind Speed (km/hr)", title = "Density of Wind Speed")
ggsave("WindSpeedDensity.png", width = 5, height = 5)
ggplot(weather_small, aes(visbility)) + geom_density(col = "lime green") + labs(x="Visibility (km)", title = "Density of Visibility")
ggsave("VisibilityDensity.png", width = 5, height = 5)

#graph 3 kept predictors (humidity, windspeed, visibility) individually against temp
ggplot(weather_small, aes(humidity, temperature)) + geom_line(col = "blue") + labs(x="Humidity", y="Temperature (Cº)",title ="Humidity vs Temperature")
ggsave("Humidity vs Temperature.png", width = 5, height = 5)
ggplot(weather_small, aes(windspeed, temperature)) + geom_line(col = "magenta") + labs(x="Wind Speed (km/hr)", y="Temperature (Cº)", title = "Wind Speed vs Temperature")
ggsave("Wind Speed vs Temperature.png", width = 5, height = 5)
ggplot(weather_small, aes(visbility, temperature)) + geom_line(col = "lime green") + labs(x="Visibility (km)", y="Temperature (Cº)", title = "Visibility vs Temperature")
ggsave("Visibility vs Temperature.png", width = 5, height = 5)

#create variables from weather_small:
avg_temp = mean(weather_small$temperature)
avg_humid = mean(weather_small$humidity)
avg_vis = mean(weather_small$visbility)

diff_temp = data.table(weather_small$temperature - avg_temp)
diff_humid = data.table(weather_small$humidity - avg_humid)
diff_vis = data.table(weather_small$visbility - avg_vis)

avg_temp_daily <- dcast(weather_small, Daily.Summary ~., mean, value.var = c("temperature"))
avg_humid_daily <- dcast(weather_small, Daily.Summary ~., mean, value.var = c("humidity"))
avg_vis_daily <- dcast(weather_small, Daily.Summary ~., mean, value.var = c("visbility"))

avg_temp_daily <- data.table(avg_temp_daily)
avg_humid_daily <- data.table(avg_humid_daily)
avg_vis_daily <- data.table(avg_vis_daily)

setnames(avg_temp_daily,".","Avg Temperature (Cº) by Type of Day")
setnames(avg_humid_daily,".","Avg Humidity by Type of Day")
setnames(avg_vis_daily,".","Avg Visibility by Type of Day")

setkey(avg_temp_daily, Daily.Summary)
setkey(avg_humid_daily, Daily.Summary)
setkey(avg_vis_daily, Daily.Summary)

#merge into a new tidy table
Avgs_for_Type_of_Day <- merge(avg_temp_daily, avg_humid_daily, all.x=T)
Avgs_for_Type_of_Day <- merge(Avgs_for_Type_of_Day, avg_vis_daily, all.x=T)

fwrite(Avgs_for_Type_of_Day, "Avgerages_for_type_of_day.csv")
