library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(scales)
library(forecast)
library(zoo)
library(tseries)
library(corrplot)
library(e1071)
library(pander)
library(ggplot2)
library(MLmetrics)
library(psych)
library(rpart.plot)
library(car)
library(tidyverse)
library(MASS)
library(fpp)
library(fpp2)


day <-read.csv("/Users/ash/Downloads/AL1/bike-sharing-dataset.csv")
day$dteday <- as.Date(day$dteday, "%Y-%m-%d")
day<- day%>% 
mutate(date = ymd(dteday)) %>% 
mutate_at(vars(date), funs(year, month, day))
head(day)


# Missing values
is.na(day)
print(paste("The total number of missing data are",sum(is.na(day))))

summary(day)

#Seasonal Ridership
library(RColorBrewer)
library(relaimpo)
col <- brewer.pal(4,"Set3")
ggplot(day,aes(season,cnt)) +geom_boxplot(fill = col) +theme_classic() +
labs(title = "Boxplot of rental bikes per season") +
scale_x_discrete(labels = c("Spring","Summar","Fall","Winter"))


## Year wise casual and registered ridership
Year_count<- day%>% 
  select(year,casual,registered)
df <- Year_count%>%
  group_by(year) %>%
  summarise(casual_ridership=sum(casual),
            registered_ridership = sum(registered))
df <-as.data.frame(df)
df$year <- as.character(df$year)

dfm <- melt(df[,c('year','casual_ridership','registered_ridership')],id.vars = 1)
point <- format_format(big.mark = ",", scientific = FALSE)
ggplot(dfm,aes(x = year,y = value)) + labs(title="Yearly casual and registered ridership ")+
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_continuous(labels = point)

## Ridership based on Working nd holiday
mnth_count<- df %>% 
  select(mnth,workingday,cnt)
df <- mnth_count%>%
  group_by(mnth,workingday) %>%
  summarise(cnt = sum(cnt))
df$mnth <- as.factor(df$mnth)
df$workingday <- as.character(df$workingday)
point <- format_format(big.mark = ",", scientific = FALSE)
ggplot(df, aes(mnth,cnt)) + labs(title="Monthly ridership based on working and holiday")+
  geom_line(aes(color=workingday, group=workingday))+ scale_y_continuous(labels = point)


## Ridership based on weather
weather_count<- day %>% 
  select(mnth,weathersit,cnt)
weather_df <- weather_count%>%
  group_by(mnth,weathersit) %>%
  summarise(cnt = sum(cnt))
weather_df$month <- as.factor(weather_df$month)
weather_df$weathersit <- as.character(weather_df$weathersit)
point <- format_format(big.mark = ",", scientific = FALSE)
ggplot(weather_df, aes(mnth,cnt)) + labs(title="Monthly ridership based on weather")+
  geom_line(aes(color=weathersit, group=weathersit))+scale_y_continuous(labels = point)

## Registered ridership
day%>%
  mutate(weathersit= factor(weathersit))%>% 
  ggplot(aes(y=registered , x=weathersit, fill=weathersit))+
  geom_boxplot(colour="black")+labs(title="Registered riderships")+ scale_fill_discrete(name="Weather type",
                                                                                       labels=c("Clear",
                                                                                                "Mist", "Light Snow", "Heavy Rain"))
## Casual ridership
day%>% 
  mutate(weathersit= factor(weathersit))%>% 
  ggplot(aes(y=casual , x=weathersit, fill=weathersit))+
  geom_boxplot(colour="black")+labs(title="Casual riderships")+scale_fill_discrete(name="Weather type",
  
  #edit one
  
  #CORELATION MATRIX
  correlation <- mutate_all(day, function(x) as.numeric(as.character(x)) )
  df<- cor(day[,3:16] )
  corrplot(cor(df), method = 'square')
  
  1. #Simple linear regression 
  plot(jitter(cnt) ~ jitter(temp),xlab="temp (mpg)",ylab="cnt",data=day) 
  fit= lm (cnt~temp, data= day)
  abline(fit)
  
  
  # Plotting residuals 
  res <- residuals(fit)
  plot(jitter(res)~jitter(temp), ylab="Residuals", xlab="temp", data=day) 
  abline(0,0)
  # Checking Rsquare 
  summary(fit)
  
  
  2. # Multiple regression 
  library(MASS)
  A0=lm(cnt~ temp+ weathersit + yr + mnth ,data=day)
  stepAIC(A0 , direction = "backward") # Show this by taking pic of its use
  
  # Show efficieny with this code. 
  mod2 = lm(cnt ~  temp+ weathersit + yr + mnth,data = day) 
  mod2
  summary(mod2)  # Not able to get the plot show only r square
  plot(mod2)
  abline(mod2) # Unable to plot and residuals also. What to show with this? 
  
  #Testing with the LR model. 
  newdata <- day %>% select_("mnth","weathersit","temp","yr")
  predictions <- predict(mod2,newdata = newdata)
  head(predictions)
  
  #Plotting with the model and observing
  plot(day$cnt[1:365],type='l',col="blue",main = "Observed Vs Predicted",ylab = "Daily Demand")
  lines(predictions[1:365],col="red")
  legend(0, 6000, legend=c("Observed", "Predicted"),col=c("blue", "red"),lty = 1,box.lty = 0)
  grid()
  
  
                                                                                    
                                                                                                                                                                  labels=c("Clear",
                                                                                           "Mist", "Light Snow", "Heavy Rain"))


#Time series analysis

data_ts <-msts(day[,'cnt'], seasonal.periods=c(7))
train_ts <- head(data_ts, round(length(data_ts) * 0.9))
test_ts <- tail(data_ts, round(length(data_ts) * 0.1))
autoplot(data_ts)+
  autolayer(train_ts) +
  autolayer(test_ts) +
  xlab('Weeks')+ ylab('Bike riders')+ guides(colour=guide_legend(title=""))


# Decomposition
plot(decompose(train_ts, type='add'), xlab="Weeks")

#Test for stationery 
adf_test <- adf.test(train_ts, alternative='stationary')
library(urca)
Test=ur.kpss(train_ts) 
summary(Test) # 

# Now applying differencing. 
ndiffs(train_ts)  # 1 differencing needed. 
nsdiffs(train_ts) #Seasonal Differencing not allowed.

# Applying ARIMA Model (p,d,q) DIfferencing is 1. 
auto.arima(train_ts)
fit1 <- Arima(train_ts, order=c(7,1,0),seasonal=c(6,1,0),
              method = "CSS", optim.method = "BFGS")
fit1  
#The RMSE for your training and your test sets should be very similar if you have built a good model.
#If the RMSE for the test set is much higher than that of the training set, it is likely that you've badly over fit the data,
#i.e. you've created a model that tests well in sample, but has little predictive value when tested out of sample.
forecast_ts <- forecast(fit1,h= length(test_ts))
autoplot(forecast_ts, xlab="Weeks", ylab= "Bike Riders") + autolayer(test_ts)
accuracy(forecast_ts, test_ts)


# ACF and PACF
acf_ts <- acf(train_ts[1:length(train_ts)], plot = FALSE)
plot(acf_ts,  main = "Autocorrelation function")

pacf_ts <- pacf(train_ts[1:length(train_ts)], plot = FALSE)
plot(pacf_ts,  main = " Partial autocorrelation function")
print(adf_test)


# ACF and PACF for non seasonal series the relatiopn hgets reduced after differencing.
acf_ts <- acf(stat_diff[1:length(stat_diff)], plot = FALSE)
plot(acf_ts,  main = "Autocorrelation function")

pacf_ts <- pacf(stat_diff[1:length(stat_diff)], plot = FALSE)
plot(pacf_ts,  main = " Partial autocorrelation function")


