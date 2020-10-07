library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(scales)
library(zoo)
library(tseries)
library(corrplot)
library(e1071)
library(pander)
library(psych)
library(rpart.plot)
library(car)
library(tidyverse)
library(MASS)
library(fpp)
library(fpp2)
library(ggplot2)
library(MLmetrics)
library(forecast)

day <-read.csv("/Users/ash/Downloads/AL1/bike-sharing-dataset/day.csv")
day$dteday <- as.Date(day$dteday, "%Y-%m-%d")
day<- day%>% 
  mutate(date = ymd(dteday)) %>% 
  mutate_at(vars(date), funs(year, month, day))
head(day)

# Various Parameters about the data
names(day)

# Missing values
is.na(day)
print(paste("The total number of missing data are",sum(is.na(day))))

summary(day)


#Seasonal Ridership
season_count<- day %>%select(season,cnt)
point <- format_format(big.mark = ",", scientific = FALSE)
ggplot(season_count, aes(season, cnt))+ geom_bar(stat = "identity", fill="coral1") + labs(title="Seasonal ridership")+
  scale_y_continuous(labels = point)

## Year wise casual and registered ridership
Year_count<- day%>% 
  select(year,casual,registered)
day <- Year_count%>%
  group_by(year) %>%
  summarise(casual_ridership=sum(casual),
            registered_ridership = sum(registered))
day <-as.data.frame(day)
day$year <- as.character(day$year)

dfm <- melt(day[,c('year','casual_ridership','registered_ridership')],id.vars = 1)
point <- format_format(big.mark = ",", scientific = FALSE)
ggplot(dfm,aes(x = year,y = value)) + labs(title="Yearly casual and registered ridership ")+
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_continuous(labels = point)

## Ridership based on Working nd holiday
mnth_count<- day %>% 
  select(mnth,workingday,cnt)
day <- mnth_count%>%
  group_by(mnth,workingday) %>%
  summarise(cnt = sum(cnt))
day$mnth <- as.factor(day$mnth)
day$workingday <- as.character(day$workingday)
point <- format_format(big.mark = ",", scientific = FALSE)
ggplot(day, aes(mnth,cnt)) + labs(title="Monthly ridership based on working and holiday")+
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
  labels=c("Clear", "Mist", "Light Snow", "Heavy Rain"))

## Casual ridership
day%>% 
  mutate(weathersit= factor(weathersit))%>% 
  ggplot(aes(y=casual , x=weathersit, fill=weathersit))+
  geom_boxplot(colour="black")+labs(title="Casual ridership")+scale_fill_discrete(name="Weather type",
  labels=c("Clear","Mist", "Light Snow", "Heavy Rain"))
                                                          

#CORELATION MATRIX
correlation <- mutate_all(day, function(x) as.numeric(as.character(x)))
df<- cor(day[,3:16])
corrplot(cor(df), method = 'square')                         

1. #Simple linear regression 
plot(jitter(cnt) ~ jitter(temp),
     xlab="temp (mpg)",ylab="cnt",data=day) 
fit= lm (cnt~temp, data= day)
abline(fit)

# Plotting residuals 
res <- residuals(fit)
plot(jitter(res)~jitter(temp), 
     ylab="Residuals",xlab="temp",data=day) 
abline(0,0)

# Checking Rsquare 
summary(fit)


2. # Multiple regression 
library(MASS)
A0=lm(cnt~ temp+ weathersit + yr + mnth ,data=day)
stepAIC(A0 , direction = "backward") 

# Show efficieny with this code. 
mod2 = lm(cnt ~  temp+ weathersit + yr + mnth,data = day) 
mod2
summary(mod2)  

#Testing with the LR model. 
newdata <- day %>% select_("mnth","weathersit","temp","yr")
predictions <- predict(mod2,newdata = newdata)
head(predictions)

#Plotting with the model and observing
plot(day$cnt[1:365],type='l',col="blue",main = "Observed Vs Predicted",ylab = "Daily Demand")
lines(predictions[1:365],col="red")
legend(0, 6000, legend=c("Observed", "Predicted"),col=c("blue", "red"),lty = 1,box.lty = 0)
grid()


3.#Time series analysis

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

# Applying ARIMA Model (p,d,q) DIfferencing is 1. 
auto.arima(train_ts)
fit1 <- Arima(train_ts, order=c(7,1,0),seasonal=c(6,1,0),
              method = "CSS", optim.method = "BFGS")
fit1  

#Plotting
forecast_ts <- forecast(fit1,h= length(test_ts))
autoplot(forecast_ts, xlab="Weeks", ylab= "Bike Riders") + autolayer(test_ts)
#Accuracy
accuracy(forecast_ts, test_ts)






