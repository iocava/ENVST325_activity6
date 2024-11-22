# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity06/Deemer_GHG_Data.csv")

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

##first part: applying transformations to variables to see linear relationships

# ex. log transform methane fluxes
ghg$log.ch4 <- log(ghg$ch4+1)

# now log transformations of other variables: age, DIP, and precip
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

##second part: looking at binary categorical variables 

#look at the unique objects in the vector without duplicates
unique(ghg$Region)
#so the unique regions are Temperate, Boreal, Subtropical, and Tropical

#use ifelse function to set up a binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
#same thing to create a binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)
#also create binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)
#and lastly a binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)


##part three: running the multiple linear regression 
#step one is to create a model object 
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe
#look at the results of the model
summary(mod.full)

#step two is to check the assumptions before going all in on interpretation
#set up objects to look at the residuals 
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

#check normality using a qqplot OR the shapiro test
# qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)
#qq plot appears to be good, the residuals mostly follow the line
# shapiro-wilks test
shapiro.test(res.full)
#the p-value = 0.2817, which is greater than p = 0.05, so we accept the null hypothesis
#(i.e. the data are normally distributed)

#check the plot of the residuals 
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)
#the residuals appear to be normally distributed, have equal variance, and 
#be independent and random

#step four is to look for multicollinearity (only an issue for multiple regression)
#isolate continuous model variables into data frame:
reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)
#make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

##part four: model selection
# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 
#the lower the aic, the better the fit 
#as we add variables and we see large drops in the aic, that indicates that
#adding that variable makes the fit of the model better 
#check full model
full.step$model
#plot AIC over time
plot(full.step)

##part five: interpretations
#prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

#look at prediction with 95% confidence interval of the mean
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")


### Nov 7th class
#load in the data and look at it 
ETdat <- read.csv("/cloud/project/ETdata.csv")
unique(ETdat$crop)

#load in standard cleaning and visualization packages
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
library(PerformanceAnalytics)
library(olsrr)

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

#decompose the almond ET time series into the trend, seasonality, and random error
almond_dec <- decompose(almond_ts)

#plot decomposition
plot(almond_dec)

#calculate the autocorrelation factor (acf)
acf(na.omit(almond_ts), # remove missing data
lag.max = 24) # look at 2 years (24 months)

#partial acf
pacf.plot <- pacf(na.omit(almond_ts))

#run an autoregressive model
almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4

# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

#look at the forecast using the AR4 model
newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#conclusions: The overall trend showed that there was an initial increase in ET 
#from 2016-2017, but ET stabilized in recent years. if conditions are similar to 
#previous years, we can expect to see similar values of ET as previous years


####### HW 6

###Q1: use the CO2 transformation to design a regression analysis on the impact
#of reservoir characteristics on CO2 fluxes
ghg$trans_co2 <- 1/(ghg$co2 + 1000)

#look at which variables are available
str(ghg)
#based on that, use the following in the analysis: airTemp, log.age, log.DIP, log.precip, 
                                                  #region types

#create model
mod.CO2 <- lm(trans_co2 ~ 
                airTemp + log.age + log.DIP + log.precip + 
                BorealV + TropicalV + AlpineV, data = ghg)
summary(mod.CO2)

#step two is to check the assumptions before going all in on interpretation
#set up objects to look at the residuals 
res.CO2.full <- rstandard(mod.CO2)
fit.CO2.full <- fitted.values(mod.CO2)

#check normality using a qqplot OR the shapiro test
# qq plot
qqnorm(res.CO2.full, pch=19, col="grey50")
qqline(res.CO2.full)
#qq plot appears to be good, the residuals mostly follow the line except for at the end
# shapiro-wilks test
shapiro.test(res.CO2.full)
#the p-value is .0002695, which is less than p=.05, so we would reject the null hypothesis
#suggests the data is not normally distributed, could be because of the 
#different land types so proceed with caution

#check the plot of the residuals 
plot(fit.CO2.full,res.CO2.full, pch=19, col="grey50")
abline(h=0)
#the residuals appear to be somewhat normally distributed, have equal variance, and 
#be independent. there is some clustering, so we proceed w/ caution

#step four is to look for multicollinearity (only an issue for multiple regression)
#isolate continuous model variables into data frame:
multicol.data <- data.frame(ghg$airTemp,
                       ghg$log.age,
                       ghg$log.DIP,
                       ghg$log.precip, 
                       ghg$BorealV, 
                       ghg$TropicalV, 
                       ghg$AlpineV)
#make a correlation matrix 
chart.Correlation(multicol.data, histogram=TRUE, pch=19)

##part four: model selection
# run stepwise
CO2.step <- ols_step_forward_aic(mod.CO2)
# view table
CO2.step 
#the lower the aic, the better the fit 
#as we add variables and we see large drops in the aic, that indicates that
#adding that variable makes the fit of the model better 
#check full model
CO2.step$model
#plot AIC over time
plot(CO2.step)
#there isn't huge jumps in the steps of the model, but it seems like age, precip,
#and boreal help increase the fit of the model


###Q2: decompose the ET time series for almonds, pistachios, fallow/idle fields, corn, and 
#table grapes. evaluate diffs in the observations, trends, and seasonality of diff crops
#ALMONDS
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

almond_dec <- decompose(almond_ts)
plot(almond_dec)

#PISTACHIOS
pistachio <- ETdat %>%
  filter(crop == "Pistachios") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

pistachio_ts <- ts(pistachio$ET.in, 
                   start = c(2016,1), 
                   frequency = 12)

pistachio_dec <- decompose(pistachio_ts)
plot(pistachio_dec)

#FALLOW/IDLE FIELDS
fallow_idle <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

fallow_idle_ts <- ts(fallow_idle$ET.in, 
                   start = c(2016,1), 
                   frequency = 12)

fallow_idle_dec <- decompose(fallow_idle_ts)
plot(fallow_idle_dec)

#CORN
corn <- ETdat %>%
  filter(crop == "Corn") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

corn_ts <- ts(corn$ET.in, 
                   start = c(2016,1), 
                   frequency = 12)

corn_dec <- decompose(corn_ts)
plot(corn_dec)

#TABLE GRAPES
table_grapes <- ETdat %>%
  filter(crop == "Grapes (Table/Raisin)") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

table_grapes_ts <- ts(table_grapes$ET.in, 
                   start = c(2016,1), 
                   frequency = 12)

table_grapes_dec <- decompose(table_grapes_ts)
plot(table_grapes_dec)



###Q3: design an autoregressive model for pistachios and fallow/idle fields
#forecast future ET for each field 

#PISTACHIOS
pistachio_y <- na.omit(pistachio_ts)
pistachio_model1 <- arima(pistachio_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
pistachio_model1

pist_fit1 <- pistachio_y - residuals(pistachio_model1)

pistachio_model3 <- arima(pistachio_y, 
                          order = c(3,0,0))
pistachio_model3

pist_fit3 <- pistachio_y - residuals(pistachio_model3)

plot(pistachio_y)
points(pist_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(pist_fit3, type = "l", col = "steelblue", lty = 2, lwd=2)

future_pist <- forecast(pistachio_model3)

#make dataframe for plotting
Pistachio_F <- data.frame(future_pist)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
Pistachio_F$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = pistachio, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachio$date[1]),Pistachio_F$dateF[24])+  # Plotting original data
  geom_line(data = Pistachio_F, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=Pistachio_F, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(title = "Pistachios", x="year", y="Evapotranspiration (in)")

#FALLOW/IDLE FIELDS
fallow_y <- na.omit(fallow_idle_ts)
fallow_model1 <- arima(fallow_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
fallow_model1

fallow_fit1 <- fallow_y - residuals(fallow_model1)

fallow_model2 <- arima(fallow_y, 
                       order = c(4,0,0))
fallow_model2

fallow_fit2 <- fallow_y - residuals(fallow_model2)

plot(fallow_y)
points(fallow_fit1, type = "l", col = "purple", lty = 2, lwd=2)
points(fallow_fit2, type = 'l', col = 'darkgreen', lty = 2, lwd =2)

future_fallow <- forecast(fallow_model2)

#make dataframe for plotting
Fallow_F <- data.frame(future_fallow)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
Fallow_F$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = fallow_idle, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallow_idle$date[1]),Fallow_F$dateF[24])+  # Plotting original data
  geom_line(data = Fallow_F, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=Fallow_F, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(title = "Fallow/Idle Fields", x="year", y="Evapotranspiration (in)")





