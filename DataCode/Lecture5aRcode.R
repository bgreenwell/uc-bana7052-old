rm(list=ls())
# read data
delivery <- read.csv("eg3.1.delivery.csv",h=T)
names(delivery)
cor(delivery)
# visualize data
pairs (delivery,pch=20)
# fit linear regression
model1 <- lm(DeliveryTime ~ NumberofCases+Distance, data=delivery)
summary(model1)
# obtain coefficient estimate
model1$coefficients
# obtain residuals
model1$residuals
# obtain fitted value
model1$fitted.values
# check fitted+residual==original
delivery$DeliveryTime == model1$fitted.values+model1$residuals

d=read.csv("AirlinesCost.csv")
names(d)=c(
  "Airline",
  "LengthFlight",
  "Speed",
  "DailyFlight",
  "PopulationServed",
  "Cost",
  "Revenue",
  "Ton",
  "Capacity",
  "Assets",
  "Investments",
  "AdjustedAssets"
)
model3=lm(Cost~LengthFlight+Speed+DailyFlight+PopulationServed+
     Revenue+Ton+Capacity+Assets+Investments,data=d)
model4=lm(Cost~Revenue+Ton+Capacity+Assets+Investments,data=d)
anova(model3,model4)
attach(d)
plot(LengthFlight, Cost)
plot(Speed, Cost)
plot(DailyFlight, Cost)
plot(PopulationServed, Cost)
plot(Cost, Cost)
plot(Revenue, Cost)
plot(Ton, Cost)
plot(Capacity, Cost)
plot(Assets, Cost)
plot(Investments, Cost)
plot(AdjustedAssets, Cost)
summary(model4)
library(car)
vif(model4)
model
model=lm(Cost~LengthFlight+Speed+DailyFlight+PopulationServed+Ton+Capacity+Assets+Investments,data=d)
summary(model)
library(car)
vif(model)


d=read.csv("PGA.csv")
names(d)
attach(d)
model=lm(AverageWinnings~Age+AverageDrive+DrivingAccuracy+GreensonRegulation+AverageNumofPutts+SavePercent+NumEvents, data=d)
summary(model)
plot(Age,AverageWinnings)
plot(AverageDrive,AverageWinnings)
plot(DrivingAccuracy,AverageWinnings)
plot(GreensonRegulation,AverageWinnings)
plot(AverageNumofPutts,AverageWinnings)
plot(SavePercent,AverageWinnings)
plot(MoneyRank,AverageWinnings)
plot(NumEvents,AverageWinnings)
plot(TotalWinnings,AverageWinnings)

PGA=read.csv("PGA.csv")
pairs(PGA)
names(PGA)
model3=lm(AverageWinnings~AverageDrive+
            DrivingAccuracy+
            GreensonRegulation+
            AverageNumofPutts+
            SavePercent+
            MoneyRank+
            NumEvents,data=PGA)
summary(model3)
model4=lm(AverageWinnings~
            DrivingAccuracy+
            GreensonRegulation+
            MoneyRank+
            NumEvents,data=PGA)
anova(model3,model4)

