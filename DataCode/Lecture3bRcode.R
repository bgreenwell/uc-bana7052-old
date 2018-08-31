rm(list=ls())
Paper <- read.csv("data-ex-7-1-(Hardwood).csv",h=T)
plot(Paper$HwdCon,Paper$TsStr)
# fit polynomial regression with order 2 note the function I()
model1 <- lm(Paper$TsStr ~ Paper$HwdCon+ I(Paper$HwdCon^2) ) 
summary(model1)
library(car)
vif(model1)
cor(Paper$HwdCon, Paper$HwdCon^2)
plot(Paper$HwdCon, Paper$HwdCon^2)
# plot data
plot(Paper$HwdCon,Paper$TsStr)
curve(-6.6+11.76*x-0.63*x^2,add=TRUE)
# plot the fitted values using dots
points(Paper$HwdCon,model1$fitted.values,pch=20)

# plot the fitted values using a line
points(Paper$HwdCon,model1$fitted.values,type="l")

# center the observations
Paper$HwdCon_Centered=Paper$HwdCon-mean(Paper$HwdCon)
model2 <- lm(Paper$TsStr ~ Paper$HwdCon_Centered+ I(Paper$HwdCon_Centered^2) ) 
summary(model2)
library(car)
vif(model2)
cor(Paper$HwdCon_Centered, Paper$HwdCon_Centered^2)
plot(Paper$HwdCon_Centered, Paper$HwdCon_Centered^2)

# example 7.2
Voltage <- read.csv("data-ex-7-2-(Voltage-Drop).csv",h=T)
# visualize data
plot(Voltage$Time,Voltage$VoltageDrop)
# build polynomial regression
model3=lm(Voltage$VoltageDrop~Voltage$Time+I(Voltage$Time^2))
# plot fitted values
par(mfrow=c(1,2))
plot(Voltage$Time,Voltage$VoltageDrop)
points(Voltage$Time,model3$fitted.values,pch=20)
points(Voltage$Time,model3$fitted.values,type="l")
plot(Voltage$Time,model3$residuals)
abline(h=0,col="grey")
# build polynomial regression
model3=lm(Voltage$VoltageDrop~Voltage$Time+I(Voltage$Time^2)+I(Voltage$Time^3))
par(mfrow=c(1,2))
plot(Voltage$Time,Voltage$VoltageDrop)
points(Voltage$Time,model3$fitted.values,pch=20)
points(Voltage$Time,model3$fitted.values,type="l")
plot(Voltage$Time,model3$residuals)
abline(h=0,col="grey")
# build polynomial regression
model3=lm(Voltage$VoltageDrop~Voltage$Time+I(Voltage$Time^2)+I(Voltage$Time^3)+I(Voltage$Time^4))
par(mfrow=c(1,2))
plot(Voltage$Time,Voltage$VoltageDrop)
points(Voltage$Time,model3$fitted.values,pch=20)
points(Voltage$Time,model3$fitted.values,type="l")
plot(Voltage$Time,model3$residuals)
abline(h=0,col="grey")





# example 7.2
Voltage <- read.csv("data-ex-7-2-(Voltage-Drop).csv",h=T)
# visualize data
plot(Voltage$Time,Voltage$VoltageDrop)
# build polynomial regression
model3=lm(Voltage$VoltageDrop~Voltage$Time+I(Voltage$Time^2))
# plot fitted values
points(Voltage$Time,model3$fitted.values,pch=20)
points(Voltage$Time,model3$fitted.values,type="l")
# residual plot
plot(model3$fitted.values,model3$residuals)
abline(h=0)




library(splines)
# quadratic term
Voltage$x2 <- Voltage$Time^2
# qubic term
Voltage$x3 <- Voltage$Time^3
# knot at 6.5
Voltage$x65 <- ifelse ((Voltage$Time>6.5), (Voltage$Time-6.5)^3, 0)
# knot at 13
Voltage$x13 <- ifelse ((Voltage$Time>13), (Voltage$Time-13)^3, 0)
# fit spline regression
model4 <- lm(Voltage$VoltageDrop ~ Voltage$Time+Voltage$x2+Voltage$x3+Voltage$x65+Voltage$x13)
summary(model4)

# check the fit
par(mfrow=c(1,2))
plot(Voltage$Time,Voltage$VoltageDrop)
points(Voltage$Time,model4$fitted.values,pch=20)
points(Voltage$Time,model4$fitted.values,type="l")
# residual plot
plot(model4$fitted.values,model4$residuals)
abline(h=0)




