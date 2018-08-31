# example 5.1
rm(list=ls())
# read data
Energy=read.csv("data-ex-5-1-(Electric-Utility).csv")
# visualize data
plot(Energy$Usage,Energy$Demand,pch=20)
# fit regression
model1=lm(Energy$Demand~Energy$Usage)
# residual plots based on x
plot(Energy$Usage,model1$residuals)
abline(h=0)
# residual plots based on fitted values
plot(model1$fitted.values,model1$residuals)


# example 5.1
# example 5.1
rm(list=ls())
# read data
Energy=read.csv("data-ex-5-1-(Electric-Utility).csv")
# visualize data
plot(Energy$Usage,Energy$Demand,pch=20)
# fit regression
model1=lm(Energy$Demand~Energy$Usage)
# residual plots based on x
plot(Energy$Usage,model1$residuals)
# residual plots based on fitted values
plot(model1$fitted.values,model1$residuals)
# residual plots based using R student
plot(model1$fitted.values,rstudent(model1))



# example 5.2
Wind=read.csv("data-ex-5-2-(Windmill).csv")
# visualize data
plot(Wind$Velocity,Wind$Output,pch=20)
model2=lm(Wind$Output~Wind$Velocity)
summary(model2)
abline(model2)
# residual plots based on fitted values
plot(model2$fitted.values,model2$residuals)
abline(h=0,col="grey",lwd=3)
# residual plots based on R student
plot(model2$fitted.values,rstudent(model2))
abline(h=0)

# transform velocity
Wind$InvVelo=1/Wind$Velocity
model3=lm(Wind$Output~Wind$InvVelo)
summary(model3)
# residual plots based on fitted values
plot(model3$fitted.values,model3$residuals)
abline(h=0,col="grey",lwd=3)
# residual plots based on R student
plot(model3$fitted.values,rstudent(model3))
abline(h=0,col="grey",lwd=3)

plot(Wind$InvVelo,Wind$Output)
abline(model3)

plot(Wind$Velocity,Wind$Output)
points(Wind$Velocity,model3$fitted.values,pch=20,col="red")




require(MASS)
boxcox(model1)
boxcox(model2)
boxcox(model3)


# weight lm
Food=read.csv("table5.9.csv")
pairs(Food)
plot(Food$AdvertisingExpense,Food$Income)
# build linear regression without weights
model_noweight=lm(Food$Income~Food$AdvertisingExpense)
# check the coefficient
summary(model_noweight)
# generate residual plot
plot(Food$AdvertisingExpense,model_noweight$residuals)
abline(h=0)
# regress residual^2 on covariate
weight_coef=lm( model_noweight$residuals^2~Food$AdvertisingExpense )$coef
# build linear regression with weights
model_weight=lm(Food$Income~Food$AdvertisingExpense,weights=1/(weight_coef[1]+Food$AdvertisingExpense*weight_coef[2]))
# check coefficients
summary(model_weight)
# generate residual plot with weighted residuals
plot(Food$AdvertisingExpense,
     1/sqrt(weight_coef[1]+Food$AdvertisingExpense*weight_coef[2]) * model_weight$residuals)
abline(h=0,col="grey",lwd=3)















rm(list=ls())
Energy=read.csv("data-ex-5-1-(Electric-Utility).csv")
plot(Energy$Usage,Energy$Demand,pch=20)
model1=lm(Energy$Demand~Energy$Usage)
# residual plots based on x
plot(Energy$Usage,model1$residuals)
# residual plots based on fitted values
plot(model1$fitted.values,model1$residuals)
# residual plots based on R student
plot(model1$fitted.values,rstudent(model1))

# example 5.2
Wind=read.csv("data-ex-5-2-(Windmill).csv")
plot(Wind$Velocity,Wind$Output,pch=20)
model2=lm(Wind$Output~Wind$Velocity)
summary(model2)
# residual plots based on fitted values
plot(model2$fitted.values,model2$residuals)
abline(h=0,col="grey",lwd=3)
# residual plots based on R student
plot(model2$fitted.values,rstudent(model2))

# transform velocity
Wind$InvVelo=1/Wind$Velocity
model3=lm(Wind$Output~Wind$InvVelo)
summary(model3)
# residual plots based on fitted values
plot(model3$fitted.values,model3$residuals)
abline(h=0,col="grey",lwd=3)
# residual plots based on R student
plot(model3$fitted.values,rstudent(model3))

# Box-Cox
require(MASS)
boxcox(model1)
boxcox(model2)
boxcox(model3)

# weight lm
Food=read.csv("table5.9.csv")
plot(Food$AdvertisingExpense,Food$Income)
model_noweight=lm(Food$Income~Food$AdvertisingExpense)
summary(model_noweight)
plot(Food$AdvertisingExpense,model_noweight$residuals)
weight_coef=lm( model_noweight$residuals^2~Food$AdvertisingExpense )$coef
model_weight=lm(Food$Income~Food$AdvertisingExpense,weights=1/(weight_coef[1]+Food$AdvertisingExpense*weight_coef[2]))
summary(model_weight)
plot(Food$AdvertisingExpense,
     1/sqrt(weight_coef[1]+Food$AdvertisingExpense*weight_coef[2]) * model_weight$residuals)
abline(h=0,col="grey",lwd=3)
