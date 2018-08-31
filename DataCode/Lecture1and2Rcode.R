getwd()
setwd("C:/YanUnix/BANA7041/DataCode")

rm(list=ls())
rocket <- read.delim("Data-ex-2-1 (Rocket Prop).txt",h=T)
head(rocket)
tail(rocket)
names(rocket)
str(rocket)
print(rocket)
summary(rocket)
names(rocket)[1]="response"
names(rocket)
names(rocket)[1]="y"
attach(rocket)
y
x
#detach(rocket)
y
x
n=dim(rocket)[1]
number_of_col=dim(rocket)[2]
#plot(x,y,pch=20)

mean(x)
median(x)
sd(x)
IQR(x)
hist(x,breaks=10)
summary(x)
cor(x,y) 
cor(rocket) 
cor.test(x,y)

hist(x)
boxplot(x)
#boxplot(y~x) # creates side-by-side boxplots
#install.packages("UsingR")
#library(UsingR)
#DOTplot(x) #creates a dotplot (UsingR package must be installed)
stem(x) #creates a stem plot for the variable x
plot(y~x) #creates a scatterplot of y versus x
lines(lowess(y~x)) # adds locally weighted scatterplot smoother line to plot
#qplot(x, y) #creates a quick plot (ggplot2 package must be installed)

model1 <- lm(y ~ x, data=rocket)
plot(y~x)
abline(model1,lwd=3)
#install.packages("HH")
#library(HH)
#ci.plot(model1) #creates a scatterplot with fitted line, confidence bands, and prediction bands (HH package must be installed)
summary(model1)
model1$coefficients
model1$residuals
model1$fitted.values
plot(y~x)
points(rocket$x,model1$fitted.values,pch=20,col="red")
plot(x,model1$res)
abline(h=0)
par(mfrow=c(1,2))
plot(y~x)
points(rocket$x,model1$fitted.values,pch=20,col="red")
plot(x,model1$res)
abline(h=0)

anova(model1)

# coefficient
Sxx=sum((rocket$x-mean(rocket$x))^2)
Sxy=sum((rocket$x-mean(rocket$x))*rocket$y)
Sxy/Sxx
mean(rocket$y)-Sxy/Sxx*mean(rocket$x)


###########################################################################################
# summary of hypothesis testing results
summary(model1)$coef
# coefficient estimate
summary(model1)$coef[,1]
# standard error
summary(model1)$coef[,2]
# t value
summary(model1)$coef[,3]
# p value
summary(model1)$coef[,4]

# manual ways to get t values and p values
summary(model1)$coef[,1]/summary(model1)$coef[,2]
2*(1-pt(abs(summary(model1)$coef[,1]/summary(model1)$coef[,2]),n-2))

###########################################################################################
# F test and R square
anova(model1)

summary(model1)

# manual ways to calculate R and F test
SST=sum((y-mean(y))^2)
SSRes=sum((y-model1$fitted.values)^2)
SSR=sum((model1$fitted.values-mean(y))^2)
SSR+SSRes
SST
F=(SSR/1)/(SSRes/(n-2))
F

summary(model1)$coef[2,3]^2

# R square
summary(model1)$r.square

1-sum((rocket$y-model1$fitted.values)^2)/sum((rocket$y-mean(rocket$y))^2)


###########################################################################################
# CI for coef
confint(model1,level=0.95)
confint(model1,level=0.90)
confint(model1,level=0.99)

# manual ways to calculate these
# 95%
summary(model1)$coef[,1]-qt(0.025,18)*summary(model1)$coef[,2]
summary(model1)$coef[,1]+qt(0.025,18)*summary(model1)$coef[,2]
# 99%
summary(model1)$coef[,1]-qt(0.005,18)*summary(model1)$coef[,2]
summary(model1)$coef[,1]+qt(0.005,18)*summary(model1)$coef[,2]




###########################################################################################
# CI for mean response, prediction of new observation, 
predict.lm(model1, interval="confidence") #make prediction and give confidence interval for the mean response
predict.lm(model1, interval="prediction") #make prediction and give prediction interval for the mean response


newx<-seq(0,30)
conf<-predict(model1,newdata=data.frame(x=newx),interval = c("confidence"),level = 0.95,type="response")
plot(rocket$x,rocket$y,pch=20)
model1 <- lm(y ~ x, data=rocket)
abline(model1,col="blue")
lines(newx,conf[,2],col="red",lty=2)
lines(newx,conf[,3],col="red",lty=2)
pred<-predict(model1,newdata=data.frame(x=newx),interval = c("prediction"),level = 0.95,type="response")
lines(newx,pred[,2],col="green",lty=2)
lines(newx,pred[,3],col="green",lty=2)

# manual ways to do CI
MS_Res=SSRes/n-2
model1$coef[1]+model1$coef[2]*newx-qt(0.025,n-2)*sqrt(MS_Res*(1/n+(newx-mean(rocket$x))^2/Sxx))
model1$coef[1]+model1$coef[2]*newx+qt(0.025,n-2)*sqrt(MS_Res*(1/n+(newx-mean(rocket$x))^2/Sxx))

points(newx,model1$coef[1]+model1$coef[2]*newx-qt(0.025,n-2)*sqrt(MS_Res*(1/n+(newx-mean(rocket$x))^2/Sxx))
       ,lwd=3,col="grey",type="l")
points(newx,model1$coef[1]+model1$coef[2]*newx+qt(0.025,n-2)*sqrt(MS_Res*(1/n+(newx-mean(rocket$x))^2/Sxx))
       ,lwd=3,col="grey",type="l")

model1$coef[1]+model1$coef[2]*newx-qt(0.025,n-2)*sqrt(MS_Res*(1/n+1+(newx-mean(rocket$x))^2/Sxx))
model1$coef[1]+model1$coef[2]*newx+qt(0.025,n-2)*sqrt(MS_Res*(1/n+1+(newx-mean(rocket$x))^2/Sxx))

points(newx,model1$coef[1]+model1$coef[2]*newx-qt(0.025,n-2)*sqrt(MS_Res*(1/n+1+(newx-mean(rocket$x))^2/Sxx))
       ,lwd=3,col="cyan",type="l")
points(newx,model1$coef[1]+model1$coef[2]*newx+qt(0.025,n-2)*sqrt(MS_Res*(1/n+1+(newx-mean(rocket$x))^2/Sxx))
       ,lwd=3,col="cyan",type="l")

# R square
summary(model1)$r.square

# manual way
SST=sum((rocket$y-mean(rocket$y))^2)
SSRes=sum((rocket$y-model1$fitted.values)^2)
SSR=sum((model1$fitted.values-mean(rocket$y))^2)
SSR+SSRes
SST
1-SSRes/SST


# additional features
plot(model1)
#Tests for homogeneity of variance
install.packages("lmtest")
library(lmtest)
bptest(model1) #get the Breusch-Pagan test (lmtest package must be installed)
install.packages("lawstat")
library(lawstat)
levene.test(y, (x<13)*1) #get the Levene test (lawstat package must be installed)
#Tests for normality
install.packages("nortest")
library(nortest)
ad.test(model1$res) #get Anderson-Darling test for normality
hist(model1$res)
cvm.test(model1$res) #get Cramer-von Mises test for normaility
lillie.test(model1$res) #get Lilliefors (Kolmogorov-Smirnov) test for normality
pearson.test(model1$res) #get Pearson chi-square test for normaility
sf.test(model1$res) #get Shapiro-Francia test for normaility
#Lack-of-fit test
Reduced=lm(y~x)#fit reduced model
Full=lm(y~0+as.factor(x)) #fit full model
anova(Reduced, Full) #get lack-of-fit test
library(MASS)
boxcox(model1)
detach(rocket)

rm(list=ls())
d=read.csv("chicago.csv")
names(d)
attach(d)
plot(x,y)
model2=lm(y~x)
summary(model2)
abline(model2)
library(HH)
ci.plot(model2)
anova(model2)
confint(model2,level=0.95)

plot(model2$residuals,pch=20)
abline(h=0)
points(model2$residuals,type="h")
detach(d)

rm(list=ls())
d=read.csv("delta.csv")
names(d)
names(d)[5]="Time"
names(d)[6]="Dist"
attach(d)
plot(Dist,Time,pch=20)
model3=lm(Time~Dist)
summary(model3)
abline(model3)
lines(lowess(Time~Dist))
library(HH)
ci.plot(model3)
anova(model3)
confint(model3,level=0.95)

plot(model3$residuals,pch=20)
abline(h=0,col="red",lwd=3)
detach(d)

rm(list=ls())
d=read.csv("vote.csv")
names(d)
names(d)[3]="Unlikeness"
names(d)[4]="Percentage"
pairs(d)
attach(d)
plot(Congress,Beginning.Year)
plot(Unlikeness,Percentage,pch=20)
model4=lm(Percentage~Unlikeness)
summary(model4)
abline(model4)
lines(lowess(Percentage~Unlikeness))
library(HH)
ci.plot(model4)
anova(model4)
confint(model4,level=0.95)

plot(model4$residuals,pch=20)
points(model4$residuals,type="h")
abline(h=0,col="red",lwd=3)
