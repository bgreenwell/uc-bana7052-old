delivery <- read.csv("eg3.1.delivery.csv",h=T)
n=dim(delivery)[1]
# build linear regression
model1 <- lm(DeliveryTime ~ NumberofCases+Distance, data=delivery)

# obtain residual
model1$residuals
# obtain SSRes and MSRes, note the MSRes is also our estimate of sigma square.
SSRes=sum((model1$residuals-mean(model1$residuals))^2)
MSRes=SSRes/(n-3)
# obtain standardized residuals
standardized_res=model1$residuals/sqrt(MSRes)
standardized_res=model1$residuals/sd(model1$residuals)
# PRESS residuals
PRESS_res=model1$residuals/(1 - lm.influence(model1)$hat)



















delivery <- read.csv("eg3.1.delivery.csv",h=T)

# build linear regression
model1 <- lm(DeliveryTime ~ NumberofCases+Distance, data=delivery)

# obtain residual
model1$residuals
# obtain SSRes and MSRes, note the MSRes is also our estimate of sigma square.
SSRes=sum((model1$residuals-mean(model1$residuals))^2)
MSRes=SSRes/(n-3)
# obtain standardized residuals
standardized_res=model1$residuals/sqrt(MSRes)
# PRESS residuals
PRESS_res=model1$residuals/(1 - lm.influence(model1)$hat)



# plot all residual and leverage
# partition the canvas into 6 columns.
par(mfrow=c(1,6))
plot(model1$fitted.values,model1$residuals,pch=20,ylab="residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,standardized_res,pch=20,ylab="standardized residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,PRESS_res,pch=20,ylab="PRESS residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,PRESS_res/sd(PRESS_res),pch=20,ylab="PRESS residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,lm.influence(model1)$hat,pch=20,ylab="leverage",xlab="fitted value")
abline(h=0,col="grey")




qqnorm(model1$residuals)
qqline(model1$residuals)
plot(density(model1$residuals))




# generate residual plot, NumberofCases vs residuals
plot(delivery$NumberofCases,model1$residuals)
# generate residual plot, Distance vs residuals
plot(delivery$Distance,model1$residuals)
# generate residual plot, fitted values vs residual
plot(model1$fitted.values,model1$residuals)


# example
rm(list=ls())
rocket <- read.delim("Data-ex-2-1 (Rocket Prop).txt",h=T)
n=dim(rocket)[1]
plot(rocket$x,rocket$y,pch=20)
model1 <- lm(y ~ x, data=rocket)
abline(model1,col="blue")
# residual
model1$residuals
#standardized residuals
SSRes=sum((model1$residuals-mean(model1$residuals))^2)
MSRes=SSRes/(n-3)
standardized_res=model1$residuals/sqrt(MSRes)
# PRESS residuals
PRESS_res=model1$residuals/(1 - lm.influence(model1)$hat)

# plot all residual and leverage
# partition the canvas into 6 columns.
par(mfrow=c(1,4))
plot(model1$fitted.values,model1$residuals,pch=20,ylab="residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,standardized_res,pch=20,ylab="standardized residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,PRESS_res,pch=20,ylab="PRESS residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,lm.influence(model1)$hat,pch=20,ylab="leverage",xlab="fitted value")
































# clean memory
rm(list=ls())
# read data
delivery <- read.csv("eg3.1.delivery.csv",h=T)
# obtain sample size
n=dim(delivery)[1]
names(delivery)
# visualize data
pairs (delivery,pch=20)
# build linear regression
model1 <- lm(DeliveryTime ~ NumberofCases+Distance, data=delivery)

# residual
model1$residuals

SSRes=sum((model1$residuals-mean(model1$residuals))^2)
MSRes=SSRes/(n-3)
#standardized residuals
standardized_res=model1$residuals/sqrt(MSRes)

# studentized residuals
studentized_res=model1$residuals/sqrt(MSRes)/sqrt(1 - lm.influence(model1)$hat)
# manual way
X=as.matrix(cbind(1,delivery[,c("NumberofCases","Distance")]))
H=X%*%solve(t(X)%*%X)%*%t(X)
studentized_res2=model1$residuals/sqrt(MSRes)/sqrt(1 - diag(H))

# PRESS residuals
PRESS_res=model1$residuals/(1 - lm.influence(model1)$hat)

# R student
R_Student=rstudent(model1)
# manual way
S2_i=( (n-3)*MSRes-model1$residuals^2/(1 - diag(H)) )/(n-3-1)
R_Student2=model1$residuals/sqrt(S2_i)/sqrt(1 - diag(H))

# plot all residual and leverage
# partition the canvas into 6 columns.
par(mfrow=c(1,6))
plot(model1$fitted.values,model1$residuals,pch=20,ylab="residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,standardized_res,pch=20,ylab="standardized residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,studentized_res,pch=20,ylab="studentized residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,PRESS_res,pch=20,ylab="PRESS residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,R_Student,pch=20,ylab="R student",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,lm.influence(model1)$hat,pch=20,ylab="leverage",xlab="fitted value")





# example
rm(list=ls())
rocket <- read.delim("Data-ex-2-1 (Rocket Prop).txt",h=T)
n=dim(rocket)[1]
plot(rocket$x,rocket$y,pch=20)
model1 <- lm(y ~ x, data=rocket)
abline(model1,col="blue")
# residual
model1$residuals
#standardized residuals
SSRes=sum((model1$residuals-mean(model1$residuals))^2)
MSRes=SSRes/(n-3)
standardized_res=model1$residuals/sqrt(MSRes)
# studentized residuals
studentized_res=model1$residuals/sqrt(MSRes)/sqrt(1 - lm.influence(model1)$hat)
# PRESS residuals
PRESS_res=model1$residuals/(1 - lm.influence(model1)$hat)
# R student
R_Student=rstudent(model1)

# plot all residual and leverage
# partition the canvas into 6 columns.
par(mfrow=c(1,6))
plot(model1$fitted.values,model1$residuals,pch=20,ylab="residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,standardized_res,pch=20,ylab="standardized residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,studentized_res,pch=20,ylab="studentized residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,PRESS_res,pch=20,ylab="PRESS residual",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,R_Student,pch=20,ylab="R student",xlab="fitted value")
abline(h=0,col="grey")
plot(model1$fitted.values,lm.influence(model1)$hat,pch=20,ylab="leverage",xlab="fitted value")






# qqplot
qqnorm(model1$residuals)
qqline(model1$residuals)
qqnorm(studentized_res)
qqline(studentized_res)
hist(model1$residuals,breaks=20)

# residual plot
plot(delivery$NumberofCases,model1$residuals)
plot(delivery$Distance,model1$residuals)
plot(model1$fitted.values,model1$residuals)
plot(model1$fitted.values,PRESS_res)





# partial regression plot
model2 <- lm(DeliveryTime ~ Distance, data=delivery)
model3 <- lm(NumberofCases ~ Distance, data=delivery)
plot(model3$residuals,model2$residuals,pch=20,ylab="y residual", xlab="NumberofCases residual")

# R_square_prediction
PRESS=sum(PRESS_res^2)
SST=sum((delivery$DeliveryTime-mean(delivery$DeliveryTime))^2)
R_square_pred=1-PRESS/SST
SSR=sum((model1$fitted.values-mean(delivery$DeliveryTime))^2)
SSRes=sum((delivery$DeliveryTime-model1$fitted.values)^2)

