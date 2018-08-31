n=30
x=rnorm(n)
y=1+2*x+rnorm(n)
summary(lm(y~x))
x2=rnorm(n)
summary(lm(y~x+x2))
x3=rnorm(n)
summary(lm(y~x+x2+x3))
x4=rnorm(n)
summary(lm(y~x+x2+x3+x4))

n=30
x=rnorm(n)
y=1+2*x+rnorm(n)
m1=lm(y~x)
MSRes1=sum(m1$residuals^2)/(n-2)
MSRes1
x2=rnorm(n)
m2=lm(y~x+x2)
MSRes2=sum(m2$residuals^2)/(n-3)
MSRes2
x3=rnorm(n)
m3=lm(y~x+x2+x3)
MSRes3=sum(m3$residuals^2)/(n-4)
MSRes3
x4=rnorm(n)
m4=lm(y~x+x2+x3+x4)
MSRes4=sum(m3$residuals^2)/(n-5)

MSRes1
MSRes2
MSRes3
MSRes4
summary(m1)
summary(m2)
summary(m3)
summary(m4)

library(leaps)
n=10000
x=rnorm(n)
y=1+2*x+3*x2+0.1*rnorm(n)
m1=lm(y~x)
x2=rnorm(n)
m2=lm(y~x+x2)
x3=rnorm(n)
m3=lm(y~x+x2+x3)
x4=rnorm(n)
m4=lm(y~x+x2+x3+x4)
leaps( x=cbind(x,x2,x3,x4), y=y, method="Cp")

extractAIC(m1)
extractAIC(m2)
extractAIC(m3)
extractAIC(m4)

# BIC
extractAIC(m1, k = log(n))
extractAIC(m2, k = log(n))
extractAIC(m3, k = log(n))
extractAIC(m4, k = log(n))


library(MASS)
library(leaps)
asphalt <- read.csv("data-table-10-5 (Asphalt).csv",h=T)
temp <- lm(y~x1+x2+x3+x4+x5+x6,data=asphalt)
step <- stepAIC(temp, direction="both")
step <- stepAIC(temp, direction="backward")

step$anova 
library(leaps)
temp <- regsubsets(y~x1+x2+x3+x4+x5+x6,data=asphalt,nbest=4)
summary(temp)
plot(temp)

null=lm(y~1, data=asphalt[,-1])
full=lm(y~., data=asphalt[,-1])
step(null, scope=list(lower=null, upper=full), direction="forward")

step(full, scope=list(lower=null, upper=full), direction="backward")

step(full, scope=list(lower=null, upper=full), direction="both")

step(null, scope=list(lower=null, upper=full), direction="both")

steplm=step(null, scope=list(lower=null, upper=full), direction="both")
summary(steplm)

n=dim(asphalt)[1]
step(null, scope=list(lower=null, upper=full), k = log(n), direction="forward")

step(full, scope=list(lower=null, upper=full), k = log(n), direction="backward")

step(full, scope=list(lower=null, upper=full), k = log(n), direction="both")

step(null, scope=list(lower=null, upper=full), k = log(n), direction="both")
