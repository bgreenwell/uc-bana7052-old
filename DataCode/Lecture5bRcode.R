rm(list=ls())
Life <- read.csv("data-ex-8-1.csv",h=T)
pairs(Life,pch=20)
# creat a dummy variable
Life$TypeB=1*(Life$ToolType=="B")
Life$TypeB
# creat color vector for plotting
col_vec=ifelse(Life$TypeB==1,"blue","red")
pch_vec=ifelse(Life$TypeB==1,15,18)
plot(Life$rpm,Life$Hour,col=col_vec,pch=pch_vec)
# fit regression
model1 <- lm(Life$Hour ~ Life$rpm+Life$TypeB )
summary(model1)
model2 <- lm(Life$Hour ~ Life$rpm+Life$TypeB+Life$rpm:Life$TypeB)


model1a <- lm(Life$Hour ~ Life$rpm+Life$ToolType)
summary(model1a)
model2a <- lm(Life$Hour ~ Life$rpm+Life$ToolType+Life$rpm:Life$ToolType)
summary(model2a)

# fit regression
model2 <- lm(Life$Hour ~ Life$rpm+Life$ToolType+Life$rpm:Life$ToolType)
summary(model2)
model0= lm(Life$Hour ~ Life$rpm)
anova(model2,model0)
anova(model2,model1)


model3=lm(Life$Hour ~ Life$rpm+Life$ToolType+Life$Oil+Life$rpm:Life$ToolType+Life$rpm:Life$Oil)
summary(model3)
model4=lm(Life$Hour ~ Life$rpm+Life$ToolType+Life$Oil+Life$rpm:Life$ToolType+Life$rpm:Life$Oil+Life$ToolType:Life$Oil)
summary(model4)

model5=lm(Life$Hour ~ Life$rpm+Life$ToolType+Life$Oil+Life$rpm:Life$ToolType+Life$rpm:Life$Oil+Life$rpm:Life$ToolType:Life$Oil)
summary(model5)


rm(list=ls())
d <- read.csv("data-prob-8-3.csv",h=T)
names(d)
model1=lm(y~x1+x2+x3,data=d)
summary(model1)
d$Bos=ifelse(d$x3=="Boston",1,0)
d$Minn=ifelse(d$x3=="Minneapolis",1,0)
d$SD=ifelse(d$x3=="San Diego",1,0)
model2=lm(y~x1+x2+Bos+Minn+SD,data=d)
summary(model2)

d$Aus = ifelse(d$x3=="Austin",1,0)
model3=lm(y~x1+x2+Minn+SD+Aus,data=d)
summary(model3)
model3=lm(y~x1+x2+Bos+Minn+SD+Aus,data=d)
summary(model3)
