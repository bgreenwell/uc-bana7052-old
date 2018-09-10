# Load required packages
library(ggplot2)

# Load the data
rocket <- read.csv("https://bgreenwell.netlify.com/data/rocket.csv")

# Print first few observations
head(rocket)

# Print number of rows and columns
dim(rocket)

# Scatterplot of the data
ggplot(rocket, aes(x = age, y = strength)) +
  geom_point() +
  theme_light()

# Correlation between age and strength
cor(rocket)

# Summary of age
mean(rocket$age)
sd(rocket$age)
median(rocket$age)
boxplot(rocket$age)
hist(rocket$age)

# SLR model
fit <- lm(strength ~ age, data = rocket)

# Fitted values
fitted(fit)

# Residuals
residuals(fit)

# Sanity check
rocket$strength - fitted(fit)

# Summary of model
summary(fit)

# RMSE and MSE
summary(fit)$sigma
sigma(fit)  # using extractor function
sigma(fit)^2
sum(residuals(fit)^2) / (nrow(rocket) - 2)  # sanity check
