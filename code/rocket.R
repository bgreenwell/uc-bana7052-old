# Lecture 01 -------------------------------------------------------------------

# Load required packages
library(ggplot2)

# Load the data
rocket <- read.csv("https://bgreenwell.github.io/uc-bana7052/data/rocket.csv")

# Print first few observations
head(rocket)

# Print number of rows and columns
dim(rocket)

# Scatterplot of the data
p <- ggplot(rocket, aes(x = age, y = strength)) +
  geom_point() +
  theme_light()
print(p)

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

# Draw the fitted regression line
center_of_data <- data.frame(
  "strength" = mean(rocket$strength),
  "age" = mean(rocket$age)
)
p + geom_smooth(method = "lm", se = FALSE) +
  geom_point(data = center_of_data, col = "red2", size = 2)
  

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

# Why not zero!?
sum(residuals(fit))


# Lecture 02 -------------------------------------------------------------------

# Summarize fitted arsenic model
investr::plotFit(fit)
summary(fit)

# Confidence intervals for the regression coefficients
confint(fit)
confint(fit, level = 0.95)  # alpha = 0.05
confint(fit, level = 0.9)
confint(fit, level = 0.99)

