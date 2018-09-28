# Boyfat data
bodyfat <- data.frame(
  triceps = c(19.5, 24.7, 30.7, 29.8, 19.1, 25.6, 31.4, 27.9, 22.1, 25.5, 31.1,
              30.4, 18.7, 19.7, 14.6, 29.5, 27.7, 30.2, 22.7, 25.2),
  thigh   = c(43.1, 49.8, 51.9, 54.3, 42.2, 53.9, 58.5, 52.1, 49.9, 53.5, 56.6,
              56.7, 46.5, 44.2, 42.7, 54.4, 55.3, 58.6, 48.2, 51),
  midarm  = c(29.1, 28.2, 37, 31.1, 30.9, 23.7, 27.6, 30.6, 23.2, 24.8, 30,
              28.3, 23, 28.6, 21.3, 30.1, 25.7, 24.6, 27.1, 27.5),
  bodyfat = c(11.9, 22.8, 18.7, 20.1, 12.9, 21.7, 27.1, 25.4, 21.3, 19.3, 25.4,
              27.2, 11.7, 17.8, 12.8, 23.9, 22.6, 25.4, 14.8, 21.1)
)

# Correlation matrix
corrplot::corrplot(cormat <- cor(bodyfat))
cormat

# Scatterplot matrix
pairs(bodyfat, pch = 19, col = adjustcolor("dodgerblue2", alpha.f = 0.5))

# Fit a linear model
fit <- lm(bodyfat ~ ., data = bodyfat)
summary(fit)  # print model summary

# Variance inflation factors
vif(fit)

# Manually compute VIF(midarm)
fit_midarm <- lm(midarm ~ triceps + thigh, data = bodyfat)
summary(fit_midarm)
1 / (1 - 0.9904)
