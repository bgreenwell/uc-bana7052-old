# Load the hardwood conentration data
hardwood <- read.csv("docs/data/hardwood.csv")

# Print first few observations
head(hardwood)

# Scatterplot of the data
plot(hardwood)

# Fit an SLR model
fit1 <- lm(TsStr ~ HwdCon, data = hardwood)
investr::plotFit(fit1)

# Plot residuals vs HwdCon (i.e., X)
plot(x = hardwood$HwdCon, y = residuals(fit1))
abline(h = 0, lty = "dotted")

# Fit a quadratic model
fit2 <- lm(TsStr ~ HwdCon + I(HwdCon^2), data = hardwood)
investr::plotFit(fit2)

# Fit higher order models
par(mfrow = c(2, 3))
for (i in 1:6) {
  fit <- lm(TsStr ~poly(HwdCon, degree = i), data = hardwood)
  investr::plotFit(fit, main = paste("Degree =", i))
}

# Fit higher order models
par(mfrow = c(2, 3))
for (i in 1:6) {
  fit <- lm(TsStr ~poly(HwdCon, degree = i), data = hardwood)
  investr::plotFit(fit, main = paste("Degree =", i), 
                   interval = "confidence", shade = TRUE,
                   xlim = c(-10, 30))
}

