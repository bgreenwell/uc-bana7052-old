# Data concerning the strength of kraft paper and the percentage of hardwood in 
# the batch of pulp from which the paper was produced.

# Load the hardwood conentration data
url <- "https://bgreenwell.github.io/uc-bana7052/data/hardwood.csv"
hardwood <- read.csv(url)

# Print first few observations
head(hardwood)

# Scatterplot of the data
plot(hardwood, pch = 19)

# Fit an SLR model
fit1 <- lm(TsStr ~ HwdCon, data = hardwood)
investr::plotFit(fit1, pch = 19, col.fit = "red2")

# Plot residuals vs HwdCon (i.e., X)
par(mfrow = c(1, 2))
plot(x = hardwood$HwdCon, y = residuals(fit1), xlab = "HwdCon",
     ylab = "Residuals", main = "Residuals vs X")
abline(h = 0, lty = "dotted")
plot(fit1, which = 1, caption = "", main = "Residuals vs Fitted")

# Fit a quadratic model
par(mfrow = c(1, 1))
fit2 <- lm(TsStr ~ HwdCon + I(HwdCon^2), data = hardwood)
investr::plotFit(fit2, pch = 19, col.fit = "red2")

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
