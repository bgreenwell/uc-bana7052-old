# Load required packages
library(ggplot2)
## # List of required (CRAN) packages
## pkgs <- c(
##   "ggplot2",    # for awesome graphics
##   "investr",    # for data sets and plotFit() functions
## )
## 
## # Install required (CRAN) packages
## for (pkg in pkgs) {
##   if (!(pkg %in% installed.packages()[, "Package"])) {
##     install.packages(pkg)
##   }
## }

set.seed(101)
x <- rep(1:5, each = 10)
y <- 1 + 1*x + rnorm(length(x), sd = 3)
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  labs(x = "X", y = "Y")

set.seed(101)
x <- rep(1:5, each = 10)
y <- 1 + 1*x + rnorm(length(x), sd = 3)
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, col = "red2") +
  labs(x = "X", y = "Y")

set.seed(101)
x <- rep(1:5, each = 10)
y <- 1 + 1*x + rnorm(length(x), sd = 3)
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, col = "red2") +
  labs(x = "X", y = "Y")

# Simulate data
n <- 100
set.seed(8451)
x <- runif(n, min = 0, max = 10)
y <- rnorm(n, mean = 1 + 10*x, sd = 10)  #<<

# Fit an SLR model
fit <- lm(y ~ x)
coef(fit)

# Plot the results
plot(x, y)
abline(fit, lwd = 2, col = "red2")

# Run simulation
set.seed(8451)
sim <- t(replicate(10000, expr = {
  x <- runif(n = 100, min = 0, max = 10)
  y <- rnorm(n = 100, mean = 1 + 10*x, sd = 10)
  coef(lm(y ~ x))
}))

# Sample means
apply(sim, MARGIN = 2, FUN = mean)

# Sample standard deviations
apply(sim, MARGIN = 2, FUN = sd)

# Sampling distribution
hist(sim[, 2], breaks = 50, freq = FALSE, col = "gray30", border = "white", 
     las = 1, xlab = expression(hat(beta)[1]),
     main = expression(paste("Sampling distribution of ", hat(beta)[1])))
abline(v = 10, lwd = 3, col = "red2")
alpha <- 0.05           # significance level
n <- 30                 # sample size         
qt(1 - alpha/2, n - 2)  # cutoff value        #<<

# Load the rocket propellant data
url <- "https://bgreenwell.github.io/uc-bana7052/data/rocket.csv"
rocket <- read.csv(url)

# Print first six rows
tibble::as_tibble(rocket)[1:6, ]

# Load required packages
library(investr)

# Fit an SLR model
rocket_fit <- lm(strength ~ age, data = rocket)

# Plot the data with the fitted mean response
plotFit(rocket_fit, lwd.fit = 2, col.fit = "red2", pch = 19)

# Print a summary of the fitted model
summary(rocket_fit)

# Compute a 95% CI for the slope
confint(rocket_fit, level = 0.95)  #<<
confint(rocket_fit, level = 0.95)

# Load the crystal weight data
data(crystal, package = "investr")

# Fit an SLR model
crystal_fit <- lm(weight ~ time, data = crystal)

# Plot the data with the fitted mean response
plotFit(crystal_fit, lwd.fit = 2, 
        col.fit = "red2", pch = 19)

# Print a summary of the model
summary(crystal_fit)

# Compute 95% CIs
confint(crystal_fit, level = 0.95)  #<<

# Extract summary of estimated slope
(slope <- summary(rocket_fit)$coef["age", 1:2])  #<<

# Compute test statistic
(t_obs <- (slope["Estimate"] + 40) / 
    slope["Std. Error"])

# Compute cutoff from reference distribution
alpha <- 0.05
n <- nrow(rocket)
(t_ref <- qt(1 - alpha/2, df = n - 2))  #<<

# Decision rule
abs(t_obs) > t_ref

# Compute a 90% CI for the slope
confint(crystal_fit, parm = "time", level = 0.9)

# Rocket propellant example
(t_obs <- slope["Estimate"] / slope["Std. Error"])
pt(abs(t_obs), df = nrow(rocket) - 2, lower.tail = FALSE)

# Rocket propellant example
(t_obs <- slope["Estimate"] / slope["Std. Error"])
2 * pt(abs(t_obs), df = nrow(rocket) - 2, lower.tail = FALSE)

slope <- summary(crystal_fit)$coef["time", ]
(t_obs <- (slope["Estimate"] - 3/4) / slope["Std. Error"])  #<<
(p_val <- 2 * pt(abs(t_obs), df = nrow(crystal) - 2, lower.tail = FALSE))  #<<

# Compute ANOVA table for the fitted model
anova(rocket_fit)  #<<

# Print summary of fitted model
summary(rocket_fit)

# Print summary of the fitted model
summary(crystal_fit)

# What values can we pull out from summary()
names(summary(crystal_fit))

# Observed test statitic
f_obs <- summary(crystal_fit)$fstatistic

# Compute p-value (one approach)
pf(f_obs, df1 = 1, df2 = nrow(crystal) - 2, lower.tail = FALSE)

# Compute p-value (another approach)
1 - pf(f_obs, df1 = 1, df2 = nrow(crystal) - 2)

# Fit an intercept only model
rocket_fit_reduced <- lm(strength ~ 1, data = rocket)
mean(rocket$strength)  # compare to estimated intercept  #<<
anova(rocket_fit_reduced, rocket_fit)  # compare models  #<<
sapply(c(10, 20, 30, 50, Inf), function(x)  #<<
  qt(0.975, df = x))                        #<<

# Confidence interval for the mean response 
# at age = 15
new_data <- data.frame(age = 15)
predict(rocket_fit, newdata = new_data, 
        interval = "confidence")

# Plot a 95% (pointwise) confidence band
plotFit(rocket_fit, interval = "confidence")
abline(v = 15, col = "red2")

# Extrapolation
plotFit(rocket_fit, interval = "confidence", 
        shade = TRUE, xlim = c(-20, 100))

# Confidence interval for the mean response at age = 15
new_data <- data.frame(time = 20)
predict(crystal_fit, newdata = new_data, interval = "confidence",
        level = 0.9)  #<<

# Plot the (pointwise) confidence band around the fitted regression line
plotFit(crystal_fit, interval = "confidence", cex = 1.4, pch = 19, shade = TRUE,
        col.conf = adjustcolor("red2", alpha.f = 0.5))

# Extract R-squared from the model summary
summary(rocket_fit)$r.squared

# ANOVA decomposition
anova(rocket_fit)

# Compute R-squared by hand
SSE <- anova(rocket_fit)["Residuals", "Sum Sq"]
SST <- sum((rocket$strength - 
              mean(rocket$strength)) ^ 2)
round(c(SSE, SST, 1 - SSE/SST), digits = 3)

# Compute R-squared by hand
SSE <- anova(crystal_fit)["Residuals", "Sum Sq"]
SST <- sum((crystal$weight - 
              mean(crystal$weight)) ^ 2)
1 - SSE/SST

# Compute coefficient of correlation
(r_squared <- summary(rocket_fit)$r.squared)
sqrt(r_squared)
cor(rocket)  # compare with correlation coefficient
