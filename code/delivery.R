# Required packages
library(broom)      # for augment() function
library(dplyr)      # for data wrangling
library(ggplot2)    # for snazzier plots
library(gridExtra)  # for grid.arrange() function

# Load the delivery data
url <- "https://bgreenwell.github.io/uc-bana7052/data/delivery.csv"
delivery <- read.csv(url)
head(delivery, n = 10)  # print first 10 observations

# Scatterplot matrix
GGally::ggpairs(delivery[, -1])  

# Another scatterplot matrix
lattice::splom(delivery[, -1], type = c("p", "smooth"), pch = 19, 
               col = 1, lty = "dotted", alpha = 0.6)

# Fit a linear model
(delivery_fit <- lm(DeliveryTime ~ NumberofCases + Distance, 
                    data = delivery))

# Fit a linear model (equivalent to previous?)
(delivery_fit <- lm(DeliveryTime ~ ., data = delivery))

# Fit a linear model
delivery <- subset(delivery, select = -Index)  #<<
(delivery_fit <- lm(DeliveryTime ~ ., data = delivery))

round(coef(delivery_fit), digits = 3)  #<<

# Print summary of fitted model
summary(delivery_fit)

# Manually construct F-test
delivery_fit_reduced <- lm(DeliveryTime ~ 1, data = delivery)
anova(delivery_fit_reduced, delivery_fit)

# Construct 95% CIs for the coefficients
confint(delivery_fit, level = 0.95)


# Estimate regression coefficients using matrix approach -----------------------

xnames <- c("NumberofCases", "Distance")
X <- data.matrix(delivery[, xnames])  # or use `as.matrix()`
head(X)
Y <- delivery$DeliveryTime
solve(t(X) %*% X) %*% t(X) %*% Y  #<<

X <- cbind(1, X)  # add column of ones
solve(t(X) %*% X) %*% t(X) %*% Y  #<<

# A safer appraoch
X <- model.matrix(~ NumberofCases + Distance, data = delivery)
head(X)
Y <- delivery$DeliveryTime
solve(t(X) %*% X) %*% t(X) %*% Y  #<<


# Fitted values and residuals  -------------------------------------------------

# Extract fitted values and residuals
.fitted <- fitted(delivery_fit)
.resids <- residuals(delivery_fit)
head(cbind(delivery, .fitted, .resids))
cbind(.fitted + .resids, delivery$DeliveryTime)

# The fitted values are just the predicted values from the data used to fit the
# model
.fitted2 <- predict(delivery_fit)  
.fitted3 <- predict(delivery_fit, newdata = delivery)
head(cbind(.fitted, .fitted2, .fitted3))

# Hand compute resiuals
.resids2 <- delivery$DeliveryTime - .fitted  # observed minus predicted
head(cbind(.resids, .resids2))


# Standardized residuals -------------------------------------------------------

# Residuals
r <- residuals(delivery_fit)
rstan <- rstandard(delivery_fit)
rstud <- rstudent(delivery_fit)
press <- rstandard(delivery_fit, type = "predictive")

# Compute PRESS residuals using LOOCV
press2 <- R2.loocv <- numeric(length = nrow(delivery))
for (i in seq_len(nrow(delivery))) {
  .in <- delivery[-i, ]  # remove i-th row
  .out <- delivery[i, ]  # extract i-th row
  .fit <- update(delivery_fit, data = .in)
  press2[i] <- .out$DeliveryTime - predict(.fit, newdata = .out)
  R2.loocv[i] <- summary(.fit)$r.squared  # just for kicks
}
head(cbind(press, press2))  # should be same (within rounding error)
mean(R2.loocv)


# Leverage (i.e., hat) values --------------------------------------------------

# Which training observations might be considered as outliers?
h <- hatvalues(delivery_fit)
plot(h, type = "h", ylim = extendrange(h, f = 0.1))
abline(h = 2 * 3 / nrow(delivery), lty = "dotted")  #<<
text(h, labels = seq_len(nrow(delivery)), pos = 3, col = "red2")

# Determine outlyingness of new observations (HOMEWORK!!)
X <- model.matrix(delivery_fit)
head(X)
X_new <- rbind(  # new observation
  c(1, 10, 1000), 
  c(1, 40,    3)
)
head(X_new)
(h_new <- diag(X_new %*% solve(t(X) %*% X) %*% t(X_new)))
h_new > max(abs(h))


# Diagnostic plots -------------------------------------------------------------

# Residuals vs fitted values and QQ normal plot (base R graphics)
par(mfrow = c(1, 2))  # setup for 2-by-2 grid of plots
plot(.fitted, .resids, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2, col = "red")
qqnorm(.resids)
qqline(.resids, lty = 2, col = "red")

# The plot method for "lm" objects (i.e., plot.lm()) generates a number of 
# diagnostic plots; see ?plot.lm for details (can you infer what the bottom 
# middle plot is telling us?)
par(mfrow = c(2, 3))
plot(delivery_fit, which = 1:6)

# The broom::augment() function adds a number of observation-level statistics
# to the training data which can be useful for plotting with ggplot2
delivery2 <- delivery_fit %>%
  broom::augment() %>%
  mutate(row_num = 1:n())
head(delivery2)

# Residual plots (ggplot2)
p1 <- ggplot(delivery2, aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  # geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  xlab("Fitted value") +
  ylab("Standardized residual") +
  theme_light()
p2 <- ggplot(delivery2, aes(sample = .std.resid)) +
  geom_qq(alpha = 0.75) +
  geom_qq_line(linetype = "dashed", color = "red2") +
  xlab("Theoretical quantile") +
  ylab("Sample quantile") +
  theme_light()
p3 <- ggplot(delivery2, aes(x = .fitted, y = .hat)) +
  geom_point(alpha = 0.75) +
  xlab("Fitted value") +
  ylab("Leverage (i.e., hat value)") +
  theme_light()
p4 <- ggplot(delivery2, aes(x = row_num, y = .std.resid)) +
  geom_point(alpha = 0.75) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  xlab("Index") +
  ylab("Standardized residual") +
  theme_light()
p5 <- ggplot(delivery2, aes(x = NumberofCases, y = .std.resid)) +
  geom_point(alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  # geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  xlab("Number of cases") +
  ylab("Standardized residual") +
  theme_light()
p6 <- ggplot(delivery2, aes(x = Distance, y = .std.resid)) +
  geom_point(alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  # geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  xlab("Distance (ft)") +
  ylab("Standardized residual") +
  theme_light()
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)
