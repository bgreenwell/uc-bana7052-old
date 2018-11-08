## ----prerequisites, eval=FALSE-------------------------------------------
# List of required (CRAN) packages
pkgs <- c(
  "GGally",  # for gggplot2 extensions
  "pdp",     # for (corrected) Boston housing data
  "plotly",  # for interactive plots
  "tibble",  # for nicer data frames
  "vip"      # for variable importance plots
)

# Install required (CRAN) packages
for (pkg in pkgs) {
  if (!(pkg %in% installed.packages()[, "Package"])) {
    install.packages(pkg)
  }
}

# Load required packages
library(dplyr)

## ----mlr-3d-df-01, fig.width=6, fig.asp=0.618, out.width="100%"----------
# Simulate data from an MLR model
set.seed(101)  # for reproducibility
n <- 50
df <- tibble::tibble(
  x1 = runif(n),
  x2 = runif(n),
  y = 1 + 2*x1 - 3*x2 + rnorm(n, sd = 1)  #<<
)
head(df, n = 3)  # print first few rows

## ----mlr-3d-df-02-01, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Construct a scatterplot matrix
pairs(df, cex = 1.2, pch = 19, col = adjustcolor("darkred", alpha.f = 0.5))

## ----mlr-3d-df-04, echo=FALSE, out.width="100%"--------------------------
# Load required packages
library(plotly)  # for interactive plotting

# Draw (interactive) 3-D scatterplot
plot_ly(data = df, x = ~x1, y = ~x2, z = ~y, mode = "markers", 
        type = "scatter3d",
        marker = list(opacity = 0.7, symbol = 1, size = 5, color = "black")) %>%
  layout(
    scene = list(
      aspectmode = "manual", aspectratio = list(x = 1, y = 1, z = 1),
      xaxis = list(title = "X1", range = c(0, 1)),
      yaxis = list(title = "X2", range = c(0, 1)),
      zaxis = list(title = "Y")
    )
  )

## ----mlr-3d-df-06, echo=FALSE, out.width="100%"--------------------------
# Fit an MLR model to the simulated data
fit <- lm(y ~ x1 + x2, data = df)
betas <- coef(fit)                

# Generate predictions over a fine grid  #<<
.x1 <- .x2 <- seq(from = 0, to = 1, length = 50)
yhat <- t(outer(.x1, .x2, function(x1, x2) {
  betas[1] + betas[2]*x1 + betas[3]*x2
}))

# Draw (interactive) 3-D scatterplot with fitted mean response
plot_ly(x = ~.x1, y = ~.x2, z = ~yhat, 
        type = "surface", opacity = 0.7) %>%
  add_trace(data = df, x = ~x1, y = ~x2, z = ~y, 
            mode = "markers", 
            type = "scatter3d",
            marker = list(opacity = 0.7, symbol = 1, 
                          size = 5, color = "black")) %>%
  layout(
    scene = list(
      aspectmode = "manual", 
      aspectratio = list(x = 1, y = 1, z = 1),
      xaxis = list(title = "X1", range = c(0, 1)),
      yaxis = list(title = "X2", range = c(0, 1)),
      zaxis = list(title = "Y")
    )
  )

## ----delivery-01---------------------------------------------------------
# Load the delivery data
url <- "https://bgreenwell.github.io/uc-bana7052/data/delivery.csv"
delivery <- read.csv(url)
head(delivery, n = 5)  # print first 5 observations

## ----delivery-ggpairs-02, echo=FALSE, fig.width=5, fig.height=5, out.width="100%"----
GGally::ggpairs(
  data = delivery[, -1]  #<<
)  

## ----delivery-splom-02, echo=FALSE, fig.width=5, fig.height=5, out.width="100%"----
lattice::splom(
  x = delivery[, -1],  #<<
  type = c("p", "smooth"), 
  pch = 19, 
  col = "dodgerblue2", 
  lty = "dotted", 
  alpha = 0.6
)

## ----delivery-lm-01------------------------------------------------------
# Fit a multile linear regression model
delivery_fit <- lm(DeliveryTime ~ NumberofCases + 
                     Distance, data = delivery)

# Extract estimated coefficients
coef(delivery_fit)  

## ----delivery-lm-02------------------------------------------------------
# Fit a multile linear regression model
delivery_fit <- lm(DeliveryTime ~ ., 
                   data = delivery)

# Extract estimated coefficients
coef(delivery_fit)  

## ----delivery-lm-03------------------------------------------------------
# Fit a multile linear regression model
delivery <- subset(delivery, select = -Index)  #<<
(delivery_fit <- lm(DeliveryTime ~ ., data = delivery))

## ----delivery-lm-04------------------------------------------------------
round(coef(delivery_fit), digits = 3)  #<<

## ----delivery-matrix-wrong-----------------------------------------------
xnames <- c("NumberofCases", "Distance")
X <- data.matrix(delivery[, xnames])
head(X)
Y <- delivery$DeliveryTime
solve(t(X) %*% X) %*% t(X) %*% Y  #<<

## ----delivery-matrix-right-----------------------------------------------
X <- model.matrix(~ NumberofCases + Distance, 
                  data = delivery)
head(X)
Y <- delivery$DeliveryTime
solve(t(X) %*% X) %*% t(X) %*% Y  #<<

## ----delivery-output-----------------------------------------------------
# Extract fitted values and residuals
.fitted <- fitted(delivery_fit)
.resids <- residuals(delivery_fit)
head(cbind(delivery, .fitted, .resids))

## ----boston-load, eval=FALSE---------------------------------------------
## data(boston, package = "pdp")

## ----boston-spm-01, fig.width=6, fig.asp=0.618, out.width="100%"---------
pairs(pdp::boston[, c("cmedv", "lstat", "rm")], pch = 19,
      col = "black")

## ----boston-spm-02, fig.width=6, fig.asp=0.618, out.width="100%"---------
pairs(pdp::boston[, c("cmedv", "lstat", "rm")], pch = 19, 
      col = adjustcolor("black", alpha.f = 0.2))  #<<

## ----boston-mlr----------------------------------------------------------
coef(boston_fit <- lm(cmedv ~ lstat + rm, 
                      data = pdp::boston))

## ----boston-predict------------------------------------------------------
predict(
  object = boston_fit, 
  newdata = data.frame(lstat = 20, rm = 6),  #<<
  se.fit = TRUE,  #<<
  interval = "confidence"  #<<
)

## ----delivery-ftest-01, highlight.output=c(12:13, 19)--------------------
summary(delivery_fit)

## ----delivery-ftest-02, highlight.output=7-------------------------------
# Manually construct F-test
delivery_fit_reduced <- lm(DeliveryTime ~ 1, data = delivery)
anova(delivery_fit_reduced, delivery_fit)

## ----delivery-rsquared-01, highlight.output=18---------------------------
summary(delivery_fit)

## ----delivery-rsquared-02, highlight.output=c(14:16, 21)-----------------
# Simulate new columns at random
set.seed(101)  # for reproducibility
delivery2 <- delivery
delivery2$X3 <- rnorm(nrow(delivery))
delivery2$X4 <- rnorm(nrow(delivery))
delivery2$X5 <- rnorm(nrow(delivery))

# Update the fitted mode
delivery2_fit <- lm(DeliveryTime ~ ., data = delivery2)

# Print model summary
summary(delivery2_fit)

## ----boston-ftest, highlight.output=9------------------------------------
# General F-test
data(boston, package = "pdp")  # Load the data
fit1 <- lm(cmedv ~ lstat + rm + lon, data = boston)  # full model
fit2 <- lm(cmedv ~ lstat + rm, data = boston)  # reduced model
anova(fit2, fit1)  # reduced model goes first  #<<

## ----delivery-inference--------------------------------------------------
# Print summary of the model
summary(delivery_fit)  # SEs and marginal tests  #<<

# Construct 95% CIs for the coefficients
confint(delivery_fit, level = 0.95)
