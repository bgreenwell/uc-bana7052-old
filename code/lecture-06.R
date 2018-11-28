## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE, servr.daemon = TRUE, 
        crayon.enabled = FALSE)

# Global chunk options
knitr::opts_chunk$set(
  cache = TRUE,
  echo = TRUE,
  dev = "svglite",
  fig.align = "center",
  message = FALSE,
  warning = FALSE,
  error = FALSE
)

# Bitmoji id
my_id <- "1551b314-5e8a-4477-aca2-088c05963111-v1"

# Load required packages
library(ggplot2)

## ----prerequisites-------------------------------------------------------
# List of required (CRAN) packages
pkgs <- c(
  "car",           # for subsets() function
  "GGally",        # for ggpairs() function
  "ggplot2",       # for awesome graphics
  "gridExtra",     # for grid.arrange() function
  "leaps",         # for regsubsets() function
  "pdp",           # for Boston housing data set
  "plotly",        # for interactive plots
  "RBitmoji",      # ummm, just because
  "scales",        # for comma() function
  "SMPracticals",  # for Hald's cement data
  "tibble"         # for nicer data frames
)

# Install required (CRAN) packages
for (pkg in pkgs) {
  if (!(pkg %in% installed.packages()[, "Package"])) {
    install.packages(pkg)
  }
}

## ----lets-go, echo=FALSE, out.width="70%"--------------------------------
set.seed(4); RBitmoji::plot_comic(my_id, tag = "lets go")

## ----boston-plotly-01, echo=FALSE, out.width="100%"----------------------
# Load required packages
library(plotly)

# Draw (interactive) 3-D scatterplot w/ fitted regression plane
plot_ly(data = pdp::boston, x = ~lstat, y = ~rm, z = ~cmedv, 
        mode = "markers", type = "scatter3d",
        marker = list(opacity = 0.3, symbol = 1, 
                      size = 5, color = "black")) %>%
  layout(
    scene = list(
      aspectmode = "manual", 
      aspectratio = list(x = 1, y = 1, z = 1)
    )
  )

## ----boston-plotly-02, echo=FALSE, out.width="100%"----------------------
# Draw (interactive) 3-D scatterplot w/ fitted regression plane
fit <- lm(cmedv ~ lstat + rm, data = pdp::boston)
betas <- coef(fit)                
lstat <- seq(from = 1.73, to = 37.97, length = 50)
rm <- seq(from = 3.561, to = 8.780, length = 50)
yhat <- t(outer(lstat, rm, function(x1, x2) {
  betas[1] + betas[2]*x1 + betas[3]*x2
}))
plot_ly(x = ~lstat, y = ~rm, z = ~yhat, 
        type = "surface", opacity = 0.7, showlegend = FALSE) %>%
  add_trace(data = pdp::boston, x = ~lstat, y = ~rm, z = ~cmedv, 
            mode = "markers",
            type = "scatter3d",
            marker = list(opacity = 0.7, symbol = 1, 
                          size = 5, color = "black")) %>%
  layout(title = "Without interaction",
    scene = list(
      aspectmode = "manual", 
      aspectratio = list(x = 1, y = 1, z = 1),
      zaxis = list(title = "cmedv")
    )
  )

## ----boston-plotly-03, echo=FALSE, out.width="100%"----------------------
# Draw (interactive) 3-D scatterplot w/ fitted regression plane (curvature)
fit2 <- lm(cmedv ~ lstat * rm, data = pdp::boston)
betas2 <- coef(fit2)  
yhat2 <- t(outer(lstat, rm, function(x1, x2) {
  betas2[1] + betas2[2]*x1 + betas2[3]*x2 + betas2[4]*x1*x2
}))
plot_ly(x = ~lstat, y = ~rm, z = ~yhat2, 
        type = "surface", opacity = 0.7, showlegend = FALSE) %>%
  add_trace(data = pdp::boston, x = ~lstat, y = ~rm, z = ~cmedv, 
            mode = "markers", 
            type = "scatter3d",
            marker = list(opacity = 0.7, symbol = 1, 
                          size = 5, color = "black")) %>%
  layout(title = "With interaction",
    scene = list(
      aspectmode = "manual", 
      aspectratio = list(x = 1, y = 1, z = 1),
      zaxis = list(title = "cmedv")
    )
  )


## ----contours, echo=FALSE, fig.width=6, fig.asp=0.5, out.width="100%"----
nd <- expand.grid("lstat" = lstat, "rm" = rm)
nd$yhat <- predict(fit, newdata = nd)
nd$yhat2 <- predict(fit2, newdata = nd)
p1 <- lattice::levelplot(yhat ~ lstat * rm, data = nd, contour = TRUE,
                         col = "white", col.regions = viridis::magma(100))
p2 <- lattice::levelplot(yhat2 ~ lstat * rm, data = nd, contour = TRUE, 
                         col = "white", col.regions = viridis::magma(100))
gridExtra::grid.arrange(p1, p2, ncol = 2)

## ----overfitting-linear-regression, echo=FALSE, fig.width=7, fig.height=7/3, out.width="100%"----
# Simulate some data 
n <- 100
set.seed(8451)
df <- tibble::tibble(
  x = runif(n, min = -2, max = 2),
  y = rnorm(n, mean = 1 + 2*x + x^2, sd = 1)
)
p <- ggplot(df, aes(x, y)) + 
  geom_point(alpha = 0.5) + 
  theme_light()
p1 <- p + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  ggtitle("Under fitting")
p2 <- p + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  ggtitle("Just right?")
p3 <- p + 
  geom_smooth(method = "loess", span = 0.075, se = FALSE) +
  ggtitle("Over fitting")
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

## ---- echo=FALSE---------------------------------------------------------
fit <- lm(cmedv ~ ., data = pdp::boston)

## ----all-subsets, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
tib <- tibble::tibble(x = 1:20, y = 2^x / 1000)
ggplot(tib, aes(x, y)) + 
  geom_line() +
  geom_point(color = "dodgerblue2", size = 2) +
  labs(x = "Number of predictors", y = "Number of possible subsets (thousands)") +
  theme_light()

## ----cement-load---------------------------------------------------------
# Load the Hald cement data
data(cement, package = "SMPracticals")
head(cement)  # see ?cement for details

## ----cement-cor, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Scatterplot matrix
GGally::ggpairs(cement, mapping = aes(alpha = 0.5)) + theme_light()

## ----cement-leaps-01-----------------------------------------------------
# Load required packages
library(leaps)

# All subsets regression (main effects only)
a1 <- regsubsets(y ~ ., data = cement, 
                 nbest = 6, nvmax = 4)
# why 6 and 4?  #<<

## ----cement-leaps-02, echo=FALSE, fig.width=6, fig.asp=0.9, out.width="80%"----
# Plot results from all subsets regression
plot(a1, scale = "bic")

## ----cement-leaps-03, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Load required packages
library(ggplot2)

# Gather results
res1 <- data.frame(
  "nvar" = apply(summary(a1)$which, 1, FUN = function(x) sum(x) - 1),
  "bic" = summary(a1)$bic,
  "adjr2" = summary(a1)$adjr2
)

# Plot results
p1 <- ggplot(res1, aes(x = nvar, y = bic)) +
  geom_point(alpha = 0.5, size = 2, color = "darkred") +
  stat_summary(fun.y = min, geom = "line", alpha = 0.5, linetype = "dashed") +
  theme_light() +
  labs(x = "Number of predictors", y = "BIC")
p2 <- ggplot(res1, aes(x = nvar, y = adjr2)) +
  geom_point(alpha = 0.5, size = 2, color = "darkgreen") +
  stat_summary(fun.y = max, geom = "line", alpha = 0.5, linetype = "dashed") +
  theme_light() +
  labs(x = "Number of predictors", y = "Adjusted R-squared")
gridExtra::grid.arrange(p1, p2, nrow = 2)

## ----cement-best-01, echo=FALSE------------------------------------------
# Summarize best model
summary(best1 <- lm(y ~ x1 + x2, data = cement))

## ----cement-best-02, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Plot residuals from best model
par(mfrow = c(1, 2))
plot(best1, which = 1:2)

## ----numSubsets----------------------------------------------------------
numSubsets <- function(x, max.int = 1) {
  if (max.int > x) {
    stop("`max.int` cannot be larger than ", 
         x, ".", call. = FALSE)
  }
  x <- as.integer(x)
  max.int <- as.integer(max.int)
  res <- 0
  for (i in seq_len(max.int)) {
    res <- res + choose(n = x, k = i)
  }
  2 ^ res
}

## ----all-subsets-int-----------------------------------------------------
# How many possible subsets if we allow for 
# interactions?
x <- c(numSubsets(4, max.int = 1),
       numSubsets(4, max.int = 2),
       numSubsets(4, max.int = 3),
       numSubsets(4, max.int = 4))
scales::comma(x)

## ----cement-leaps-04-----------------------------------------------------
# All subsets regression (with two-way interactions)
a2 <- regsubsets(y ~ .^2, data = cement, 
                 nbest = 40, nvmax = 1000)

## ----cement-leaps-05, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Gather results
res2 <- data.frame(
  "nvar" = apply(summary(a2)$which, 1, FUN = function(x) sum(x) - 1),
  "bic" = summary(a2)$bic,
  "adjr2" = summary(a2)$adjr2
)

# Plot results
p3 <- ggplot(res2, aes(x = nvar, y = bic)) +
  geom_point(alpha = 0.5, size = 2, color = "darkred") +
  stat_summary(fun.y = min, geom = "line", alpha = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks = 1:10) +
  theme_light() +
  labs(x = "Number of predictors", y = "BIC")
p4 <- ggplot(res2, aes(x = nvar, y = adjr2)) +
  geom_point(alpha = 0.5, size = 2, color = "darkgreen") +
  stat_summary(fun.y = max, geom = "line", alpha = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks = 1:10) +
  theme_light() +
  labs(x = "Number of predictors", y = "Adjusted R-squared")
gridExtra::grid.arrange(p3, p4, nrow = 2)

## ----cement-leaps-06, echo=FALSE-----------------------------------------
# Summarize best model
id <- which.min(summary(a2)$bic)
trms <- names(which(summary(a2)$which[id, ])[-1L])
form <- as.formula(paste("y ~", paste(trms, collapse = "+")))
round(summary(best2 <- lm(form, data = cement))$coefficients, digits = 3)

## ----boston-load---------------------------------------------------------
# Load the Boston housing data
data(boston, package = "pdp")

# Print first few observations
head(tibble::as_tibble(boston), n = 5)

## ----boston-numSubsets---------------------------------------------------
# How many subsets (main effects only)
scales::comma(numSubsets(15, 1))

# How many subsets (two-way interactions)
scales::comma(numSubsets(15, 2))
# Over 1 undecillion!

## ----dayum, echo=FALSE, out.width="20%"----------------------------------
RBitmoji::plot_comic(my_id, tag = "daaayum")

## ----press-function------------------------------------------------------
# Function to compute the PRESS statistic (a form of 
# cross-validation). Note: smaller is better!
PRESS <- function(object, ...) {
  if(!missing(...)) {
    res <- sapply(list(object, ...), FUN = function(x) {
      sum(rstandard(x, type = "predictive") ^ 2)
    })
    names(res) <- as.character(match.call()[-1L])
    res
  } else {
    sum(rstandard(object, type = "predictive") ^ 2)
  }
}

## ----modelMetrics-function-----------------------------------------------
# Function to compute various model metrics
modelMetrics <- function(object, ...) {
  if(!missing(...)) {
    res <- sapply(list(object, ...), FUN = function(x) {
      c("AIC" = AIC(x), "BIC" = BIC(x), 
        "adjR2" = summary(x)$adj.r.squared,
        "RMSE"  = sigma(x), "PRESS" = PRESS(x), 
        "nterms" = length(coef(x)))
    })
    colnames(res) <- as.character(match.call()[-1L])
    res
  } else {
    c("AIC" = AIC(object), "BIC" = BIC(object), 
      "adjR2" = summary(object)$adj.r.squared, 
      "RMSE"  = sigma(object), "PRESS" = PRESS(object),
      "nterms" = length(coef(object)))
  }
}

## ----boston-be-----------------------------------------------------------
# Backward elimination --------------------------

# Note that setting `k = 2` in the call to step(), which 
# is the default, corresponds to using AIC; below we set 
# it to `k = ln(n)`, which corresponds to using BIC!

# Main effects only (i.e., no interactions)
fit_max_1 <- lm(cmedv ~ ., data = boston)  #<<
be_1 <- step(fit_max_1, direction = "backward", 
             trace = 0, k = log(nrow(boston)))

# Main effects and two-way interactions
fit_max_2 <- lm(cmedv ~ .^2, data = boston)  #<<
be_2 <- step(fit_max_2, direction = "backward", 
             trace = 0, k = log(nrow(boston)))

## ----boston-fs-----------------------------------------------------------
# Forward selection -----------------------------

# Main effects only (i.e., no interactions)
fit_min <- lm(cmedv ~ 1, data = boston)
fs_1 <- step(fit_min, direction = "forward", 
             scope = list(lower = fit_min,     #<<
                          upper = fit_max_1),  #<<
             trace = 0, k = log(nrow(boston)))

# Main effects and two-way interactions
fs_2 <- step(fit_min, direction = "forward", 
             scope = list(lower = fit_min,     #<<
                          upper = fit_max_2),  #<<
             trace = 0, k = log(nrow(boston)))

## ----boston-ss-----------------------------------------------------------
# Stepwise selection ----------------------------

# Main effects only (i.e., no interactions)
ss_1 <- step(be_1, direction = "both", 
             scope = list(lower = fit_min,     #<<
                          upper = fit_max_1),  #<<
             trace = 0, k = log(nrow(boston)))

# Main effects and two-way interactions
ss_2 <- step(be_2, direction = "both", 
           scope = list(lower = fit_min,     #<<
                        upper = fit_max_2),  #<<
           trace = 0, k = log(nrow(boston)))

## ----boston-compare-models, highlight.output = 6:7-----------------------
# Compare models
res <- modelMetrics(be_1, be_2, fs_1, fs_2, ss_1, ss_2)
round(res, digits = 3)

## ----boston-ss-2---------------------------------------------------------
summary(ss_2)

## ----boston-fs-2---------------------------------------------------------
summary(fs_2)

## ----quittin-time, echo=FALSE, out.width="60%"---------------------------
RBitmoji::plot_comic(my_id, tag = "quittin")

