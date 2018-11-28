## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE, servr.daemon = TRUE)

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

## ----prerequisites, eval=FALSE-------------------------------------------
## # List of required (CRAN) packages
## pkgs <- c(
##   "broom",      # for augment() function  #<<
##   "car",        # for vif() function  #<<
##   "GGally",     # for ggpairs() function
##   "ggplot2",    # for awesome graphics
##   "ggrepel",    # for geom_text_repel() function
##   "gridExtra",  # for grid.arrange() function
##   "pdp",        # for Boston housing data
##   "plotly",     # for interactive plots
##   "tibble"      # for nicer data frames
## )
## 
## # Install required (CRAN) packages
## for (pkg in pkgs) {
##   if (!requireNamespace(pkg)) {  # check if already installed
##     install.packages(pkg)  # install it
##   }
## }

## ----lets-go, echo=FALSE, out.width="70%"--------------------------------
set.seed(4); RBitmoji::plot_comic(my_id, tag = "lets go")

## ----example-diagnostics, echo=FALSE, fig.width=6, fig.asp=1, out.width="70%"----
# Simualated data sets
set.seed(101)
n <- 100
df1 <- tibble::tibble(
  x = runif(n, min = -5, max = 5),
  y = 1 + 2*x^2 + rnorm(n, sd = 10)
)
df2 <- tibble::tibble(
  x = runif(n, min = 1, max = 10),
  y = 1 + 4*x + rnorm(n, sd = 2*x)
)
df2 <- rbind(df2, data.frame(x = 2, y = 50))
df3 <- tibble::tibble(
  x = runif(n, min = 1, max = 10),
  y = 1 + 4*x + arima.sim(list(order = c(1,0,0), ar = 0.99), n = n, sd = 20)
)
df4 <- tibble::tibble(
  x = runif(n, min = 1, max = 10),
  y = 1 + 4*x + rlnorm(n, sd = 0.9)
)

# Fitted models
fit1 <- lm(y ~ x, data = df1)
fit2 <- lm(y ~ x, data = df2)
fit3 <- lm(y ~ x, data = df3)
fit4 <- lm(y ~ x, data = df4)

# Residuals
r1 <- residuals(fit1)
r2 <- rstandard(fit2)
r3 <- residuals(fit3)
r4 <- residuals(fit4)

# Residual plots
par(mfrow = c(2, 2))
plot(df1$x, r1, xlab = "X", ylab = "Residual", 
     main = "Misspecified mean structure")
abline(h = 0, lty = "dotted", col = "red2")
lines(lowess(df1$x, r1), col = "dodgerblue", lwd = 3)
plot(df2$x, r2, xlab = "X", ylab = "Residual",
     main = "Non-constant variance")
points(df2$x[101L], r2[101L], pch = 19, col = "red2")
abline(h = 0, lty = "dotted", col = "red2")
plot(residuals(fit3), xlab = "Index", ylab = "Residual", type = "l",
     main = "Serial correlation")
points(residuals(fit3), col = adjustcolor("black", alpha.f = 0.2))
abline(h = 0, lty = "dotted", col = "red2")
qqnorm(r4, main = "Non-normal errors", ylim = c(-5, 15))
qqline(r4, lty = "dotted", col = "red2")

## ----qqnorm-normal, fig.width=6, fig.asp=0.618, out.width="70%"----------
set.seed(101)
x <- rnorm(100)
qqnorm(x, main = "Normal data")
qqline(x, lty = "dotted", col = "red2")

## ----qqnorm-skew-right, fig.width=6, fig.asp=0.618, out.width="70%"------
set.seed(101)
x <- rlnorm(100)
qqnorm(x, main = "Skew right data")
qqline(x, lty = "dotted", col = "red2")

## ----qqnorm-skew-left, fig.width=6, fig.asp=0.618, out.width="70%"-------
set.seed(101)
x <- -rlnorm(100)
qqnorm(x, main = "Skew left data")
qqline(x, lty = "dotted", col = "red2")

## ----qqnorm-heavy tails, fig.width=6, fig.asp=0.618, out.width="70%"-----
set.seed(101)
x <- rt(100, df = 1)
qqnorm(x, main = "Heavy-tailed data")
qqline(x, lty = "dotted", col = "red2")

## ----never, echo=FALSE, out.width="50%"----------------------------------
my_id <- "1551b314-5e8a-4477-aca2-088c05963111-v1"
set.seed(4); RBitmoji::plot_comic(my_id, tag = "never")

## ----normality-tests, highlight.output=1:2-------------------------------
# Shapiro-Wilk test and sample size
set.seed(101)  # for reproducibility
x <- replicate(100, c(
  shapiro.test(rt(10, df = 40))$p.value,
  shapiro.test(rt(100, df = 40))$p.value,
  shapiro.test(rt(1000, df = 40))$p.value,
  shapiro.test(rt(5000, df = 40))$p.value
))
rownames(x) <- c("n=10", "n=100", "n=1000", "n=5000")
rowMeans(x < 0.05)

## ----normal-vs-t, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
x <- seq(from = -5, to = 5, length = 500)
y1 <- dnorm(x)
y2 <- dt(x, df = 40)
df <- data.frame(
  x = c(x, x), 
  y = c(y1, y2),
  Distribution = rep(c("Standard\nnormal", "t(df = 40)"), each = length(x))
)
set.seed(2); comic <- RBitmoji::get_comic(my_id, tag = "fuck")
ggplot(df, aes(x = x, y = y, color = Distribution)) +
  geom_line(size = 1, alpha = 0.5) +
  labs(x = expression(x), y = "Density") +
  annotation_raster(comic, xmin = -5, xmax = -1.25, ymin = 0.25, ymax = 0.4)

## ----say-what, echo=FALSE, out.width="50%"-------------------------------
set.seed(4); RBitmoji::plot_comic(my_id, tag = "say what")

## ----residual-patterms, echo=FALSE, out.width="70%"----------------------
knitr::include_graphics("images/residual-patterns.png")

## ----delivery-01---------------------------------------------------------
# Load the delivery data
url <- paste0("https://bgreenwell.github.io/",
              "uc-bana7052/data/delivery.csv")
delivery <- read.csv(url)
head(delivery, n = 5)  # print first 5 observations

## ----delivery-02---------------------------------------------------------
# Fit an MLR model
delivery_fit <- lm(DeliveryTime ~ NumberofCases + Distance, 
                   data = delivery)

# Fitted values
yhat <- fitted(delivery_fit)
# equivalent to `yhat <- predict(delivery_fit)`  #<<

# Residuals
rorig <- residuals(delivery_fit)  # ordinary
rstan <- rstandard(delivery_fit)  # studentized
rstud <- rstudent(delivery_fit)  # studentized deleted
press <- rstandard(delivery_fit, type = "predict")  # PRESS

## ----rstandard-----------------------------------------------------------
getAnywhere("rstandard.lm")

## ----rstudent------------------------------------------------------------
getAnywhere("rstudent.lm")

## ----leverage-influence, echo=FALSE, fig.width=6, fig.asp=0.5, out.width="90%"----
set.seed(101)
df1 <- tibble::tibble(
  x = c(1:5, 20),
  y = 1 + 2*x + rnorm(length(x))
)
set.seed(101)
df2 <- tibble::tibble(
  x = c(1:5),
  y = 1 + 2*x + rnorm(length(x))
)
df2 <- rbind(df2, data.frame(x = 20, y = 5))
df1$h <- paste0("h = ", round(hatvalues(lm(y ~ x, data = df1)), digits = 3))
df2$h <- paste0("h = ", round(hatvalues(lm(y ~ x, data = df2)), digits = 3))
p1 <- ggplot(df1, aes(x, y, label = h)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(nudge_x = 5, direction = "y", hjust = 0, 
                           alpha = 0.3, segment.alpha = 0.3) +
  ggtitle("Pure leverage point") +
  theme_light()
p2 <- ggplot(df2, aes(x, y, label = h)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(nudge_x = 5, direction = "y", hjust = 0, 
                           alpha = 0.3, segment.alpha = 0.3) +  ggtitle("Influential point") +
  theme_light()
gridExtra::grid.arrange(p1, p2, nrow = 1)

## ----boston-spm, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
GGally::ggpairs(pdp::boston[, c("cmedv", "lstat", "rm")])

## ----boston-broom-01, echo=FALSE-----------------------------------------
library(broom)
library(dplyr)
boston2 <- pdp::boston %>%
  lm(cmedv ~ lstat + rm, data = .) %>%
  augment() %>%
  mutate(row_num = 1:n())

## ----boston-broom-02-----------------------------------------------------
library(broom)
library(dplyr)
pdp::boston %>%
  lm(cmedv ~ lstat + rm, data = .) %>%
  broom::augment() %>%
  mutate(row_num = 1:n()) %>%
  head(5)

## ----boston-residual-plots-01, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
ggplot(boston2, aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  # geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  xlab("Fitted value") +
  ylab("Standardized residual") +
  theme_light()

## ----boston-residual-plots-02, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
ggplot(boston2, aes(sample = .std.resid)) +
  geom_qq(alpha = 0.3) +
  geom_qq_line(linetype = "dashed", color = "red2") +
  xlab("Theoretical quantile") +
  ylab("Sample quantile") +
  theme_light()

## ----boston-residual-plots-03, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
ggplot(boston2, aes(x = row_num, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  xlab("Index") +
  ylab("Standardized residual") +
  theme_light()

## ----boston-residual-plots-04, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
ggplot(boston2, aes(x = lstat, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  ylab("Standardized residual") +
  theme_light()

## ----boston-residual-plots-05, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
ggplot(boston2, aes(x = rm, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  ylab("Standardized residual") +
  theme_light()

## ----chull, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"-----
hull <- delivery[chull(delivery$NumberofCases, delivery$Distance), ]
ggplot(delivery, aes(x = NumberofCases, y = Distance)) +
  geom_polygon(data = hull, fill = "red", alpha = 0.1) +
  geom_point(size = 3) +
  geom_point(data = delivery[c(9, 22), ], size = 3, col = "red2") +
  theme_light()

## ----delivery-hatvalue, fig.width=6, fig.asp=0.618, out.width="70%"------
# Which training observations might be considered as outliers?
h <- hatvalues(delivery_fit)
plot(h, type = "h", ylim = extendrange(h, f = 0.15))
abline(h = 2 * 3 / nrow(delivery), lty = "dotted")  #<<
text(h, labels = seq_len(nrow(delivery)), pos = 3, col = "red2")

## ------------------------------------------------------------------------
# Determine outlyingness of new observations
X <- model.matrix(delivery_fit)
head(X)
X_new <- rbind(c(1, 10, 1000), c(1, 40, 3))
(h_new <- diag(X_new %*% solve(t(X) %*% X) %*% t(X_new)))
h_new > max(h)

## ----base-residual-plots-01, eval=FALSE----------------------------------
## # Set up plotting grid
## par(mfrow = c(2, 3))  # three rows and two columns
## plot(delivery_fit, which = 1:6)

## ----base-residual-plots-02, echo=FALSE, fig.width=6, fig.asp=2/3, out.width="100%"----
# Set up plotting grid
par(mfrow = c(2, 3))  # three rows and two columns
plot(delivery_fit, which = 1:6)

## ----heart-01------------------------------------------------------------
# Load required packages
library(car)     # for vif() function
library(plotly)  # for interactive plotting

# Load the data
url <- "https://bgreenwell.github.io/uc-bana7052/data/heart"
heart <- read.table(url, header = TRUE)
head(heart, 5)  

## ----heart-02, fig.width=6, fig.asp=0.618, out.width="100%"--------------
GGally::ggpairs(heart)  # scatterplot matrix

## ----heart-03, out.width="70%"-------------------------------------------
# Correlation matrix
(cor_mat <- cor(heart))

## ----heart-04, echo=FALSE, out.width="100%"------------------------------
# Interactive 3-D scatterplot
p <- plot_ly(heart, x = ~Height, y = ~Weight, z = ~Length) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Weight (pounds)"),
                      yaxis = list(title = "Height (inches)"),
                      zaxis = list(title = "Length (cm)")))
p

## ----heart-05, highlight.output=c(12:13, 18:19)--------------------------
# Fit a linear model
heart_fit <- lm(Length ~ Height + Weight, data = heart)
summary(heart_fit)

## ----heart-06------------------------------------------------------------
# Compute variance inflation factors
vif(heart_fit)  #<<

## ----boston-vif----------------------------------------------------------
# Compute VIF scores for the Boston housing example
pdp::boston %>%
  lm(cmedv ~ lstat + rm, data = .) %>%
  vif()

## ----quittin-time, echo=FALSE, out.width="60%"---------------------------
RBitmoji::plot_comic(my_id, tag = "quittin")

