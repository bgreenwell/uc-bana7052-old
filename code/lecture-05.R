## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE, servr.daemon = TRUE)

# Global chunk options
knitr::opts_chunk$set(
  cache = FALSE,
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
##   "ggplot2",  # for drawing nicer graphics
##   "lattice",  # for drawing nicer graphics
##   "MASS",     # for boxcox() function
##   "tibble"    # for nicer data frames
## )
## 
## # Install required (CRAN) packages
## for (pkg in pkgs) {
##   if (!(pkg %in% installed.packages()[, "Package"])) {
##     install.packages(pkg)
##   }
## }

## ----lets-go, echo=FALSE, out.width="70%"--------------------------------
set.seed(4); RBitmoji::plot_comic(my_id, tag = "lets go")

## ----cutting-tool-01, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="70%"----
n <- 20
set.seed(1213)
df <- tibble::tibble(
  x1 = rnorm(n),
  x2 = rep(c("Tool A", "Tool B"), each = n/2),
  y = 1 + 2*x1 + 3*ifelse(x2 == "Tool A", yes = 0, no = 1) + rnorm(n)
)
ggplot(df, aes(x = x1, y = y, color = as.factor(x2))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  theme_light() +
  labs(x = expression(X[1]), y = expression(Y)) +
  annotate("text", x = -1.5, y = 6, label = "Hypothetical data", size = 6) +
  guides(color = guide_legend(title = "")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

## ----indicator-variables, echo=FALSE, out.width="100%"-------------------
knitr::include_graphics("images/indicator-variables.png")

## ----dummy-encoding, echo=FALSE, out.width="100%"------------------------
knitr::include_graphics("images/dummy-encoding.png")

## ----categorical-variables-01--------------------------------------------
# Categorical variable
(dow <- c("Mon", "Tue", "Wed", "Thu", "Fri"))
class(dow)

## ----categorical-variables-02--------------------------------------------
# Coerce to a factor; needed for use in lm()
(dow2 <- as.factor(dow))
class(dow2)

## ----categorical-variables-03--------------------------------------------
# R will handle dummy encoding for you
model.matrix( ~ dow2)

## ----cutting-tool-02-----------------------------------------------------
# Load required packages
library(broom)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lattice)

# Load the data
url <- paste0("https://bgreenwell.github.io/",
              "uc-bana7052/data/cutting_tool.csv")
cutting_tool <- read.csv(url)

## ----cutting-tool-03-----------------------------------------------------
# Inspect data
head(cutting_tool)

## ----cutting-tool-04-----------------------------------------------------
# Check column types
sapply(cutting_tool, class)

# Inspect ToolType column
head(cutting_tool$ToolType)

## ----cutting-tool-05-----------------------------------------------------
# How will R encode ToolType?
head(model.matrix( ~ rpm + ToolType, 
                   data = cutting_tool))

## ----cutting-tool-06, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
ggplot(cutting_tool, aes(x = rpm, y = Hour, group = ToolType)) +
  geom_point(aes(colour = ToolType), size = 3) +
  # geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
  #             color = "black") +
  theme_light()

## ----cutting-tool-07, highlight.output=12:13-----------------------------
# Regression model
summary(fit <- lm(Hour ~ rpm + ToolType, data = cutting_tool))

## ----cutting-tool-08, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# FItted regression line(s)
ggplot(augment(fit), aes(x = rpm, y = .fitted)) +
  geom_point(aes(y = Hour, colour = ToolType), size = 3) +
  geom_line(aes(group = ToolType), color = "black") +
  labs(x = "rpm", y = "Hour") +
  theme_light()

## ----thinking-picard, echo=FALSE, out.width="100%"-----------------------
knitr::include_graphics("images/thinking-picard.jpg")

## ----cutting-tool-unequal-slopes, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="70%"----
url <- "https://bgreenwell.github.io/uc-bana7052/data/cutting_tool.csv"
cutting_tool <- read.csv(url)
ggplot(cutting_tool, aes(x = rpm, y = Hour, color = ToolType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  theme_light()

## ----fruitfly-01---------------------------------------------------------
url <- "https://bgreenwell.github.io/uc-bana7052/data/fruitfly.csv"
fruitfly <- read.csv(url)
tibble::as_tibble(fruitfly)

## ----fruitfly-02, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
ggplot(fruitfly, aes(x = thorax, y = lifespan, color = group)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_light() +
  labs(x = "Thorax length (mm)", y = "Lifespan (days)") +
  theme(legend.title = element_blank(), legend.position = c(0.1, 0.8))

## ----fruitfly-03---------------------------------------------------------
# Two-sample t-test
t.test(lifespan ~ group, data = fruitfly)

# Linear model (equivalent)
summary(lm(lifespan ~ group, data = fruitfly))

## ----fruitfly-04---------------------------------------------------------
# Full model
fit1 <- lm(lifespan ~ thorax + group + thorax * group, 
           data = fruitfly)

# Print model summary
summary(fit1)

## ----fruitfly-05---------------------------------------------------------
# Thorax only
fit2 <- lm(lifespan ~ thorax, data = fruitfly)

# Print model summary
summary(fit2)

# Compare models
anova(fit2, fit1)

## ----fruitfly-06---------------------------------------------------------
# Parallel regression lines
fit3 <- lm(lifespan ~ thorax + group, data = fruitfly)

# Print model summary
summary(fit3)

# Compare models
anova(fit3, fit1)

## ----fruitfly-07, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
library(lattice)
xyplot(lifespan ~ thorax, groups = group, data = fruitfly, pch = 19, 
       alpha = 0.5, type = c("p", "r"), xlab = "Thorax length (mm)",
       ylab = "Lifespan (days)", auto.key = list(corner = c(0, 1)))

## ----residuals-vs-x, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
set.seed(101)
n <- 100
df <- tibble::tibble(
  x = runif(n, min = 1, max = 5),
  y = 1 + 4*x + rnorm(n, sd = x)
)
fit <- lm(y ~ x, data = df)
df$r <- residuals(fit)
df$f <- fitted(fit)
ggplot(df, aes(f, r)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted value", y = "Residual", 
       title = "Error variance increases with E(Y)") +
  theme_light()

## ----utility-01----------------------------------------------------------
# Load the utility data
url <- paste0("https://bgreenwell.github.io/",
              "uc-bana7052/data/utility.csv")
utility <- read.csv(url)
head(utility, n = 5)  # print first 5 observations 

## ----utility-02, eval=FALSE----------------------------------------------
## # Scatterplot
## plot(Demand ~ Usage, data = utility, pch = 19, las = 1,
##      col = adjustcolor("darkblue", alpha.f = 0.5))
## 
## # Fitted regression line
## abline(fit <- lm(Demand ~ Usage, data = utility),
##        lwd = 2,
##        col = adjustcolor("darkred", alpha.f = 0.5))

## ----utility-03, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Scatterplot
plot(Demand ~ Usage, data = utility, pch = 19, las = 1,
     col = adjustcolor("darkblue", alpha.f = 0.5))
abline(fit <- lm(Demand ~ Usage, data = utility), 
       lwd = 2,
       col = adjustcolor("darkred", alpha.f = 0.5))

## ----utility-04, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Residual plots
# par(mfrow = c(1, 2))
plot(
  x = fitted(fit), 
  y = rstudent(fit),
  pch = 19,
  las = 1,
  col = adjustcolor("darkblue", alpha.f = 0.5),
  xlab = "Fitted value",
  ylab = "Studentized residual"
)
abline(h = 0, lty = 2)
# plot(
#   x = utility$Usage, 
#   y = residuals(fit),
#   pch = 19,
#   las = 1,
#   col = adjustcolor("darkblue", alpha.f = 0.5),
#   xlab = "Usage (KWH)",
#   ylab = "Residual"
# )
# abline(h = 0, lty = 2)

## ----utility-05, eval=FALSE----------------------------------------------
## # Scatterplot
## plot(sqrt(Demand) ~ Usage, data = utility,
##      pch = 19, las = 1,
##      col = adjustcolor("darkblue", alpha.f = 0.5))
## 
## # Fitted regression line
## abline(fit <- lm(sqrt(Demand) ~ Usage, data = utility),  #<<
##        lwd = 2,
##        col = adjustcolor("darkred", alpha.f = 0.5))

## ----utility-06, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Scatterplot
plot(sqrt(Demand) ~ Usage, data = utility, pch = 19, las = 1,
     col = adjustcolor("darkblue", alpha.f = 0.5))
abline(fit <- lm(sqrt(Demand) ~ Usage, data = utility), 
       lwd = 2,
       col = adjustcolor("darkred", alpha.f = 0.5))

## ----utility-07, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Residual plots
# par(mfrow = c(1, 2))
plot(
  x = fitted(fit), 
  y = rstudent(fit),
  pch = 19,
  las = 1,
  col = adjustcolor("darkblue", alpha.f = 0.5),
  xlab = "Fitted value",
  ylab = "Studentized residual"
)
abline(h = 0, lty = 2)
# plot(
#   x = utility$Usage, 
#   y = residuals(fit),
#   pch = 19,
#   las = 1,
#   col = adjustcolor("darkblue", alpha.f = 0.5),
#   xlab = "Usage (KWH)",
#   ylab = "Residual"
# )
# abline(h = 0, lty = 2)

## ----boxcox-01, fig.wdith=6, fig.asp=0.618, out.width="60%"--------------
# Find optimal lambda value via ML estimation
bc <- MASS::boxcox(Demand ~ Usage, data = utility)
(lambda <- bc$x[which.max(bc$y)])

## ----boxcox-02, fig.wdith=6, fig.asp=0.618, out.width="80%"--------------
# Scatterplot and fitted model
utility$Demand2 <- (utility$Demand ^ lambda - 1) / lambda
plot(Demand2 ~ Usage, data = utility, pch = 19, las = 1,
     col = adjustcolor("darkblue", alpha.f = 0.5))
abline(fit <- lm(Demand2 ~ Usage, data = utility), 
       lwd = 2,
       col = adjustcolor("darkred", alpha.f = 0.5))

## ----boxcox-03, fig.wdith=7, fig.asp=0.5, out.width="100%"---------------
par(mfrow = c(1, 2))  # side-by-side plots
plot(fit, which = 1:2)

## ----prototype-01, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="80%"----
curve(log10(x), xlab = "X", ylab = "Y", main = "General trend", 
      lwd = 10, axes = FALSE, lend = 2)
axis(1, labels = FALSE, tick = TRUE)
axis(2, labels = FALSE, tick = TRUE)

## ----prototype-02, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="80%"----
curve(x^2, xlab = "X", ylab = "Y", main = "General trend", 
      lwd = 10, axes = FALSE, lend = 2)
axis(1, labels = FALSE, tick = TRUE)
axis(2, labels = FALSE, tick = TRUE)

## ----prototype-03, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="80%"----
curve(1/x, xlab = "X", ylab = "Y", main = "General trend", 
      lwd = 10, axes = FALSE, xlim = c(0.1, 0.5), lend = 2)
axis(1, labels = FALSE, tick = TRUE)
axis(2, labels = FALSE, tick = TRUE)

## ----windmill-01---------------------------------------------------------
# Load the windmill data
url <- paste0("https://bgreenwell.github.io/",
              "uc-bana7052/data/windmill.csv")
windmill <- read.csv(url)
head(windmill, n = 5)  # print first 5 observations 

## ----windmill-02, eval=FALSE---------------------------------------------
## # Scatterplot
## plot(Output ~ Velocity, data = windmill, pch = 19, las = 1,
##      col = adjustcolor("darkblue", alpha.f = 0.5),
##      xlab = "Wind velocity", ylab = "DC output",
##      main = "Original data")
## 
## # Fitted regression line
## abline(fit <- lm(Output ~ Velocity, data = windmill),
##        lwd = 2,
##        col = adjustcolor("darkred", alpha.f = 0.5))

## ----windmill-03, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Scatterplot
plot(Output ~ Velocity, data = windmill, pch = 19, las = 1,
     col = adjustcolor("darkblue", alpha.f = 0.5),
     xlab = "Wind velocity", ylab = "DC output",
     main = "Original data")

# Fitted regression line
abline(fit <- lm(Output ~ Velocity, data = windmill), 
       lwd = 2,
       col = adjustcolor("darkred", alpha.f = 0.5))

## ----windmill-04, eval=FALSE---------------------------------------------
## # Residual plot
## plot(fitted(fit), rstudent(fit), pch = 19, las = 1,
##      col = adjustcolor("darkblue", alpha.f = 0.5),
##      xlab = "Fitted value", ylab = "Studentized residual",
##      main = "Original data")
## abline(h = 0, lty = 2,
##        col = adjustcolor("darkred", alpha.f = 0.5))

## ----windmill-05, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Residual plot
plot(fitted(fit), rstudent(fit), pch = 19, las = 1,
     col = adjustcolor("darkblue", alpha.f = 0.5),
     xlab = "Fitted value", ylab = "Studentized residual",
     main = "Original data")
abline(h = 0, lty = 2, 
       col = adjustcolor("darkred", alpha.f = 0.5))

## ----windmill-06, eval=FALSE---------------------------------------------
## # Scatterplot
## plot(Output ~ I(1/Velocity), data = windmill, pch = 19, las = 1,
##      col = adjustcolor("darkblue", alpha.f = 0.5),
##      xlab = "Wind velocity", ylab = "DC output",
##      main = "Transformed data")
## 
## # Fitted regression line
## abline(fit <- lm(Output ~ I(1/Velocity), data = windmill),
##        lwd = 2,
##        col = adjustcolor("darkred", alpha.f = 0.5))

## ----windmill-07, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Scatterplot
plot(Output ~ I(1/Velocity), data = windmill, pch = 19, las = 1,  #<<
     col = adjustcolor("darkblue", alpha.f = 0.5),
     xlab = "Wind velocity", ylab = "DC output",
     main = "Transformed data")

# Fitted regression line
abline(fit <- lm(Output ~ I(1/Velocity), data = windmill), 
       lwd = 2,
       col = adjustcolor("darkred", alpha.f = 0.5))

## ----windmill-08, eval=FALSE---------------------------------------------
## # Residual plot
## plot(fitted(fit), rstudent(fit), pch = 19, las = 1,  #<<
##      col = adjustcolor("darkblue", alpha.f = 0.5),
##      xlab = "Fitted value", ylab = "Studentized residual",
##      main = "Transformed data")
## abline(h = 0, lty = 2,
##        col = adjustcolor("darkred", alpha.f = 0.5))

## ----windmill-09, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
# Residual plot
plot(fitted(fit), rstudent(fit), pch = 19, las = 1,
     col = adjustcolor("darkblue", alpha.f = 0.5),
     xlab = "Fitted value", ylab = "Studentized residual",
     main = "Transformed data")
abline(h = 0, lty = 2, 
       col = adjustcolor("darkred", alpha.f = 0.5))

## ----windmill-10, highlight.output=12------------------------------------
summary(fit)

## ----quittin-time, echo=FALSE, out.width="60%"---------------------------
RBitmoji::plot_comic(my_id, tag = "quittin")

