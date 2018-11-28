## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE, servr.daemon = TRUE)

# Global chunk options
knitr::opts_chunk$set(
  echo = TRUE,
  # dev = "svg",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,
  out.width = "70%",
  message = FALSE,
  warning = FALSE,
  error = FALSE
)

# Load required packages
library(dplyr)
library(patchwork)

# RBitmoji id
my_id <- "1551b314-5e8a-4477-aca2-088c05963111-v1"

## ----curve-fitting, echo=FALSE, out.width="40%"--------------------------
knitr::include_graphics("images/curve-fitting.png")

## ----prerequisites, eval=FALSE-------------------------------------------
## # List of required (CRAN) packages
## pkgs <- c(
##   "animation",   # for pre-built statistical animations
##   "dplyr",       # for data wrangling
##   "ggplot2",     # for drawing nicer graphics
##   "gridExtra",   # for grid.arrange() function
##   "HistData",    # for historical data sets
##   "investr",     # for inverse estimation
##   "magick",      # for working with images
##   "roundhouse",  # for pure awesomeness
##   "tibble"       # for nicer data frames
## )
## 
## # Install required (CRAN) packages
## for (pkg in pkgs) {
##   if (!(pkg %in% installed.packages()[, "Package"])) {
##     install.packages(pkg)
##   }
## }

## ----wonka, echo=FALSE, out.width="70%"----------------------------------
knitr::include_graphics("images/read-the-book.jpg")

## ----statistical-relationships-01----------------------------------------
# Load required packages
library(ggplot2)
library(tibble)

# Generate some random data
set.seed(101)
d <- tibble(
  x = seq(from = -2, to = 2, length = 100),
  y1 = 1 + x + rnorm(length(x)),
  y2 = 1 + x^2 + rnorm(length(x)),
  y3 = 1 + x^3 + rnorm(length(x)),
  y4 = 1 + rnorm(length(x))
)

## ----statistical-relationships-02, fig.keep="none"-----------------------
# Construct scatterplots
p1 <- ggplot(d, aes(x = x, y = y1)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "Linear")
p2 <- ggplot(d, aes(x = x, y = y2)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "Quadratic")
p3 <- ggplot(d, aes(x = x, y = y3)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "Cubic")
p4 <- ggplot(d, aes(x = x, y = y4)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "None")

# Display plots in a grid
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

## ----statistical-relationships-04, echo=FALSE, out.width="80%"-----------
# Display plots in a grid
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

## ----pearson-correlation-01, echo=FALSE, fig.width=7, fig.height=5, out.width="80%"----
set.seed(101)
d <- tibble(
  x = seq(from = -2, to = 2, length = 100),
  y1 = 1 + x + rnorm(length(x)),
  y2 = 1 - x + rnorm(length(x)),
  y3 = 1 + x^2 + rnorm(length(x)),
  y4 = 1 + rnorm(length(x))
)
p1 <- ggplot(d, aes(x = x, y = y1)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "Positive")
p2 <- ggplot(d, aes(x = x, y = y2)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "Negative")
p3 <- ggplot(d, aes(x = x, y = y4)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "None")
p4 <- "images/correlation-coefficient.png" %>%
  png::readPNG() %>%
  grid::rasterGrob(interpolate = TRUE, width = 0.8)
p4 <- ggplot() +
  annotation_custom(p4, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
p1 + p2 + p3 - p4 + 
  plot_layout(ncol = 1, widths = c(1, 1, 1), heights = c(1, 2.5))
# (p1 | p2 | p3) / p4

## ----pearson-correlation-02, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="100%"----
ggplot(d, aes(x = x, y = y3)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "What's the correlation here?")

## ----correlation-causation, echo=FALSE, out.width="100%", fig.cap="If you get the joke in this comic, then you probably undestand enough about correlation!"----
knitr::include_graphics("images/correlation-causation-comic.png")

## ----crystal-------------------------------------------------------------
data(crystal, package = "investr")  # ?investr::crystal
tibble::as_tibble(crystal)  # print a summary of the data

## ----your-turn-01-01, fig.width=6, fig.asp=0.618, out.width="70%"--------
# Scatterplot using base R graphics
# plot(crystal)  # OK
plot(weight ~ time, data = crystal)  # better

## ----your-turn-01-02, fig.width=6, fig.asp=0.618, out.width="70%"--------
# Scatterplot using ggplot2
ggplot(crystal, aes(x = time, y = weight)) +
  geom_point(size = 3)

## ----your-turn-01-03-----------------------------------------------------
cor(crystal)  # correlation matrix
with(crystal, cor(time, weight))  # using with() 
cor(crystal$time, crystal$weight)  # using $

## ----your-turn-01-04-----------------------------------------------------
# Test for (linear) association
with(crystal, cor.test(time, weight, conf.level = 0.95))

## ----galton-01, eval=FALSE-----------------------------------------------
## # Scatterplot of Galton's height data
## ggplot(HistData::Galton, aes(x = parent, y = child)) +
##   geom_point(alpha = 0.5) +
##   geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
##   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
##   labs(x = "Height of mid-parent (in)", y = "Height of child (in)")

## ----galton-02, echo=FALSE, out.width="90%"------------------------------
# Scatterplot of Galton's data on the heights of parents and their children
ggplot(HistData::Galton, aes(x = parent, y = child)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Height of mid-parent (in)", y = "Height of child (in)")

## ----functional-relation-01, eval=FALSE----------------------------------
## # Scatterplot of (fake) functional data
## ggplot(data.frame(x = 1:10, y = 15*1:10), aes(x, y)) +
##   geom_line() +
##   geom_point(size = 3, color = "red2") +
##   labs(x = "Hours worked (X)", y = "Rate of pay (Y)",
##        title = expression(paste("Relation: ", Y == 15 %*% X)))

## ----functional-relation-02, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="70%"----
# Scatterplot of (fake) functional data
ggplot(data.frame(x = 1:10, y = 15*1:10), aes(x, y)) +
  geom_line() +
  geom_point(size = 3, color = "red2") +
  labs(x = "Hours worked (X)", y = "Rate of pay (Y)", 
       title = expression(paste("Relation: ", Y == 15 %*% X)))

## ----linear-relationships, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="90%"----
p1 <- ggplot(investr::crystal, aes(x = time, y = weight)) +
  geom_point() +
  labs(x = "Time (hours)", 
       y = "Weight (grams)", 
       title = "Crystal weight data")
p2 <- ggplot(investr::arsenic, aes(x = actual, y = measured)) +
  geom_point() +
  labs(x = "True amount of arsenic", 
       y = "Measured amount of arsenic",
       title = "Arsenic concentration data")
gridExtra::grid.arrange(p1, p2, nrow = 1)

## ----roundhouse-02-------------------------------------------------------
roundhouse::kick("Chuck Norris caught all the Pok\uE9mon from a landline",
                 width = 40, type = 2, fps = 10)

## ----slr-conditional-distribution, echo=FALSE, out.width="80%"-----------
knitr::include_graphics("images/slr-conditional-distribution.png")

## ----arsenic-conditional-distribution, echo=FALSE, out.width="100%"------
# Scatterplot of arsenic data
ggplot(investr::arsenic, aes(x = actual, y = measured)) + 
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) + 
  labs(x = "Actual amount (ppm)", "Measured amount (ppm)",
       title = "Arsenic concentration data")

## ----calculus-joke, echo=FALSE, out.width="100%"-------------------------
knitr::include_graphics("images/calculus-joke.jpg")

## ----homer-simpson-smart, echo=FALSE, out.width="45%"--------------------
knitr::include_graphics("images/homer-simpson-smart.jpg")

## ----animation, eval=FALSE-----------------------------------------------
## animation::least.squares(ani.type = "slope")
## animation::least.squares(ani.type = "intercept")

## ----your-turn-02-01-----------------------------------------------------
# Load the data (if not already loaded)
data(crystal, package = "investr")

# Fit an SLR model to the data
fit <- lm(weight ~ time, data = crystal)
print(fit)  # print a basic summary

## ----your-turn-02-02-----------------------------------------------------
# More verbose summary
summary(fit)

## ----fitted-values-------------------------------------------------------
head(cbind(crystal, "fitted_values" = fitted(fit)))

## ----crystal-residuals-01, eval=FALSE------------------------------------
## # Residual plot
## ggplot(data = crystal, aes(x = time, y = weight)) +
##   geom_point(size = 3) +
##   geom_smooth(method = "lm", formula = y ~ x,  #<<
##               se = FALSE, alpha = 0.5) +       #<<
##   geom_segment(aes(x = time, y = fitted(fit),
##                    xend = time, yend = weight),
##                alpha = 0.75, col = "red2", linetype = "solid") +
##   theme_light() +
##   xlab("Time (weeks)") +
##   ylab("Weight (grams)") +
##   ggtitle("Crystal weight data")

## ----crystal-residuals-02, echo=FALSE, fig.width=6, fig.asp=0.618, out.width="90%"----
# Residual plot
ggplot(data = crystal, aes(x = time, y = weight)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, alpha = 0.5) +  
  geom_segment(aes(x = time, y = fitted(fit), xend = time, yend = weight), 
               alpha = 0.75, col = "red2", linetype = "solid") +
  theme_light() +
  xlab("Time (weeks)") +
  ylab("Weight (grams)") +
  ggtitle("Crystal weight data")

## ----your-turn-03-01-----------------------------------------------------
# Simulate data
set.seed(1105)  # for reproducibility
x <- runif(30, min = 0, max = 5)
y <- rnorm(30, mean = 1 + 2*x, sd = 1.2)

## ----your-turn-03-02-----------------------------------------------------
# Simple scatterplot
plot(x, y)

## ----your-turn-03-03-----------------------------------------------------
# Fit an SLR model
lm(y ~ x)

## ----quittin-time, echo=FALSE, out.width="100%"--------------------------
RBitmoji::plot_comic(my_id, tag = "quittin")

