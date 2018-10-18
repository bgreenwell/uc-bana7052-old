# List of required (CRAN) packages
pkgs <- c(
  "animation",   # for pre-built statistical animations
  "dplyr",       # for data wrangling
  "ggplot2",     # for drawing nicer graphics
  "gridExtra",   # for grid.arrange() function
  "HistData",    # for historical data sets
  "investr",     # for inverse estimation
  "magick",      # for working with images
  "roundhouse",  # for pure awesomeness
  "tibble"       # for nicer data frames
)

# Install required (CRAN) packages
for (pkg in pkgs) {
  if (!(pkg %in% installed.packages()[, "Package"])) {
    install.packages(pkg)
  }
}

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

# Display plots in a grid
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

# Quadratic data
ggplot(d, aes(x = x, y = y3)) +
  geom_point(alpha = 0.9) + 
  labs(y = "y", title = "What's the correlation here?")

# Crystal weight example
data(crystal, package = "investr")  # ?investr::crystal
as_tibble(crystal)  # print a summary of the data

# Scatterplot using base R graphics
# plot(crystal)  # OK
plot(weight ~ time, data = crystal)  # better

# Scatterplot using ggplot2
ggplot(crystal, aes(x = time, y = weight)) +
  geom_point(size = 3)

cor(crystal)  # correlation matrix
with(crystal, cor(time, weight))  # using with() 
cor(crystal$time, crystal$weight)  # using $

# Test for (linear) association
with(crystal, cor.test(time, weight, conf.level = 0.95))


# Scatterplot of Galton's height data
ggplot(HistData::Galton, aes(x = parent, y = child)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Height of mid-parent (in)", y = "Height of child (in)")

# Scatterplot of Galton's data on the heights of parents and their children
ggplot(HistData::Galton, aes(x = parent, y = child)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Height of mid-parent (in)", y = "Height of child (in)")

# Scatterplot of (fake) functional data
ggplot(data.frame(x = 1:10, y = 15*1:10), aes(x, y)) +
  geom_line() +
  geom_point(size = 3, color = "red2") +
  labs(x = "Hours worked (X)", y = "Rate of pay (Y)",
       title = expression(paste("Relation: ", Y == 15 %*% X)))


# Scatterplot of (fake) functional data
ggplot(data.frame(x = 1:10, y = 15*1:10), aes(x, y)) +
  geom_line() +
  geom_point(size = 3, color = "red2") +
  labs(x = "Hours worked (X)", y = "Rate of pay (Y)", 
       title = expression(paste("Relation: ", Y == 15 %*% X)))

# Statistical relationships
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

roundhouse::kick("Chuck Norris caught all the Pok\uE9mon from a landline",
                 width = 40, type = 2, fps = 10)

# Scatterplot of arsenic data
ggplot(investr::arsenic, aes(x = actual, y = measured)) + 
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) + 
  labs(x = "Actual amount (ppm)", "Measured amount (ppm)",
       title = "Arsenic concentration data")

animation::least.squares(ani.type = "slope")
animation::least.squares(ani.type = "intercept")

# Fit an SLR model to the data
fit <- lm(weight ~ time, data = crystal)
print(fit)  # print a basic summary

# More verbose summary
summary(fit)

head(cbind(crystal, "fitted_values" = fitted(fit)))

# Residual plot
ggplot(data = crystal, aes(x = time, y = weight)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ x,  #<<
              se = FALSE, alpha = 0.5) +       #<<
  geom_segment(aes(x = time, y = fitted(fit),
                   xend = time, yend = weight),
               alpha = 0.75, col = "red2", linetype = "solid") +
  theme_light() +
  xlab("Time (weeks)") +
  ylab("Weight (grams)") +
  ggtitle("Crystal weight data")

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

# Simulate data
set.seed(1105)  # for reproducibility
x <- runif(30, min = 0, max = 5)
y <- rnorm(30, mean = 1 + 2*x, sd = 1.2)

# Simple scatterplot
plot(x, y)

# Fit an SLR model
lm(y ~ x)

