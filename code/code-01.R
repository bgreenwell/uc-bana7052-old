# Lecture 01

# Setup ------------------------------------------------------------------------

# Load required packages
library(ggplot2)
library(investr)
library(magick)
library(roundhouse)

# Color palette
cols <- RColorBrewer::brewer.pal(9, name = "Set1")

# Boston housing data
boston <- pdp::boston


# Quotes -----------------------------------------------------------------------

# Generate quote with roundhouse kick
roundhouse(
  text = paste0(
    "\"Essentially, all models are wrong, but some are useful.",
    "\"\n\n--George Box"
  ),
  width = 40, 
  size = 25, 
  fps = 5
) %>% image_write("gifs/quote-box.gif")

# Example of a functional relation
plot(x = 1:8, y = 15 * 1:8, type = "b", cex = 1.3, pch = 19, las = 1,
  xlab = "Hours worked", ylab = "Pay ($)"
)

# Example of a statistical relation
ggplot(boston, aes(x = lstat, y = cmedv)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
              color = cols[1L]) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, 
              color = cols[2L]) +
  theme_light()

# Example of observational and experimental data
library(ggplot2)  # for advanced plotting
library(investr)  # for crystal and arsenic data sets
p1 <- ggplot(crystal, aes(x = time, y = weight)) +
  geom_point() +
  labs(x = "Time (hours)", y = "Weight (grams)")
p2 <- ggplot(arsenic, aes(x = actual, y = measured)) +
  geom_point() +
  labs(x = "True amount of arsenic", y = "Measured amount of arsenic")
gridExtra::grid.arrange(p1, p2, nrow = 1)

# Galton's height data
library(HistData)  # for Galton's height data
library(ggplot2)   # for advanced plotting
ggplot(Galton, aes(x = parent, y = child)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Height of mid-parent (in)", y = "Height of child (in)")


# Rocket propellant data -------------------------------------------------------

# Load the data
rocket <- read.csv(
  file = file.choose(),
  header = TRUE
)

# Print first 5 observations (6 is default)
head(rocket, n = 5)

# Tibbles are also nice
tibble::as.tibble(rocket)

# Print last 5 observations (6 is default)
tail(rocket, n = 5)

# Print column names
names(rocket)  # colanmes() works too and also for matrices!

# Print (compact) information about any R object with str()
str(rocket)

# Explicitly print an object
print(rocket)  # or just type rocket into the console and hit enter

# Print summary (this is a generic function with a data.frame method)
summary(rocket)

# Everything in R is a class
class(rocket)
class("a")
class(x <- "a")  # lazy
class(1)
class(1L)  # L forces an integer

# You can reassign column names pretty easily
names(rocket)[1L] = "response"
# NNNOOOOOooooooooo!! DO NOT USE `=` FOR ASSIGNMENT!

names(rocket)
names(rocket)[1L] <- "y"

# There are a number of ways to extract the number of rows/columns
dim(rocket)
n <- dim(rocket)[1L]
number_of_col <- dim(rocket)[2L]
nrow(rocket)
NROW(rocket)  # ?NROW

# A simple scatterplot
plot(rocket$x, rocket$y, pch = 20)    # <- gross
plot(y ~ x, data = rocket, pch = 20)  # <- cleaner

# Using lattice
lattice::xyplot(y ~ x, data = rocket, type = c("p", "smooth"),
                pch = 19, col = "black")
p <- lattice::xyplot(y ~ x, data = rocket)  # you can save lattice plots
print(p)

# Using ggplot2 (a bit weirder syntax, but totally worth mastering!)
library(ggplot2)
p <- ggplot(data = rocket, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.75) +
  theme_light() +
  xlab("Age of propellant (weeks)") +
  ylab("Shear strength (psi)") +
  ggtitle("Rocket propellant data")
p

# Explore data a bit
attach(rocket)
plot(x, y, pch = 20)
mean(x)  # use `na.rm = TRUE` to deal with missing values
median(x)
sd(x)
IQR(x)  # remember what this is??
hist(x, breaks = 10)
hist(x, freq = FALSE)
ggplot(data = rocket, aes(x = x)) + 
  geom_histogram(color = "white", fill = "red2")
hist(x, freq = FALSE)
ggplot(data = rocket, aes(x = x)) + 
  geom_histogram(binwidth = 5, color = "white", fill = "red2", alpha = 0.5)
summary(x)
fivenum(x)  # Tukey's five-number summary
cor(x, y)
cor(rocket)
cor.test(x, y)  # POP QUIZ: Can you interpret this p-value? How about the CI?
boxplot(x)

# Do some data wrangling (and visualization)
library(tidyr)
rocket_long <- rocket %>%
  gather(key = "variable", value = "value") 
ggplot(rocket_long, aes(y = value, group = variable, fill = variable)) +
  geom_boxplot(notch = FALSE) +
  facet_wrap( ~ variable, scales = "free")

# Fitting a linear model in R using the lm() function
rocket_slr <- lm(y ~ x, data = rocket)

# Plot the data and the fitted regression line
plot(
  formula = y ~ x, 
  data = rocket,
  pch = 19,
  xlab = "Age of propellant (weeks)",
  ylab = "Shear strength (psi)",
  main = "Rocket propellant example"
)
abline(rocket_slr, col = "red2")  # only works for SLR models

# The investr package offers a more flexible function called plotFit()
library(investr)
plotFit(
  object = rocket_slr,
  pch = 19,
  xlab = "Age of propellant (weeks)",
  ylab = "Shear strength (psi)",
  main = "Rocket propellant example",
  col.fit = adjustcolor("red2", alpha.f = 0.5)
)

# Extract the LS estimated of the intercept and slope
coef(rocket_slr)

# Add column of fitted values and residuals
rocket$fit <- fitted(rocket_slr)
rocket$res <- residuals(rocket_slr)
write.csv(  # save results as CSV
  x = rocket, 
  file = "/Users/bgreenwell/Dropbox/teaching/uc-bana7052/data/rocket.csv",
  row.names = FALSE
)
head(rocket, n = 5)

# Residual plot
ggplot(data = rocket, aes(x = x, y = y)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, alpha = 0.5) +
  geom_segment(aes(x = , y = fit, xend = x, yend = y), 
               alpha = 0.5, col = "red2", linetype = "dashed") +
  theme_light() +
  xlab("Age of propellant (weeks)") +
  ylab("Shear strength (psi)") +
  ggtitle("Rocket propellant data")
