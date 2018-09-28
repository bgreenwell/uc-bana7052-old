# Heart catheter example
#
# A study was conducted and data collected to fit a regression model to predict 
# the length of a catheter needed to pass from a major artery at the femoral 
# region and moved into the heart for children (Weisberg 1980). For 12 children, 
# the proper catheter length was determined by checking with a fluoroscope that 
# the catheter tip had reached the right position. The goal is to determine a 
# model where a child’s height and weight could be used to predict the proper 
# catheter length.

# Install required packages
# install.packages("plotly")

# Load required packages
library(car)            # for vif() function
library(plotly)         # for interactive plotting
library(scatterplot3d)  # for 3-D scatterplots

# Load the data
url <- "https://bgreenwell.github.io/uc-bana7052/data/heart"
heart <- read.table(url, header = TRUE)

# Scatterplot matrix
pairs(heart)

# Correlation matrix
(cor_mat <- cor(heart))

# Interactive 3-D scatterplot
p <- plot_ly(heart, x = ~Height, y = ~Weight, z = ~Length) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Weight (pounds)"),
                      yaxis = list(title = "Height (inches)"),
                      zaxis = list(title = "Length (cm)")))
p

# Fit a linear model
heart_fit <- lm(Length ~ Height + Weight, data = heart)
summary(heart_fit)

# 3-D scatterplot with fitted surface
p <- scatterplot3d(
  x = heart$Height,
  y = heart$Weight,
  z = heart$Length,
  color = "red2",
  pch = 19,
  xlab = "Height",
  ylab = "Weight",
  zlab = "Length"
)
p$plane3d(heart_fit, lty.box = "solid", draw_polygon = TRUE)

# Compute variance inflation factors
vif(heart_fit)

# Verify formula
fit <- lm(Height ~ Weight, data = heart)
1 / (1 - summary(fit)$r.squared)
