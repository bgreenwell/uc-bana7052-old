# Load required packages
library(ggplot2)
library(tibble)

# Simulate data from an MLR model
set.seed(101)  # for reproducibility
n <- 50
sim <- tibble(
  x1 = runif(n),
  x2 = runif(n),
  y = 1 + 2*x1 - 3*x2 + rnorm(n, sd = 1)
)

# Construct a scatterplot matrix
pairs(sim, cex = 1.2, pch = 19, col = adjustcolor("purple", alpha.f = 0.3))

# Draw (interactive) 3-D scatterplot
plot_ly(data = sim, x = x1, y = x2, z = y, mode = "markers", type = "scatter3d",
        marker = list(opacity = 0.7, symbol = 1, size = 5, color = "black")) %>%
  layout(
    scene = list(
      aspectmode = "manual", aspectratio = list(x = 1, y = 1, z = 1),
      xaxis = list(title = "X1", range = c(0, 1)),
      yaxis = list(title = "X2", range = c(0, 1)),
      zaxis = list(title = "Y")
    )
  )

# Fit an MLR model to the simulated data
fit <- lm(y ~ x1 + x2, data = sim)
betas <- coef(mod)

# Generate predictions over a fine grid
.x1 <- .x2 <- seq(from = 0, to = 1, length = 50)
yhat <- t(outer(.x1, .x2, function(x1, x2) betas[1] + betas[2]*x1 + betas[3]*x2))

# Draw (interactive) 3-D scatterplot with fitted mean response
plot_ly(x = ~.x1, y = ~.x2, z = ~yhat, type = "surface", opacity = 0.7) %>%
  add_trace(data = sim, x = x1, y = x2, z = y, mode = "markers", 
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
