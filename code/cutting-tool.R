# Load required packages
library(broom)
library(dplyr)
library(ggplot2)
library(lattice)

# Load the data
url <- "https://bgreenwell.github.io/uc-bana7052/data/cutting_tool.csv"
cutting_tool <- read.csv(url)

# Check column types
sapply(cutting_tool, class)

# Inspect ToolType column
head(cutting_tool$ToolType)

# How will R create an indicator variable from ToolType?
head(model.matrix( ~ ToolType, data = cutting_tool))

# Scatterplot matrix
splom(cutting_tool, groups = cutting_tool$ToolType)

# Separate regressions (do the slopes look equal?)
xyplot(Hour ~ rpm, groups = ToolType, data = cutting_tool, type = c("p", "r"),
       pch = 19)

# Regression model
fit1 <- lm(Hour ~ rpm + ToolType, data = cutting_tool)

# Print model summary
summary(fit1)

# Refit the model by creating your own indicator for ToolType
cutting_tool$ToolTypeB <- ifelse(cutting_tool$ToolType == "B", yes = 1, no = 0)
fit2 <- lm(Hour ~ rpm + ToolTypeB, data = cutting_tool)

# Residual plots
cutting_tool_2 <- augment(fit1)

# Residual plots
p1 <- ggplot(cutting_tool_2, aes(x = .fitted, y = .std.resid )) +
  geom_point(aes(colour = ToolType)) +
  # geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed")
p2 <- ggplot(cutting_tool_2, aes(sample = .std.resid )) +
  geom_qq(aes(color = ToolType)) +
  geom_qq_line()
gridExtra::grid.arrange(p1, p2, nrow = 1)
