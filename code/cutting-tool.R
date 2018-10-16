# Load required packages
library(lattice)

# Load the data
cutting_tool <- read.csv("docs/data/cutting_tool.csv")

# Scatterplot matrix
splom(cutting_tool, groups = cutting_tool$ToolType)

# Separate regressions
xyplot(Hour ~ rpm, groups = ToolType, data = cutting_tool, type = c("p", "r"),
       pch = 19)

# Regression model
fit1 <- lm(Hour ~ rpm + ToolType, data = cutting_tool)

# Print model summary
summary(fit1)
