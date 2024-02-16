library(ggplot2)

data <- read.csv("C:\\Users\\maria\\Downloads\\cytof_one_experiment.csv")

# Qs 2
column1 <- data[[1]]

# Histogram for first column
hist(column1, main="Histogram of the First Column", xlab="Values", breaks=30, col="green")

# Q-Q plot for first column
qqnorm(column1, main="Q-Q Plot of the First Column")
qqline(column1, col="blue")

# Qs 3

# Select first two columns
column1 <- data[[1]]
column2 <- data[[2]]

qqplot(column1, column2, main = "Q-Q Plot of the First Two Columns", xlab = "Column 1 Quantiles", ylab = "Column 2 Quantiles")
abline(0, 1, col = "blue")