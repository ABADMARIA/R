library(ggplot2)

#Qs1
latitude <- c(50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 69, 70)
mortality_index <- c(1025, 1045, 1004, 959, 870, 950, 886, 892, 789, 846, 817, 722, 651, 681, 673, 525)
temperature <- c(51.3, 49.9, 50, 49.2, 48.5, 47.8, 47.3, 45.1, 46.3, 42.1, 44.2, 43.5, 42.3, 40.2, 31.8, 34)

df <- data.frame(latitude, mortality_index, temperature)

plot(df$temperature, df$mortality_index, main="Mortality Index vs Mean Average Temperature", 
     xlab="Mean Average Temperature (°C)", ylab="Mortality Index", pch=16)

model <- lm(log(mortality_index) ~ temperature, data=df)

plot(df$temperature, residuals(model), main="Residuals Plot", 
     xlab="Mean Average Temperature (°C)", ylab="Residuals", pch=16)
abline(h=0, col="red")

summary(model)

#Qs2
subset_diamonds <- diamonds[1:1000, ]

loess_model_1 <- loess(log(price) ~ carat, data = subset_diamonds, span = 0.2)
loess_model_2 <- loess(log(price) ~ carat, data = subset_diamonds, span = 0.5)
loess_model_3 <- loess(log(price) ~ carat, data = subset_diamonds, span = 0.8)
loess_model_4 <- loess(log(price) ~ carat, data = subset_diamonds, span = 0.5, degree = 2)

subset_diamonds$fit_1 <- predict(loess_model_1)
subset_diamonds$fit_2 <- predict(loess_model_2)
subset_diamonds$fit_3 <- predict(loess_model_3)
subset_diamonds$fit_4 <- predict(loess_model_4)

ggplot(subset_diamonds, aes(x = carat, y = log(price))) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = fit_1), color = "red") +
  geom_line(aes(y = fit_2), color = "blue") +
  geom_line(aes(y = fit_3), color = "green") +
  geom_line(aes(y = fit_4), color = "purple") +
  labs(title = "Log Price as a Function of Carat with Loess Smoother",
       x = "Carat",
       y = "Log Price")

#Qs3
loess_model <- loess(log(price) ~ carat, data = subset_diamonds)
poly_step_model <- lm(log(price) ~ poly(carat, 3) + as.numeric(carat > 2), data = subset_diamonds)

subset_diamonds$loess_residuals <- residuals(loess_model)
subset_diamonds$poly_step_residuals <- residuals(poly_step_model)

ggplot(subset_diamonds, aes(x = carat)) +
  geom_point(aes(y = loess_residuals), color = "blue", alpha = 0.6, size = 2) +
  geom_point(aes(y = poly_step_residuals), color = "red", alpha = 0.6, size = 2) +
  labs(title = "Residuals Comparison",
       x = "Carat",
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()