library(tidyverse)
training_results_summary <- readRDS("C:/Users/Elize/Documents/YR3 2nd sem/DATA MINING/training_results_summary.rds")

overall_results <- training_results_summary %>%
  group_by(K) %>%
  summarize(
    mean_sq_bias = mean(bias^2),
    mean_variance = mean(variance),
    expected_test_error = mean_sq_bias + mean_variance
  )

overall_results %>%
  pivot_longer(cols = c(mean_sq_bias, mean_variance, expected_test_error), 
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = K, y = value, color = metric)) +
  geom_line(size = 1) +
  labs(title = "Bias-Variance Tradeoff in KNN", x = "K", y = "Error", color = "Metric") +
  theme_minimal()

overall_results %>%
  ggplot(aes(x = K, y = mean_sq_bias)) +
  geom_line(color = "red", size = 1) +
  geom_point() +
  labs(title = "Mean Squared Bias vs. K", x = "K", y = "Mean Squared Bias") +
  theme_minimal()

highest_bias <- training_results_summary %>%
  arrange(desc(abs(bias))) %>%
  slice(1)
print(highest_bias)

overall_results %>%
  mutate(df = n()/K) %>%
  ggplot(aes(x = df, y = mean_variance)) +
  geom_line(color = "blue", size = 1) +
  geom_point() +
  labs(title = "Variance vs. Degrees of Freedom (n/K)", x = "n/K", y = "Variance") +
  theme_minimal()

variance_formula <- function(K, sigma2) {
  return(sigma2 / K)
}

overall_results <- overall_results %>%
  mutate(theoretical_variance = variance_formula(K, mean(training_results_summary$variance)))

overall_results %>%
  ggplot(aes(x = K)) +
  geom_line(aes(y = mean_variance, color = "Empirical Variance"), size = 1) +
  geom_line(aes(y = theoretical_variance, color = "Theoretical Variance"), linetype = "dashed", size = 1) +
  labs(title = "Empirical vs Theoretical Variance", x = "K", y = "Variance", color = "Legend") +
  theme_minimal()