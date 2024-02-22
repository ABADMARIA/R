library(ggplot2)

# Qs1
q1 <- ggplot(diamonds, aes(x = carat, fill = "orange")) +
  geom_histogram(binwidth = 0.2, color = "darkblue", alpha = 0.7) +
  labs(title = "Histogram of Carat",
       x = "Carat",
       y = "Frequency")

print(q1)

# Qs2
q2 <- ggplot(diamonds, aes(x = carat, color = "red")) +
  geom_line(stat = "bin", aes(y = ..count..), position = "identity") +
  labs(title = "Carat Count line plot",
       x = "Carat",
       y = "Count")

print(q2)

# Qs3
q3 <- ggplot(diamonds, aes(x = carat, fill = color)) +
  geom_histogram(binwidth = 0.2, position = "dodge", alpha = 0.7) +
  labs(title = "Carat with Color Fill histogram",
       x = "Carat",
       y = "Frequency")

print(q3)

# Qs4
q4 <- ggplot(diamonds, aes(x = carat, fill = color)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Carat with Color Fill histogram (dodge)",
       x = "Carat",
       y = "Frequency")

print(q4)
