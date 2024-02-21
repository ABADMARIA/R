library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dcldata)

#Qs1
dataset <- "C:\\Users\\maria\\Downloads\\cytof_one_experiment.csv"
data <- read.csv(dataset)

reshaped_data <- pivot_longer(data, 
                              cols = everything(),
                              names_to = "Protein", 
                              values_to = "Amount")

print(reshaped_data)

#Qs2
result <- reshaped_data %>%
  group_by(Protein) %>%
  summarise(
    Median_Level = median(Amount),
    MAD_Level = mad(Amount)
  )

print(result)

# Qs3
ggplot(result, aes(x = MAD_Level, y = Median_Level)) +
  geom_point(color = "blue", size = 3) +  
  labs(title = "Spread-Location (s-l) Plot",
       x = "Median Absolute Deviation (MAD)",
       y = "Median Protein Level",
       subtitle = "Customized Plot") 

#Qs4
library(dplyr)
library(tidyr)

reshaped_data <- data %>%
  pivot_longer(cols = -contains("Country_Event_Year"),
               names_to = "Country_Event_Year",
               values_to = "Score") %>%
  separate(Country_Event_Year, into = c("Country", "Event", "Year"), sep = "_")

print(reshaped_data)