library(tidyverse)

who <- read_csv("C:/Users/Elize/Downloads/WHO.csv")

head(who)
num_rows <- nrow(who)
num_cols <- ncol(who)
cat("Number of rows:", num_rows, "\n")
cat("Number of columns:", num_cols, "\n")

who2 <- who %>%
  pivot_longer(cols=starts_with("new"),  
               names_to="key",           
               values_to = "cases",        
               values_drop_na =TRUE)

who3 <- who2 %>%
  separate(key, into = c("new", "type", "sex_age"), sep = "_", extra = "merge")

who_tidy <- who3 %>%
  mutate(
    sex = substr(sex_age, 1, 1),  
    age_group = substr(sex_age, 2, nchar(sex_age))  
  ) %>%
  select(country, iso2, iso3, year, type, sex, age_group, cases)

population <- read_csv("C:/Users/Elize/Downloads/population.csv")

population <- population %>%
  select(-`Indicator Name`, -`Indicator Code`)

non_countries <- c("World", "Arab World", "High income", "Europe & Central Asia",
                   "Sub-Saharan Africa", "East Asia & Pacific", "South Asia")

population_tidy <- population %>%
  filter(!`Country Name` %in% non_countries) %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to= "year", 
               values_to= "population") %>%
  mutate(year = as.integer(year),  
         population =as.numeric(population)) %>%
  rename(country = `Country Name`)

who_tidy <- who_tidy %>%
  mutate(country = ifelse(country == "United States of America", "United States", country))

population_tidy <- population_tidy %>%
  mutate(country = ifelse(country == "United States of America", "United States", country))

tuberculosis <- who_tidy %>%
  left_join(population_tidy, by = c("country", "year"))

tuberculosis <- tuberculosis %>%
  mutate(sex = ifelse(sex %in% c("f", "m"), sex, NA))

tuberculosis <- tuberculosis %>%
  drop_na()

tuberculosis <- tuberculosis %>%
  mutate(cases_per_100k= (cases / population) * 100000)

highest_case <- tuberculosis %>%
  filter(!is.na(cases_per_100k)) %>%
  arrange(desc(cases_per_100k)) %>%
  select(country, year, cases_per_100k) %>%
  slice(1)

lowest_case <- tuberculosis %>%
  filter(!is.na(cases_per_100k)) %>%
  arrange(cases_per_100k) %>%
  select(country, year, cases_per_100k) %>%
  slice(1)

print(highest_case)
print(lowest_case)

us_tb_cases <- tuberculosis %>%
  filter(country == "United States", year >= 2000) %>%
  group_by(sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))

print(us_tb_cases)

most_cases_sex <- us_tb_cases %>%
  filter(total_cases ==max(total_cases, na.rm = TRUE)) %>%
  pull(sex)

cat("The sex with the most TB cases in the US (21st century) is:", most_cases_sex, "\n")

plot_countries <- tuberculosis %>%
  filter(country %in% c("China", "India", "United States")) %>%
  group_by(country, year) %>%
  summarise(total_cases_per_100k= sum(cases_per_100k, na.rm=TRUE))

ggplot(data=plot_countries, aes(x=year, y=total_cases_per_100k, color=country)) +
  geom_line(size= 1) +
  scale_y_log10() +
  labs(title = "Total Tuberculosis Cases per 100k",
       x= "Year",
       y= "cases per 100k",
       color= "Country") +
  theme_minimal()

age_group_distribution <- tuberculosis %>%
  group_by(age_group) %>%
  summarise(total_cases_per_100k= sum(cases_per_100k,na.rm = TRUE))

ggplot(data= age_group_distribution, aes(x=age_group, y=total_cases_per_100k)) +
  geom_bar(stat = "identity", fill= "steelblue") +
  scale_y_log10() +
  labs(title = "Total TB cases per 100k",
       x="age_group",
       y="cases per 100k") +
  theme_minimal()
theme(axis.text.x = element_text(angle= 45, hjust= 1))

cases_vs_population_2000 <- tuberculosis %>%
  filter(year == 2000) %>%
  group_by(country) %>%
  summarise(total_cases_per_100k = sum (cases_per_100k, na.rm=TRUE),
            population = sum(population, na.rm = TRUE))

ggplot(data = cases_vs_population_2000, aes(x = population, y = total_cases_per_100k)) +
  geom_point(alpha = 0.7, color = "darkred") +
  scale_x_log10() +  
  scale_y_log10() +  
  labs(title = "TB Cases per 100k vs. Population (Year 2000)",
       x = "Population (Log Scale)",
       y = "Total Cases per 100k (Log Scale)") +
  theme_minimal()