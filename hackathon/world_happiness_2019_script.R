# Packages ----

library(tidyverse) # for general data analysis commands 

library(visdat) # for visualizing missing data

# 2019 Data ----

data <- read.csv("2019.csv")

# The World Happiness Report:
# Happiness score is a self-reported scale from 0 - 10 with 0 being the worst imagined country and 10 being the best
# The other columns reflect the extent with which certain factors contribute to happiness; no direct relation to happiness score

# Research Question:
# Is social support correlated with life expectancy?

## Tidy ----
str(data)

# Check missing
is.na(data)

vis_miss(data)

  # there is no missing data

tidied_data <- data %>%
  rename("social_support" = "Social.support",
         "life_expectancy" = "Healthy.life.expectancy",
         "overall_rank" = "Overall.rank",
         "country" = "Country.or.region",
         "score" = "Score",
         "gdp" = "GDP.per.capita",
         "freedom" = "Freedom.to.make.life.choices",
         "generosity" = "Generosity",
         "corruption" = "Perceptions.of.corruption"
         ) %>%
  mutate(country = tolower(country),
         social_support = num(social_support),
         life_expectancy = num(life_expectancy)
         )

## Summary and Visualization ----

# Get mean, min, max, range
tidied_data %>%
  summarise(mean_social_support = mean(social_support),
            sd_social_support = sd(social_support),
            min_social_support = min(social_support),
            max_social_support = max(social_support),
            mean_life_expectancy = mean(life_expectancy),
            sd_life_expectancy = sd(life_expectancy),
            min_life_expectancy = min(life_expectancy),
            max_life_expectancy = max(life_expectancy),
            )

  # social support: on average influences happiness by 1.21, lowest score is 0, highest score is 1.62
  # life expectancy: on average influences happiness by 1.14, lowest score is 0, highest score is 1.14

mean_data <- tidied_data %>%
  summarise(mean_social_support = mean(social_support),
            mean_gdp = mean(gdp),
            mean_life_expectancy = mean(life_expectancy),
            mean_freedom = mean(freedom),
            mean_generosity = mean(generosity),
            mean_corruption = mean(corruption)
            ) 

mean_tidied_data <- mean_data %>%
  pivot_longer(cols = c(mean_social_support, mean_gdp, mean_life_expectancy, mean_freedom, mean_generosity, mean_corruption),
               names_to = "condition", 
               values_to = "mean")

mean_tidied_data %>%
  ggplot(aes(x = condition, y = mean, color = condition)) +
  geom_col() +
  guides(color = "none")

# Life expectancy vs social support

tidied_data %>%
  ggplot(aes(x = life_expectancy, y = social_support)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Life Expectancy",
       y = "Social Support")

## Build model ----
  
linear_model <- lm(life_expectancy ~ social_support, data = tidied_data)

linear_model

# Question: How much each factor contributes to happiness?

# Somalia

0 + .698 + 0.268 + 0.559 + 0.243 + 0.270
# 2.038

# Calculate percentages
tidied_data <- tidied_data %>%
  mutate(total = gdp + social_support + life_expectancy + freedom + generosity + corruption) %>%
  mutate(gdp_percent = (gdp/total)*100,
         social_support_percent = (social_support/total)*100,
         life_expectancy_percent = (life_expectancy/total)*100,
         freedom_percent = (freedom/total)*100,
         generosity_percent = (generosity/total)*100,
         corruption_percent = (corruption/total)*100
         )

# Find mean percentage 

mean_percent <- tidied_data %>%
  summarise(mean_gdp_percent = mean(gdp_percent),
            mean_social_support_percent = mean(social_support_percent),
            mean_life_expectancy_percent = mean(life_expectancy_percent),
            mean_freedom_percent = mean(freedom_percent),
            mean_generosity_percent = mean(generosity_percent),
            mean_corruption_percent = mean(corruption_percent)
            ) %>%
  pivot_longer(cols = c(mean_gdp_percent, mean_social_support_percent, mean_life_expectancy_percent, mean_freedom_percent, mean_generosity_percent, mean_corruption_percent),
               names_to = "factor",
               values_to = "mean")

  # from the results we can see that social support, gdp, and life expectancy contributes the most to happiness

# Pie 

# Create list and label for pie chart
list_mean <- c(as.numeric(mean_percent$mean))
labs <- c("GDP", "Social Support", "Life Expectancy", "Freedom", "Generosity", "Perception of Corruption")
pct <- round(list_mean)

labs <- paste(labs, pct) # paste adds to list after converting to character
labs <- paste(labs, "%", sep = "")

result <- pie(list_mean, 
              main = "Mean Contribution to Happiness", 
              labels = labs,
              col = rainbow(length(labs)) # adds color to pie; diff for each label
              )

# 2015 Data ----

data_2 <- read.csv("2015.csv")

# The World Happiness Report:
# Happiness score is a self-reported scale from 0 - 10 with 0 being the worst imagined country and 10 being the best
# The other columns reflect the extent with which certain factors contribute to happiness; no direct relation to happiness score
# The Dystopian Residual score is the amount of error of data points to a fitted model; adds 1.85 (average happiness score of a dysopia) to make it a positive value

# Research Question:

