library(tidyverse)
library(emmeans) # for pairwise comparisons
library(afex) # for ANOVA

data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")

# Tidying data ----

colnames(data) <- c("participant", "condition", "ability", "gaming")

tidied_data <- data %>%
  mutate(condition = tolower(condition)) %>%
  mutate(condition = factor(condition))

# Summary and visualize ----

tidied_data %>%
  group_by(condition) %>%
  summarise(mean = mean(ability))

set.seed(1234)
tidied_data %>%
  ggplot(aes(x = gaming, y = ability, color = condition)) +
  geom_point() +
  labs(x = "Gaming Ability",
       y = "Motor Ability",
       title = "Effect of gaming ability on motor ability for people who drink caffeine")
# We see higher caffeine conditions grouped themselves with people who played games more
# We can both factors influence motor ability, just don't know which plays a greater role

# ANOVA ----

anova_model <- aov_4(ability ~ condition + (1 | participant), data = tidied_data)

anova(anova_model)  
# Without taking into account gaming condition is significant

emmeans(anova_model, pairwise ~ condition)
# Condition is significant between all three levels

  # emmeans
  # double espresso = 9.02
  # single espresso = 6.69
  # water = 4.82

# ANCOVA ----

ancova_model <- aov_4(ability ~ gaming + condition + (1 | participant), data = tidied_data, factorize = FALSE)
# We don't want to make gaming a factor as it has numerical values

anova(ancova_model)
# We see that gaming is significant while condition is not
# Gaming better explains difference in motor ability

emmeans(ancova_model, pairwise ~ condition)

  # emmeans
  # double espresso = 6.32
  # single espresso = 6.87
  # water = 7.33

# We see that after adjusting for covariance emmeans for the three conditions are all similar

# Manually calculating means ----

# Manually calculating emmeans for condition using linear model for ANOVA

relevel_data <- tidied_data %>%
  mutate(condition = fct_relevel(condition, levels = c("water", "double espresso", "single espresso")))
# Changes way data is looked at when modelling


relevel_data_new <- tidied_data %>%
  mutate(condition = fct_relevel(condition, levels = c("single espresso", "double espresso", "water")))

model_lm_tidied_new <- lm(ability ~ condition, data = relevel_data_new)

model_lm_tidied <- lm(ability ~ condition, data = tidied_data)

# Remodel

model_lm <- lm(ability ~ condition, data = relevel_data)

value = 6.687 + 2.328
value

  # Output
  # Intercept - the adjusted mean
  # Other values - the difference between intercept and condition

# Apply code
contrasts(relevel_data$condition)

  # coding
  # water 0 | 0
  # double espresso 1 | 0 
  # single espresso 0 | 1

  # ability lm = intercept + B1(double espresso) + B2(single espresso)
    # B1 difference between double espresso mean and water mean / intecept 
    # B2 difference between single espresso mean and water mean / intercept

  # for mean of double espresso apply code with value from model_lm
  # i.e.

  double_mean_anova = 4.817 + 4.199 * (1) + 1.871 * (0)
  # we find mean is 9.016

# Manually calculating emmeans for condition using linear model for ANOCVA

model_lm_ancova <- lm(ability ~ gaming + condition, data = relevel_data)
model_lm_ancova
# these are mean coefficient adjusted for gaming

mean(relevel_data$gaming)
# since gaming is not a factor so we enter the mean instead of a code

  # ability lm = intercept + B1(double espresso) + B2(single espresso) + B3(gaming)

  # for mean of water group 
  # i.e.

  water_mean_ancova = -3.4498 + 0.8538 * (12.62296) - 1.0085 * (0) - 0.4563 * (0)
  # we find mean is 7.33
  
  double_mean_ancova = -3.4498 + 0.8538 * (12.62296) - 1.0085 * (1) - 0.4563 * (0)
  # we find mean is 6.32

# Changing the scale so mean for gaming is 0

scaled_data <- relevel_data %>%
  mutate(centred_gaming = scale(gaming))

plot(density(tidied_data$gaming))
plot(density(scaled_data$centred_gaming))
# comparing the two plots we can see nothing changes except for the scale of the data

scaled_model_anocova <- lm(ability ~ centred_gaming + condition, data = scaled_data)

scaled_model_anocova

# We can find the means now without having to separately calculate the mean for gaming; just use code

  # ability lm = intercept + B1(double espresso) + B2(single espresso) + B3(gaming)

  scaled_double_mean_ancova = 7.328 + 2.3046 * (0) - 1.0085 * (1) - 0.4563 * (0)
  # we find mean is still 6.32

# Challenge ----

# Reading in the data 
rm_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/rm_data.csv")
  
  # Aim: To find if there is a difference in time it takes to pronounce words of different difficulty
  # n = 32
  # DV = reaction time after 30 minutes of encountering the word (seconds)
  # IV = Difficulty of words
  
colnames(rm_data) = c("participant", "condition", "rt")
  
# Tidying data
tidied_rm_data <- rm_data %>%
  mutate(condition = tolower(condition)) %>%
  mutate(condition = factor(condition))

relevel_rm_data <- tidied_rm_data %>%
  mutate(condition = fct_relevel(condition, levels = c("very easy", "easy", "hard", "very hard")))

# Build linear models

challenge_model <- lm(rt ~ condition, data = relevel_rm_data)

challenge_model

# Output from emmeans from original anova()
# very.easy = 1.20
# easy = 1.23 
# hard = 1.39 
# very.hard = 1.87

linear_model_is_the_same_as_anova_easy = 1.199975 + 0.02846 * 1 + 0.19174 * 0 + 0.67147 * 0

linear_model_is_the_same_as_anova_easy

linear_model_is_the_same_as_anova_hard = 1.199975 + 0.02846 * 0 + 0.19174 * 1 + 0.67147 * 0

linear_model_is_the_same_as_anova_hard  

linear_model_is_the_same_as_anova_very_hard = 1.199975 + 0.02846 * 0 + 0.19174 * 0 + 0.67147 * 1

linear_model_is_the_same_as_anova_very_hard

