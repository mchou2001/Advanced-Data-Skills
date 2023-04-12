# Packages ----
library(ggplot2)
library(ggthemes)
library(glue)
library(plotly)
library(tidyverse)

# Load the data

data_2015 <- read.csv("2015.csv")
data_2016 <- read.csv("2016.csv")
data_2017 <- read.csv("2017.csv")
data_2018 <- read.csv("2018.csv")
data_2019 <- read.csv("2019.csv")

# Look at data

glimpse(data_2015)
glimpse(data_2016)
glimpse(data_2017)
glimpse(data_2018)
glimpse(data_2019)

# Clean data 

clean_2015 <- data_2015 %>% 
  select(-c(Standard.Error, Dystopia.Residual)) %>% 
  mutate(year = 2015) %>% 
  rename(country = Country,
         region = Region,
         overall_rank = Happiness.Rank,
         happiness = Happiness.Score,
         generosity = Generosity,
         gdp = Economy..GDP.per.Capita., 
         social_support = Family, 
         life_expectancy = Health..Life.Expectancy., 
         freedom = Freedom,
         corruption = Trust..Government.Corruption.,
         )

clean_2016 <- data_2016 %>% 
  select(-c(Lower.Confidence.Interval, Upper.Confidence.Interval, Dystopia.Residual)) %>% 
  mutate(year = 2016) %>%
  rename(country = Country,
         region = Region,
         overall_rank = Happiness.Rank,
         happiness = Happiness.Score,
         generosity = Generosity,
         gdp = Economy..GDP.per.Capita.,
         social_support = Family,
         life_expectancy = Health..Life.Expectancy.,
         freedom = Freedom,
         corruption = Trust..Government.Corruption.)

clean_2017 <- data_2017 %>% 
  select(-c(Whisker.high, Whisker.low, Dystopia.Residual)) %>% 
  mutate(year = 2017) %>%
  rename(country = Country,
         overall_rank = Happiness.Rank,
         happiness = Happiness.Score,
         generosity = Generosity,
         gdp = Economy..GDP.per.Capita.,
         social_support = Family,
         life_expectancy = Health..Life.Expectancy.,
         freedom = Freedom,
         corruption = Trust..Government.Corruption.
         )

clean_2018 <- data_2018 %>% 
  mutate(Perceptions.of.corruption = as.numeric(Perceptions.of.corruption),
         year = 2018) %>%
  fill(Perceptions.of.corruption) %>%
  rename(overall_rank = Overall.rank,
         country = Country.or.region,
         happiness = Score,
         corruption = Perceptions.of.corruption,
         social_support = Social.support,
         life_expectancy = Healthy.life.expectancy,
         freedom = Freedom.to.make.life.choices,
         generosity = Generosity,
         gdp = GDP.per.capita
         )

clean_2019 <- data_2019 %>% 
  mutate(year = 2019) %>%
  rename(overall_rank = Overall.rank,
         country = Country.or.region,
         happiness = Score,
         gdp = GDP.per.capita,
         social_support = Social.support,
         life_expectancy = Healthy.life.expectancy,
         freedom = Freedom.to.make.life.choices,
         generosity = Generosity,
         corruption = Perceptions.of.corruption
         )



