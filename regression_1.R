#Class----
#Loading packages, performance packages are for Pearson's calculations
library(tidyverse)
library(Hmisc)
library(performance)

#Read data from Andrew's github
crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")
head(crime)

#Changing the labels for the table
crime_tidied <- crime %>%
  separate(col = "City, State", into = c("City", "State")) %>%
  rename(House_Price = index_nsa) %>%
  rename(Violent_Crimes = "Violent Crimes")
head(crime_tidied)

#Plotting regression
crime_tidied %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Violent Crimes",
       title = "Violent Crime Occurences in Areas of Different Population Size")

#Calculating Pearson's R
rcorr(crime_tidied$Population, crime_tidied$Violent_Crimes)  
# R = 0.81
# R^2 = 0.6561
# Around 66% of change in violent crimes is influence by population

#Filtering data to only look at certain population sizes
crime_filtered <- crime_tidied %>%
  filter(Population < 2000000)

#Plotting regression
crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Violent Crimes",
       title = "Violent Crime Occurences in Areas with Population Less Than 2 Million")

#Pearson's R
rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)
# R = 0.69
# R^2 = 0.4761
# Around 48% of change in violent crimes is influenced by population size

#Filter out crime based on year, rids of data independence based on same city
crime_2015 <- crime_filtered %>%
  filter(Year == 2015)

#Plot regression
crime_2015 %>%
  ggplot(aes(x = Population, y = Violent_Crimes, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 1800000) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Violent Crimes",
       title = "Violent Crime Occurences in Areas with Population Less Than 2 Million in 2015")

#Pearson'r R
rcorr(crime_2015$Population, crime_2015$Violent_Crimes)
# R = .65
# R^2 = .4225
# Around 42% of violent crime changes is predicted by population size

#Build 2 linear models
model_1 <- lm(Violent_Crimes ~ 1, data = crime_2015)
model_2 <- lm(Violent_Crimes ~ Population, data = crime_2015)
#Output - tells you characteristics of the model
#The first model is used to check for the gradient (when y = 1 what is x?)
#The second model is used to check for assumptions

#Check model assumptions
check_model(model_2)
#linearty and homogeneity of variance should be a flat line to not be violated

#Get F value
anova(model_1, model_2)
# F = 27.705

#Interpreting the model
summary(model_2)
# y-intercept = 944.3 
# gradient = 0.0069693

#Calculate predicted value based on model for 1000000 population
predicted = 944.3 + (0.00693 * 1000000)
predicted

#Challenges----
#Check whether the same relationship holds for population size and robberies in 2015

#Plot of robberies
crime_2015 %>%
  ggplot(aes(x = Population, y = Robberies, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Robberies",
       title = "Robberies in Areas with Population Less Than 2 Million in 2015")

#Create models for robberies
model_3 <- lm(Robberies ~ 1, data = crime_2015)
model_4 <- lm(Robberies ~ Population, data = crime_2015)

#Check for assumptions
check_model(model_4)

#See if model is good fit
anova(model_3, model_4)

#Interpreting the model
summary(model_4)

#Are house prices predicted by the number of violent crimes in 2015?

#Plot of house price and crime rate
crime_2015 %>%
  ggplot(aes(x = House_Price, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "House Price",
       y = "Violent Crimes",
       title = "Violent Crimes and House Price for Populations Less than 2 million in 2015")

#Models
model_5 <- lm(Violent_Crimes ~ 1, data = crime_2015)
model_6 <- lm(Violent_Crimes ~ House_Price, data = crime_2015)

#assumptions, anova, and summary
check_model(model_6)
anova(model_5, model_6)
summary(model_6)

#Are house prices predicted by population size in 2015?

#Plot
crime_2015 %>%
  ggplot(aes(x = Population, y = House_Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "House Price",
       title = "House Price for Populations Less than 2 million in 2015")

#Models
model_7 <- lm(House_Price ~ 1, data = crime_2015)
model_8 <- lm(House_Price ~ Population, data = crime_2015)

#assumptions, anova, and summary
check_model(model_8)
anova(model_7, model_8)
summary(model_8)

#Extra----
#Remove outliers in Violent Crimes vs Population

#Makes sure crime is as numeric
crime_2015$Violent_Crimes <- as.numeric(crime_2015$Violent_Crimes)

#Seeing standard deviation alone
mean_sdl(crime_2015$Violent_Crimes, mult = 1.96, na.rm = TRUE)

#Filtering out based on output from mean_sdl
crime_outlier_removed <- crime_2015 %>%
  filter(Violent_Crimes < 11909.3 & Violent_Crimes > -1340.554) 
# I manually inputed the ymin and ymax  because I could figure out how to... 
# directly put mean_sdl into the command

#Plot
crime_outlier_removed %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Violent Crimes",
       title = "Violent Crime Occurences in Areas with Different Populations in 2015 ")

