library (tidyverse)

#First Task--------------------------------------------------------------

mpg
#is a data set already stored in the tidyverse package
#it shows fuel economy data in the USA collected by EPA from 1999 to 2008
#the 11 variables in the data set include:
      #manufacturer = manufacturer model
      #model        = model name
      #displ        = engine displacement in litres
      #year         = year of manufacture
      #cyl          = number of cylinders
      #trans        = type of transmission
      #drv          = f - front wheel drive, r = rear wheel drive, 4 = 4w drive
      #cty         = city miles per gallon
      #hwy          = highway miles per gallon
      #fl           = fuel type
      #class        = car class

head(mpg)

tail(mpg)

str(mpg)

#mpg <- rename(mpg, manufacturer = "car brand") 
#how do you get rename() to work?

mpg %>%
  select(manufacturer)

mpg %>%
  distinct(manufacturer) %>%
  count()

mpg %>%
  filter(manufacturer == "honda" & year == "1999") %>%
  select(manufacturer, year, cty, hwy)

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer), 
         model = str_to_title(model)) %>%
  select(manufacturer, model, year, trans, hwy)

#Second Task-------------------------------------------------------------

my_messy_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/my_data.csv")

head(my_messy_data)

#to recode condition column and separate it into two columns
my_tidied_data <- my_messy_data %>%
  mutate(condition = recode(condition,
                            "1" = "Prime_A-Target_A",
                            "2" = "Prime_A-Target_B",
                            "3" = "Prime_B-Target_A",
                            "4" = "Prime_B-Target_B")) %>%
  separate(col = "condition", into = c("Prime", "Target"), sep = "-") %>%
  mutate(Prime = factor(Prime), Target = factor(Target))

head(my_tidied_data)

#Third Task--------------------------------------------------------------

my_wide_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/my_wide_data.csv")

head(my_wide_data)

my_long_data <- my_wide_data %>%
  pivot_longer(cols = c(Condition1, Condition2, Condition3, Condition4),
               names_to = "condition",
               values_to = "rt") %>%
  mutate(condition = factor(condition))

#my_long_data <- rename(my_long_data, condition = condition("Condition1" = "condition_1"))

head(my_long_data)

my_wide_data_2 <- my_long_data %>%
  pivot_wider(names_from = "condition",
              values_from = "rt")

  head(my_wide_data_2)

#Fourth Task (requires data from 3rd task)-------------------------------------------------------------

individual_diffs <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/individual_diffs.csv")

combined_data <- full_join(my_long_data, individual_diffs, by = "ID")

#Fifth Task (requires data from 3rd task)--------------------------------------------------------------

large_ind_diffs <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/large_ind_diffs.csv")

combined_data_2 <- left_join(my_long_data, large_ind_diffs, by = "ID")

#Sixth Task------------------------

starwars_human_tidied <- starwars %>%
  filter(!is.na(height)) %>%
  filter(species == "Human") %>%
  select(name, height, species)

starwars_summary <- starwars_human_tidied %>%
  summarise(mean_height = mean(height),
            median_height = median(height),
            min_height = min(height),
            max_height = max(height))

# ALternative way of only getting mean height
starwars %>%
  filter(species == "Human") %>%
  summarise(mean_height = mean(height, na.rm = TRUE))

starwars_all_tidied <- starwars %>%
  filter(!is.na(

#Seventh Task--------------------------------------------------------------------

titanic_data <- as.data.frame(Titanic)

titanic_wider <- titanic_data %>%
  filter(Age == "Adult") %>%
  pivot_wider(names_from = c("Age", "Class"), values_from = "Freq")

titanic_wider %>%
  mutate(total = Adult_1st + Adult_2nd + Adult_3rd + Adult_Crew) %>%
  select(Sex, Survived, total)

titanic_long <- titanic_wider %>%
  pivot_longer(cols = c(Adult_1st, Adult_2nd, Adult_3rd, Adult_Crew),
               names_to = "age",
               values_to = "frequency") %>%
  separate(col = "age", into = c("age", "class"), sep = "_")
