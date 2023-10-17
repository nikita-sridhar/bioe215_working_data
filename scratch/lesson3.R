library(tidyverse)

#Read and inspect
surveys <- read_csv("data/portal_data_joined.csv")
head(surveys)
summary(surveys)

#Q1: what is the type of column species_id? Of hindfoot_length?
#species_id is character, hindfoot_length is double (used typeof())

#Q2: How many rows and columns in surveys?
#34786 rows, 13 col used nrow() and ncol()

select(surveys, plot_id, species_id, weight)
select(surveys, plot_id, species_id, weight_g = weight) #can use select to rename columns in line
select(surveys,-record_id,-species_id)

filter(surveys, year == 1995)
filter(surveys, year == 1995, plot_id == 7) #and operator
filter(surveys, month == 2 | day == 20) #or operator

#Q3: filter() surveys to records collected in Nov where 
#hindfoot_length is greater than 36.0
filter(surveys, month == 11, hindfoot_length > 36.0)

#Q4: Fix these errors 
#filter(surveys, year = 1995)
#filter(surveys, polt_id == 2)
filter(surveys, year == 1995)
filter(surveys, plot_id == 2)

#Filter then select without pipes
select(filter(surveys, year == 1995), plot_id, weight)
#Filter w pipes
surveys_psw <- surveys %>%
  filter(year == 1995) %>%
  select(plot_id, weight)

#Q5: Use pipes to subset surveys to animals collected 
#before 1995 retaining just the columns year, sex, and weight
filter(surveys, year < 1995) %>%
  select(year, sex, weight)

surveys %>%
  mutate(weight_kg = weight / 1000)

surveys %>%
  filter(!is.na(weight)) %>% #is not na. ! is is not.
  mutate(weight_kg = weight/1000,
         weight_lb = weight_kg * 2.2) %>%
  view()

#Q6: Create a new data frame from the surveys data that meets 
#the following criteria: contains only the species_id column and 
#a new column called hindfoot_cm containing the hindfoot_length values 
#(currently in mm) converted to centimeters. In this hindfoot_cm column, 
#there are no NAs and all values are less than 3.

new_surveys <- surveys %>%
  mutate(hindfoot_cm = hindfoot_length / 10) %>%
  filter(!is.na(hindfoot_cm), hindfoot_cm < 3) %>%
  select(species_id, hindfoot_cm)

surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>% 
  drop_na(weight) %>%
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight),
            .groups = "drop") %>% #drops the grouping so doesn't interfere in future
  arrange(mean_weight) #arranges in ascending order

#Q7: How many animals were caught in each plot_type surveyed?
surveys %>%
  group_by(plot_type) %>%
  summarize(numanimal = (n())) %>%
  view()

#Q8: Use group_by() and summarize() to find the mean, 
#min, and max hindfoot length for each species (using species_id). 
#Also add the number of observations (hint: see ?n).
surveys %>%
  drop_na(hindfoot_length) %>%
  group_by(species_id) %>%
  summarize(mean(hindfoot_length),
            min(hindfoot_length),
            max(hindfoot_length),
            n()) %>%
  view ()
            
#Q9: What was the heaviest animal measured in each year? 
#Return the columns year, genus, species_id, and weight.
surveys %>% 
  drop_na(weight) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  ungroup() %>%
  select(year, genus, species_id, weight) %>%
  arrange(year) %>%
  View()
  
  
