
library(tidyverse)


phq9_long <- read.csv('datasets/phq9_long.csv')

phq9_baseline <- read.csv('datasets/phq9_baseline.csv')

demo <- read.csv('datasets/demographics_baseline.csv')

# passive_v1 <- read.csv('datasets/passive_v1.csv')



# keep only participants with data in weeks 2,4,6,8,10, or 12
weeks_to_keep <- c(2, 4, 6, 8, 10, 12)

phq9_long1 <- phq9_long %>%
  group_by(participant_id) %>%
  filter(any(weeks_to_keep %in% week)) %>%
  ungroup() %>%
  filter(week %in% weeks_to_keep)

phq9_long1$participant_id <- as.factor(phq9_long1$participant_id)


participant_list <- as.character(unique(phq9_long$participant_id))



all_combinations <- expand.grid(participant_id = unique(phq9_long1$participant_id), 
                                week = as.integer(weeks_to_keep))

# Group by 'group' and join with 'all_combinations' to get missing rows
phq9_long2 <- phq9_long1 %>%
  group_by(participant_id) %>%
  complete(all_combinations, fill = list(week = NA_integer_))


  

weeks_to_keep <- c(2, 4, 6, 8, 10, 12)

# Create all combinations of 'participant_id' and 'week' using expand.grid
all_combinations <- expand.grid(participant_id = unique(phq9_long1$participant_id),
                                week = weeks_to_keep)

# Use 'complete' to create rows for missing combinations of 'participant_id' and 'week'
phq <- phq9_long1 %>%
  complete(all_combinations, fill = list(phq9_1 = NA_integer_,
                                         phq9_2 = NA_integer_,
                                         phq9_3 = NA_integer_,
                                         phq9_4 = NA_integer_,
                                         phq9_5 = NA_integer_,
                                         phq9_6 = NA_integer_,
                                         phq9_7 = NA_integer_,
                                         phq9_8 = NA_integer_,
                                         phq9_9 = NA_integer_))




# Find duplicated rows based on 'participant_id' and 'week'
duplicated_rows <- phq %>%
  group_by(participant_id, week) %>%
  filter(duplicated(participant_id) | duplicated(week)) %>%
  ungroup()

# Show rows with duplicate 'participant_id' and 'week'
print(duplicated_rows, n=30)


# create function to replace duplicate weeks with their average


fill_with_average_week_data <- function(data, participant_id, week) {
  # Calculate the average sum_phq9 and phq9 for the specified participant_id and week
  average_phq <- data %>%
    filter(week == week, participant_id == participant_id) %>%
    summarise(across(starts_with("sum_phq9"), mean, na.rm = TRUE),
              across(starts_with("phq9_"), mean, na.rm = TRUE))
  
  # Create a new row with the average values for the specified participant_id
  new_row <- data %>%
    filter(participant_id == participant_id) %>%
    distinct(participant_id) %>%
    mutate(week = week, !!!average_phq)
  
  # Combine the new row with the original data and remove only the rows used for the average
  data <- bind_rows(data, new_row) %>%
    filter(!(week == week & participant_id == participant_id))
  
  # Return the modified dataframe
  return(data)
}



# inspect each case closer
# if any of the duplicates have sum_phq9=0 when the others arent, drop those

phq %>%
  filter(participant_id == 'EN00077') %>%
  print()

phq <- phq %>%
  filter(!ROW_ID %in% c(4095, 4173)) #remove the 2/3 entries with 0s
  
  
phq %>%
  filter(participant_id == 'EN00080') %>%
  print()

phq %>%
  filter(participant_id == 'EN00082') %>%
  print()

phq %>%
  filter(participant_id == 'EN00083') %>%
  print()

phq %>%
  filter(participant_id == 'EN00084') %>%
  print()

phq %>%
  filter(participant_id == 'EN00129') %>%
  print()

phq %>%
  filter(participant_id == 'EN05022') %>%
  print()


phq %>%
  filter(participant_id == 'EN05039') %>%
  print()












