
library(tidyverse)


phq_long <- read.csv('datasets/phq9_long.csv')

phq_base <- read.csv('datasets/phq9_baseline.csv')

demo <- read.csv('datasets/demographics_baseline.csv')

# passive_v1 <- read.csv('datasets/passive_v1.csv')


# join columns of phq9_baseline (as week 0) to phq_long

#first join phq9_baseline and phq_long
phq_base <- phq_base %>%
  mutate(week = 0, sum_phq9 = rowSums(across(starts_with("phq9")))) %>%
  rename(phq9Date = baselinePHQ9date) %>%
  select(-study)

phq_long1 <- rbind(phq_long, phq_base) %>%
             arrange(participant_id, week) %>%
             select(-c(ROW_ID, ROW_VERSION))





# keep only participants with data in weeks 0,2,4,6,8,10, or 12
weeks_to_keep <- c(0, 2, 4, 6, 8, 10, 12)

phq_long1 <- phq_long1 %>%
  group_by(participant_id) %>%
  filter(any(weeks_to_keep %in% week)) %>%
  ungroup() %>%
  filter(week %in% weeks_to_keep)




# remove participant_ids with less than 4 weeks(rows) of data
phq_long1 <- phq_long1 %>%
  left_join(phq_long1 %>%
              group_by(participant_id) %>%
              summarise(count = n()), by = "participant_id") %>%
  filter(count >= 4) %>%
  select(-count)

# check to see if it worked
min(table(phq_long1$participant_id)) #we good


# now we create rows for all the missing weeks for imputation

# Create all combinations of 'participant_id' and 'week' using expand.grid
all_combinations <- expand.grid(participant_id = unique(phq_long1$participant_id),
                                week = weeks_to_keep)

# Use 'complete' to create rows for missing combinations of 'participant_id' and 'week'
phq_long1 <- phq_long1 %>%
  complete(all_combinations, fill = list(phq9_1 = NA_integer_,
                                         phq9_2 = NA_integer_,
                                         phq9_3 = NA_integer_,
                                         phq9_4 = NA_integer_,
                                         phq9_5 = NA_integer_,
                                         phq9_6 = NA_integer_,
                                         phq9_7 = NA_integer_,
                                         phq9_8 = NA_integer_,
                                         phq9_9 = NA_integer_))

# check if there are rows per 525 participants
nrow(phq_long2) / 7 # some participants may have multiple responses for the same week



# Find duplicated rows based on 'participant_id' and 'week'
print(phq_long2 %>%
  group_by(participant_id, week) %>%
  filter(duplicated(participant_id) | duplicated(week)) %>%
  ungroup(), n=30)



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


phq_long2 <- phq_long1 ################################

# inspect each case closer
# if any of the duplicates have sum_phq9=0 when the others arent, drop those

phq_long2 %>%
  filter(participant_id == 'EN00077') %>%
  print()
#remove the 2/3 week2 entries with 0s
phq_long2 <- phq_long2 %>%
  filter(!(participant_id == 'EN00077' & sum_phq9 == 0) | is.na(sum_phq9))




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












