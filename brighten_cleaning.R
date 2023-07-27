
library(tidyverse)


phq_long <- read.csv('datasets/phq9_long.csv') #original file name =

phq_base <- read.csv('datasets/phq9_baseline.csv') #original file name =

demo <- read.csv('datasets/demographics_baseline.csv') #original file name =

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

# check if there are 7 rows per participant
n_distinct(phq_long1$participant_id)
nrow(phq_long1) / 7 # some participants may have multiple responses for the same week



# find participants with duplicate weeks 
print(phq_long1 %>%
  group_by(participant_id, week) %>%
  filter(duplicated(participant_id) | duplicated(week)) %>%
  ungroup(), n=30) %>%
  select(participant_id) %>%
  unique()




# ---------- create function to replace duplicate weeks with their average ---------

replace_with_average_week_data <- function(data, participant, wk) {
  average <- data %>%
    filter(week == wk, participant_id == participant) %>%
    summarise(average_sum_phq9 = mean(sum_phq9, na.rm = TRUE),
              average_phq9_1 = mean(phq9_1, na.rm = TRUE),
              average_phq9_2 = mean(phq9_2, na.rm = TRUE),
              average_phq9_3 = mean(phq9_3, na.rm = TRUE),
              average_phq9_4 = mean(phq9_4, na.rm = TRUE),
              average_phq9_5 = mean(phq9_5, na.rm = TRUE),
              average_phq9_6 = mean(phq9_6, na.rm = TRUE),
              average_phq9_7 = mean(phq9_7, na.rm = TRUE),
              average_phq9_8 = mean(phq9_8, na.rm = TRUE),
              average_phq9_9 = mean(phq9_9, na.rm = TRUE))
  
  new_row <- data %>%
    filter(participant_id == participant) %>%
    distinct(participant_id) %>%
    mutate(week = wk,
           sum_phq9 = average$average_sum_phq9,
           phq9_1 = average$average_phq9_1,
           phq9_2 = average$average_phq9_2,
           phq9_3 = average$average_phq9_3,
           phq9_4 = average$average_phq9_4,
           phq9_5 = average$average_phq9_5,
           phq9_6 = average$average_phq9_6,
           phq9_7 = average$average_phq9_7,
           phq9_8 = average$average_phq9_8,
           phq9_9 = average$average_phq9_9)
  
  data <- data %>%
    filter(!(week == wk & participant_id == participant))
  
  data <- bind_rows(data, new_row)
  return(data)

}




phq_long2 <- phq_long1 ################################

# inspect each case with duplicate weeks closer
# if any duplicates have sum_phq9=0 when the others aren't, drop those
# if any duplicates were completed on unusual dates (e.g., week4 was completed 1 day before w6), drop those 
# if duplicates are non-0 and clustered together in dates, average them and replace

# EN00077 -----------------------------------------------------------------
phq_long2 %>% filter(participant_id == 'EN00077') %>% print()
# remove the 2/3 week2 rows with 0s
phq_long2 <- phq_long2 %>%
  filter(!(participant_id == 'EN00077' & sum_phq9 == 0) | is.na(sum_phq9))
# average the 3 week4 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00077", 4)

print(phq_long2 %>% filter(participant_id == 'EN00077'))

# EN00080 -----------------------------------------------------------------
print(phq_long2 %>% filter(participant_id == 'EN00080'))
# average the 3 week2 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00080", 2)
# average the 3 week4 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00080", 4)

print(phq_long2 %>% filter(participant_id == 'EN00080'))

# EN00082 -----------------------------------------------------------------
print(phq_long2 %>% filter(participant_id == 'EN00082'))
# average the 3 week2 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00082", 2)
# average the 3 week4 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00082", 4)

print(phq_long2 %>% filter(participant_id == 'EN00082'))

# EN00083 -----------------------------------------------------------------
print(phq_long2 %>% filter(participant_id == 'EN00083'))
# average the 3 week2 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00083", 2)
# average the 3 week4 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00083", 4)

print(phq_long2 %>% filter(participant_id == 'EN00083'))

# EN00084 -----------------------------------------------------------------
print(phq_long2 %>% filter(participant_id == 'EN00084'))
# average the 3 week4 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00084", 4)

print(phq_long2 %>% filter(participant_id == 'EN00084'))

# EN00129 -----------------------------------------------------------------
print(phq_long2 %>% filter(participant_id == 'EN00129'))
# average the 2 week2 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00129", 2)
# average the 2 week4 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN00129", 4)

print(phq_long2 %>% filter(participant_id == 'EN00129'))

# EN05022 -----------------------------------------------------------------
print(phq_long2 %>% filter(participant_id == 'EN05022'))
#drop: the 2016-09-24 week4 entry
phq_long2 <- phq_long2 %>%
  filter(!(participant_id == 'EN05022' & phq9Date == '2016-09-24'))

print(phq_long2 %>% filter(participant_id == 'EN05022'))

# EN05039 -----------------------------------------------------------------
print(phq_long2 %>% filter(participant_id == 'EN05039'))
# average the 2 week2 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN05039", 2)
# average the 2 week4 rows
phq_long2 <- replace_with_average_week_data(phq_long2, "EN05039", 4)

print(phq_long2 %>% filter(participant_id == 'EN05039'))


# check again if there are 7 rows per participant
n_distinct(phq_long2$participant_id)
nrow(phq_long2) / 7 # some participants may have multiple responses for the same week

# yay


# organize dataframe ------------------------------------------------------
phq_long3 <- phq_long2 %>%
  arrange(participant_id, week)


# examine missingness -----------------------------------------------------

# Count rows with NA in "sum_phq9" column. 
# These rows will have at least one non-NA value in phq_1 to phq_9
sum(is.na(phq_long3$sum_phq9))
sum(is.na(phq_long3$sum_phq9)) / nrow(phq_long3) * 100 # percent of fully missing weeks 


# Count the number of participants with NA in "sum_phq9" column
phq_long3 %>%
  filter(is.na(sum_phq9)) %>%
  pull(participant_id) %>%
  n_distinct()



# Count the number of participants with more than 3 rows of NA in "sum_phq9" 
phq_long3 %>%
  group_by(participant_id) %>%
  filter(sum(is.na(sum_phq9)) > 3) %>%
  ungroup() %>%
  pull(participant_id) %>%
  n_distinct()




which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_1))
which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_2))
which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_3))
which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_4))
which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_5))
which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_6))
which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_7))
which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_8))
which(is.na(phq_long3$sum_phq9) != is.na(phq_long3$phq9_9))



# Remove participants with more than 3 rows of data with NA in "sum_phq9"
phq_long3 <- phq_long3 %>%
  group_by(participant_id) %>%
  filter(sum(!is.na(sum_phq9)) > 3) %>%
  ungroup()


options(dplyr.width = Inf)

# participants left in analysis
n_distinct(phq_long3$participant_id)



# imputation --------------------------------------------------------------

























