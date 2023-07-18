
library(tidyverse)


dat <- read.csv('phq9.csv')


dat <- drop_na(dat)


glimpse(dat)

dat[1,3]
dat[1,3] <- "BLUE-00050"
dat[1,3]

dat$participant_id <- as.factor(dat$participant_id)

summary(dat$participant_id)
table(dat$participant_id)


dat <- dat %>%
  group_by(participant_id) %>%
  summarise('phq9 mean' = mean(sum_phq9),
            'phq9 sd' = sd(sum_phq9))



















