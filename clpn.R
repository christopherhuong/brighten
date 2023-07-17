
library(tidyverse)


dat <- read.csv('phq9.csv')


dat1 <- drop_na(dat)


glimpse(dat)

dat[1,3]
dat[1,3] <- "BLUE-00050"
dat[1,3]

dat$participant_id <- as.factor(dat$participant_id)

summary(dat$participant_id)



