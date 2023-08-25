library(tidyverse)
library(mlVAR)



phq <- read.csv("datasets/phq2.csv")

# pv1 <- read.csv("datasets/passive_v1.csv")
pv <- read.csv("datasets/passive_v2_mobility.csv")



#select columns, rename
phq <- phq %>%
  select(c(participant_id, dt_yesterday, phq2_1, phq2_2))
colnames(phq) <- c("id", "date", "phq1", "phq2")

pv <- pv %>%
  select(c(participant_id, dt_passive, 
           hours_active, hours_of_sleep, hours_stationary))
colnames(pv) <- c("id", "date", "active", "sleep", "station")

#check NAs
sum(is.na(phq))# 14 NA's
phq <- na.omit(phq)
sum(is.na(pv)) #zero NA's

#convert dates
phq$date <- as.Date(phq$date, format = "%Y-%m-%d")
pv$date <- as.Date(pv$date, format = "%Y-%m-%d")

#rearrange
phq <- phq %>% arrange(id, date)
pv <- pv %>% arrange(id, date)


#merge data frames and remove NAs
dat <- pv %>%
  left_join(phq, by = c("id", "date"))

sum(is.na(dat))
dat <- na.omit(dat)


#assign day number based on first date = 1
dat <- dat %>%
  group_by(id) %>%
  mutate(day = as.integer(date - min(date)) + 1)


#replace duplicate id+dates with their average
dat <- dat %>%
  group_by(id, date) %>%
  mutate(across(c(active, sleep, station, phq1, phq2), ~ mean(., na.rm = TRUE))) %>%
  distinct() %>%
  ungroup()


#remove id's with less than 20 observations
dat <- dat %>%
  group_by(id) %>%
  filter(n() > 20) %>%
  ungroup()


n_distinct(dat$id) # n = 130



# temporal network estimation ---------------------------------------------

net <- mlVAR(dat, 
             vars = c("active", "sleep", "station", "phq1", "phq2"),
             idvar = "id",
             beepvar = "day")


plot(net, "temporal", layout = "spring")
plot(net, "contemporaneous", layout = "spring")


























