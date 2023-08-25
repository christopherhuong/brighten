library(tidyverse)




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


#count maximum consecutive days of data
gl <- function(x) {
  y <- c(unclass(diff(x)))  # c and unclass -- preparing it for rle
  r <- rle(y)
  with(r, max(lengths[values==1]))
}

max_days <- dat %>% group_by(id) %>% summarise(max.consecutive = gl(date))

#retain participants with >50 consecutive days
dat <- merge(dat, max_days, by = "id")

dat <- dat %>%
  filter(max.consecutive > 50) %>%
  select(-max.consecutive)

n_distinct(dat$id)

#7 participants left for personalized networks

#manually subset observations with the consecutive days
dat80 <- dat %>%
  filter(id == "EN00080", date >= "2016-10-06", date <= "2016-12-03")

dat101 <- dat %>%
  filter(id == "EN00101", date >= "2016-09-23", date <= "2016-11-30")

dat301 <- dat %>%
  filter(id == "EN00301", date >= "2016-09-12", date <= "2016-11-10")

dat399 <- dat %>%
  filter(id == "EN00399", date >= "2016-11-14", date <= "2017-01-25")

dat590 <- dat %>%
  filter(id == "EN00590", date >= "2016-11-14", date <= "2017-02-04")

dat647 <- dat %>%
  filter(id == "EN00647", date >= "2016-12-22", date <= "2017-02-19")

dat5254 <- dat %>%
  filter(id == "EN05254", date >= "2017-01-11", date <= "2017-03-03")


#check if row numbers match consecutive days, reinspect if not
# print(max_days %>%
#         filter(id == "EN00080" | id == "EN00101" |
#                id == "EN00301" | id == "EN00399" |
#                id == "EN00590" | id == "EN00647" |
#                id == "EN05254"))
rm(max_days, phq, pv, gl)

#inspect for bad responses on phq
rm(dat101, dat301)

# personalized networks ---------------------------------------------------


net80 <- graphicalVAR(dat80, nLambda = 10, verbose = T, gamma = 0.5,
         scale = T, vars =c("active", "sleep", "station", "phq1", "phq2"))
plot(net80, "PDC", layout="spring")


###########

net399 <- graphicalVAR(dat399, nLambda = 10, verbose = T, gamma = 0.5,
                      scale = T, vars =c("active", "sleep", "station", "phq1", "phq2"))
plot(net399, "PDC", layout="spring")

#############

net590 <- graphicalVAR(dat590, nLambda = 10, verbose = T, gamma = 0.5,
                      scale = T, vars =c("active", "sleep", "station", "phq1", "phq2"))
plot(net590, "PDC", layout="spring")

############

net647 <- graphicalVAR(dat647, nLambda = 10, verbose = T, gamma = 0.5,
                      scale = T, vars =c("active", "sleep", "station", "phq1", "phq2"))
plot(net647, "PDC", layout="spring")

############

net5254 <- graphicalVAR(dat5254, nLambda = 10, verbose = T, gamma = 0.5,
                      scale = T, vars =c("active", "sleep", "station", "phq1", "phq2"))
plot(net5254, "PDC", layout="spring")







library(psychonetrics)

pnet80 <- gvar(dat80, estimator = "FIML",
               vars = c("active", "sleep", "station", "phq1", "phq2"),
               standardize = "z")
pnet80 <- pnet80 %>% runmodel
#matrix not positive-definite
pnet80 <- pnet80 %>% 
  modelsearch(prunealpha = 0.01, addalpha = 0.01) 



