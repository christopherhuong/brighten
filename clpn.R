
library(tidyverse)


phq9_long <- read.csv('datasets/phq9_long.csv')
phq9_long <- drop_na(phq9_long)
# dropped 8 rows
phq9_baseline <- read.csv('datasets/phq9_baseline.csv')

demo <- read.csv('datasets/demographics_baseline.csv')

# passive_v1 <- read.csv('datasets/passive_v1.csv')
         
         

            

# dat <- dat %>%
#   group_by(participant_id) %>%
#   summarise('phq9 mean' = mean(sum_phq9),
#             'phq9 sd' = sd(sum_phq9))
# 


# race by study, v2 has a significant hispanic/latino sample
demo %>%
  group_by(study, race) %>%
  count()



# keep only participants with data in weeks 2,4,6,8,10,12
weeks_to_keep <- c(2, 4, 6, 8, 10, 12)

phq9_long1 <- phq9_long %>%
  group_by(participant_id) %>%
  filter(all(weeks_to_keep %in% week)) %>%
  ungroup() %>%
  filter(week %in% weeks_to_keep)

# n = 272


table(phq9_long1$week)
# why does week 4 have one more observation than others. 
# maybe a participant has 2 rows for week 4. check for this

phq9_long2 <- phq9_long1 %>%
  group_by(participant_id, week) %>%
  mutate(week_count = n()) %>%
  ungroup()


participants_with_more_than_one_row <- phq9_long2 %>%
  filter(week_count > 1) %>%
  select(participant_id, week) %>%
  distinct()

participants_with_more_than_one_row

#participant EN05022 has 2 rows for week 4
View(filter(phq9_long1, participant_id == 'EN05022'))
# row 3703 seems to be off, based on date

phq9_long1 <- phq9_long1 %>%
  filter(ROW_ID != 3703) %>%
  select(-c(ROW_ID, ROW_VERSION, sum_phq9, phq9Date))

table(phq9_long1$week)
# we good
rm(phq9_long2, weeks_to_keep,  participants_with_more_than_one_row)


library(psych)
library(car)
library(NetworkComparisonTest)
library(ggplot2)
library(mgm)
library(mice)
library(bootnet)
library(qgraph)
library(psychTools)
library(glmnet)
library(qgraph)
library(lavaan)
library(mice)
library(readr)



# create dataframe with baseline data, matching participant_id with values found in phq9_long1
participant_list <- as.character(unique(phq9_long1$participant_id))

w0 <- phq9_baseline %>%
  filter(participant_id %in% participant_list) 




# why does our baseline only have 220/272 participants? 
# now have to reduce the analytic set further to 220
participant_list <- as.character(unique(w0$participant_id))

w0 <- w0 %>%
  select(-c(ROW_ID, ROW_VERSION, participant_id, study, baselinePHQ9date))

# create dataframe with week 2 data 
w2 <- phq9_long1 %>%
  filter(week ==2) %>%
  filter(participant_id %in% participant_list) %>%
  select(-c(participant_id, week)) 

# create dataframe with week 4 data
w4 <- phq9_long1 %>%
  filter(week ==4) %>%
  filter(participant_id %in% participant_list) %>%
  select(-c(participant_id, week)) 

# create dataframe with week 12 data
w12 <- phq9_long1 %>%
  filter(week ==12) %>%
  filter(participant_id %in% participant_list) %>%
  select(-c(participant_id, week)) 

# create dataframe with demographic variables of participants in the analytic set
phq_demo <- demo %>%
  filter(participant_id %in% participant_list)


# create networks and covariates
w0_w2 <- data.frame(w0, w2)
w2_w4 <- data.frame(w2, w4)
w4_w12 <- data.frame(w4, w12)

w0_w2$gender <- as.factor(phq_demo$gender)
w2_w4$gender <- as.factor(phq_demo$gender)
w4_w12$gender <- as.factor(phq_demo$gender)

w0_w2$ethnicity <- as.factor(phq_demo$race)
w2_w4$ethnicity <- as.factor(phq_demo$race)
w4_w12$ethnicity <- as.factor(phq_demo$race)

rm(participant_list)
## Set number of covariates
numCovar <- 2
## Set number of nodes
k <- 9


# longitudinal network analysis #################################################
# 1st network; baseline to w2
adjMat <- matrix(0, (numCovar+k), (numCovar+k))

for (i in 1:k) {
  set.seed(1)
  lassoreg <- cv.glmnet(data.matrix(w0_w2[,c(1:k,(k*2+1):(k*2+numCovar))]), w0_w2[,(k+i)],
                        family="gaussian", alpha=1, standardize=TRUE)
  lambda <- lassoreg$lambda.min
  adjMat[(1:(k+numCovar)),i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(numCovar+k+1)] 
}


labels <- c('anh', 'depr', 'slp', 'ftg', 'apt', 'bad', 'con', 'mot', 'si',
  'sex', 'eth')
names <- c('Anhedonia', 'Down, depressed, hopeless', 'Sleep dysregulation',
           'Tired or having little energy', 'Appetite dysregulation',
           'Feeling bad about yourself', 'Trouble concentrating', 'Motor slow/fidgity',
           'Suicidal ideation')

adjMat <- getWmat(adjMat, nNodes=k+NumCovar, labels=labels, directed=T) # note: DVs are in columns
adjMat <- adjMat[1:k, 1:k]
qgraph(adjMat, nodeNames = names)

# create network without autoregressive effects
adjMat2 <- adjMat
diag(adjMat2) <- 0
nwc1 <- qgraph(adjMat2, threshold = .05, nodeNames = names)




# second network; w2 to w4

adjMat3 <- matrix(0, (numCovar+k), (numCovar+k))

for (i in 1:k) {
  set.seed(1)
  lassoreg <- cv.glmnet(data.matrix(w2_w4[,c(1:k,(k*2+1):(k*2+numCovar))]), w2_w4[,(k+i)],
                        family="gaussian", alpha=1, standardize=TRUE)
  lambda <- lassoreg$lambda.min
  adjMat3[(1:(k+numCovar)),i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(numCovar+k+1)] 
}




adjMat3 <- getWmat(adjMat3, nNodes=k+NumCovar, labels=labels, directed=T) # note: DVs are in columns
adjMat3 <- adjMat3[1:k, 1:k]
qgraph(adjMat3, nodeNames = names)

adjMat4 <- adjMat3
diag(adjMat4) <- 0
nwc2 <- qgraph(adjMat4, threshold = .05, nodeNames = names)





# third network; w4 to w12

adjMat5 <- matrix(0, (numCovar+k), (numCovar+k))

for (i in 1:k) {
  set.seed(1)
  lassoreg <- cv.glmnet(data.matrix(w4_w12[,c(1:k,(k*2+1):(k*2+numCovar))]), w4_w12[,(k+i)],
                        family="gaussian", alpha=1, standardize=TRUE)
  lambda <- lassoreg$lambda.min
  adjMat5[(1:(k+numCovar)),i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(numCovar+k+1)] 
}




adjMat5 <- getWmat(adjMat5, nNodes=k+NumCovar, labels=labels, directed=T) # note: DVs are in columns
adjMat5 <- adjMat5[1:k, 1:k]
qgraph(adjMat5, nodeNames = names)

adjMat6 <- adjMat5
diag(adjMat6) <- 0
nwc2 <- qgraph(adjMat6, threshold = .05, nodeNames = names)


# graphical presentation of the longitudinal networks ###########################
L <- averageLayout(adjMat2,adjMat4,adjMat6)

pdf(file="clpn.pdf")
par(mfrow = c(3,1))

nwc1 <- qgraph(adjMat2, nodeNames=names, legend.cex = .5, title = "baseline to week 2",
               threshold = .05, layout=L, asize=7, vsize=8, label.cex=1)

nwc2 <- qgraph(adjMat4, nodeNames=names, legend.cex = .5, title = "week 2 to week 4",
               threshold = .05, layout=L, asize=7, vsize=8, label.cex=1)

nwc3 <- qgraph(adjMat6, nodeNames=names, legend.cex = .5, title = "week 4 to week 12",
               threshold = .05, layout=L, asize=7, vsize=8, label.cex=1)

all_nwc <- list('baseline to w2'=nwc1,'w2 to w4'=nwc2,'w4 to w12'=nwc3)

cplot <- centralityPlot(all_nwc, include = c("InExpectedInfluence", "OutExpectedInfluence"), 
                        scale="z-scores")

dev.off()















