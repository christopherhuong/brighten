
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



library(tidyverse)
library(psych)
library(car)
library(NetworkComparisonTest)
library(mgm)
library(bootnet)
library(qgraph)
library(psychTools)
library(glmnet)
library(qgraph)
library(lavaan)


load('phq_imp.RData')
load('phq_imp_long.RData')
dat_w <- phq_imp             #wide
dat_l <- phq_imp_long        #long
rm(phq_imp, phq_imp_long)

covs <- c("gender", "education", "working", "marital_status", "race", "age", "study_arm", "study", "gad7_sum")
vars <- c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9")


# separate data sets by week
dat0 <- dat_l %>% filter(week == 0) %>% select(all_of(vars))
dat2 <- dat_l %>% filter(week == 2) %>% select(all_of(vars))
dat4 <- dat_l %>% filter(week == 4) %>% select(all_of(vars))
dat6 <- dat_l %>% filter(week == 6) %>% select(all_of(vars))


dat0_2 <- data.frame(dat0, dat2,
                     select(dat_w, all_of(covs)))

dat2_4 <- data.frame(dat2, dat4,
                     select(dat_w, all_of(covs)))

dat4_6 <- data.frame(dat4, dat6,
                      select(dat_w, all_of(covs)))




## Set number of covariates
numCovar <- 9
## Set number of nodes
k <- 9


# CLPN #################################################


# 1st network; baseline to w2
adjMat <- matrix(0, (numCovar+k), (numCovar+k))

for (i in 1:k) {
  set.seed(1)
  lassoreg <- cv.glmnet(data.matrix(dat0_2[,c(1:k,(k*2+1):(k*2+numCovar))]), dat0_2[,(k+i)],
                        family="gaussian", alpha=1, standardize=TRUE)
  lambda <- lassoreg$lambda.min
  adjMat[(1:(k+numCovar)),i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(numCovar+k+1)] 
}


labels <- c('PHQ1', 'PHQ2', 'PHQ3', 'PHQ4', 'PHQ5', 'PHQ6', 'PHQ7', 'PHQ8', 'PHQ9',
            'gen', 'edu', 'wrk', 'mrt', 'rce', 'age', 's_a', 'stdy', 'gad')

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
  lassoreg <- cv.glmnet(data.matrix(dat2_4[,c(1:k,(k*2+1):(k*2+numCovar))]), dat2_4[,(k+i)],
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





# third network; w4 to w6

adjMat5 <- matrix(0, (numCovar+k), (numCovar+k))

for (i in 1:k) {
  set.seed(1)
  lassoreg <- cv.glmnet(data.matrix(dat4_6[,c(1:k,(k*2+1):(k*2+numCovar))]), dat4_6[,(k+i)],
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

pdf(file="clpn_0_to_6.pdf")
par(mfrow = c(3,1))

nwc1 <- qgraph(adjMat2, nodeNames=names, legend.cex = .5, title = "baseline to week 2",
               threshold = .05, layout=L, asize=7, vsize=8, label.cex=1)

nwc2 <- qgraph(adjMat4, nodeNames=names, legend.cex = .5, title = "week 2 to week 4",
               threshold = .05, layout=L, asize=7, vsize=8, label.cex=1)

nwc3 <- qgraph(adjMat6, nodeNames=names, legend.cex = .5, title = "week 4 to week 6",
               threshold = .05, layout=L, asize=7, vsize=8, label.cex=1)

all_nwc <- list('baseline to w2'=nwc1,'w2 to w4'=nwc2,'w4 to w6'=nwc3)

cplot <- centralityPlot(all_nwc, include = c("InExpectedInfluence", "OutExpectedInfluence"), 
                        scale="z-scores")

dev.off()















