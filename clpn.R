
library(tidyverse)
library(janitor)

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
  filter(ROW_ID != 3703)

table(phq9_long1$week)
# we good
rm(phq9_long2, weeks_to_keep,  participants_with_more_than_one_row)


#set up empty data frame to hold model variables only
dat <- data.frame(phq9_long1$participant_id)



## Set number of covariates
numCovar <- 4
## Set number of symptoms
k <- 9

## Set up empty matrix of coefficients
adjMatCov.early <- matrix(0, (numCovar+k), (numCovar+k)) 
adjMatCov.middle <- matrix(0, (numCovar+k), (numCovar+k)) 

## Estimate CLPN for early
for (i in 1:k) {
  set.seed(1)
  lassoregCov.early <- cv.glmnet(data.matrix(earlyDat[,c(1:k,(k*2+1):(k*2+numCovar))]), earlyDat[,(k+i)], nfolds=10,
                                 family="binomial", alpha=1, standardize=TRUE)
  lambdaCov.early <- lassoregCov.early$lambda.min
  adjMatCov.early[(1:(k+numCovar)),i] <- coef(lassoregCov.early, s=lambdaCov.early, exact=FALSE)[2:(numCovar+k+1)] 
}

## Estimate CLPN for middle
for (i in 1:k) {
  set.seed(1)
  lassoregCov.middle <- cv.glmnet(data.matrix(middleDat[,c(1:k,(k*2+1):(k*2+numCovar))]), middleDat[,(k+i)], nfolds=10,
                                  family="binomial", alpha=1, standardize=TRUE)
  lambdaCov.middle <- lassoregCov.middle$lambda.min
  adjMatCov.middle[(1:(k+numCovar)),i] <- coef(lassoregCov.middle, s=lambdaCov.middle, exact=FALSE)[2:(numCovar+k+1)] 
}

## Adjacency matrix
adjMatCov.early <- getWmat(adjMatCov.early, nNodes=k+NumCovar, labels=labelsCov, directed=T) # note: DVs are in columns
adjMatCov.middle <- getWmat(adjMatCov.middle, nNodes=k+NumCovar, labels=labelsCov, directed=T) # note: DVs are in columns

# Remove covariates from adjacency matrix
adjMat.early <- adjMatCov.early[1:k, 1:k]
adjMat.middle <- adjMatCov.middle[1:k, 1:k]

## Convert from logit/log odds to odds ratios
adjMatOR.early <- exp(adjMat.early)
adjMatOR.middle <- exp(adjMat.middle)
adjMatOR.early
adjMatOR.middle
CLPN.OR <- function(data) {
  ## create empty adjacency matrix
  adjMatCovCLPN <- matrix(0, k+numCovar, k+numCovar) 
  ## run CLPN loop to do series of nodewise regularized regressions
  for (i in 1:9) {
    # set.seed(1) # commented out so that it doesn't give the same answer every time when bootstrapping
    lassoreg <- cv.glmnet(x=data.matrix(data[,c(1:k,(k*2+1):(k*2+numCovar))]), 
                          y=data[,(k+i)], nfolds=10, grouped=TRUE,
                          family="binomial", alpha=1, standardize=TRUE)
    lambda <- lassoreg$lambda.min
    ## paste coefficients into adjacency matrix
    adjMatCovCLPN[1:(k+numCovar),i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(k+numCovar+1)]
  }
  ## remove covariates from adjacency matrix
  adjMatCLPN <- adjMatCovCLPN[1:k, 1:k]
  # Convert from logit/log odds to odds ratios
  adjMatCLPN.OR <- exp(adjMatCLPN)
  return(adjMatCLPN.OR)
  #return(adjMatCLPN)
}

## Estimate networks in bootnet (for bootstrapping)
set.seed(1)
net.early <- estimateNetwork(earlyDat, fun=CLPN.OR, labels=labels, directed=T)
set.seed(1)
net.middle <- estimateNetwork(middleDat, fun=CLPN.OR, labels=labels, directed=T)
set.seed(1)
nonParBoot.early.or <- bootnet(net.early, type="nonparametric", nBoots=5000, directed=T,
                               statistics=c("edge"),
                               ncores=8)

set.seed(1)
nonParBoot.middle.or <- bootnet(net.middle, type="nonparametric", nBoots=5000, directed=T,
                                statistics=c("edge"),
                                ncores=8)
#estimate proportion of time bootstrapped edge > 1
#this is because the 95% CI's don't really reflect confidence intervals
#see https://psych-networks.com/bootstrapping-edges-after-regularization-clarifications-tutorial/
early.bootTable <-nonParBoot.early.or$bootTable
early_edge_conf <-early.bootTable %>% group_by(id) %>% 
  mutate(total_count = n(),
         conf = ifelse(value > 1 | value < 1, 1, 0)) %>%
  summarize(conf_count = sum(conf),
            Confidence = conf_count/total_count) %>%
  distinct(id, .keep_all = T) %>%
  separate(id, c("nodeOut", "nodeIn"), sep="->") %>% select(-conf_count)
## `summarise()` has grouped output by 'id'. You can override using the `.groups` argument.
middle.bootTable <-nonParBoot.middle.or$bootTable
middle_edge_conf <- middle.bootTable %>% group_by(id) %>% 
  mutate(total_count = n(),
         conf = ifelse(value > 1| value < 1, 1, 0)) %>%
  summarize(conf_count = sum(conf),
            Confidence = conf_count/total_count) %>%
  distinct(id, .keep_all = T) %>%
  separate(id, c("nodeOut", "nodeIn"), sep="->") %>% select(-conf_count)
## `summarise()` has grouped output by 'id'. You can override using the `.groups` argument.
## Identify the strongest edges (note: row=IV, col=DV)
res.early <- order(adjMatOR.early, decreasing = T)[seq_len(81)]
pos.early <- arrayInd(res.early, dim(adjMatOR.early), useNames = TRUE)
posWithLabs.early <- data.frame(nodeOut=labels[pos.early[,1]], 
                                nodeIn=labels[pos.early[,2]],
                                value=adjMatOR.early[res.early])
Early.edges<-posWithLabs.early %>% mutate(Odds_Ratio = value) %>%
  left_join(early_edge_conf, by=c("nodeOut", "nodeIn"))%>% drop_na() %>% filter(Odds_Ratio > 1 | Odds_Ratio < 1) #drop_na omits the autoregressive edges


res.middle <- order(adjMatOR.middle, decreasing = T)[seq_len(81)]
pos.middle <- arrayInd(res.middle, dim(adjMatOR.middle), useNames = TRUE)
posWithLabs.middle <- data.frame(nodeOut=labelsCov[1:9][pos.middle[,1]],
                                 nodeIn=labelsCov[1:9][pos.middle[,2]],
                                 value=adjMatOR.middle[res.middle])
Middle.edges<-posWithLabs.middle %>% mutate(Odds_Ratio = value) %>% select(-c(value)) %>%
  left_join(middle_edge_conf, by=c("nodeOut", "nodeIn")) %>% drop_na()%>% filter(Odds_Ratio > 1 | Odds_Ratio < 1)

#Table 2 in Manuscript
joined_table <- left_join(Early.edges, Middle.edges, by=c("nodeOut", "nodeIn"))
colnames(joined_table) <- c("nodeOut", "nodeIn", "Odds_Ratio_Early", "Confidence_Early", "Odds_Ratio_Middle", "Confidence_Middle")
joined_table












