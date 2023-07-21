################################################################################
################################################################################
#Code to accompany Odenthal, Schlechter, Benke, & Pané-Farré (accepted) 
# "Temporal dynamics in mental health symptoms and loneliness during the 
#COVID-19 pandemic in a longitudinal probability sample: a network analysis"
################################################################################
################################################################################

################################################################################
#first part: load packages and data ############################################
################################################################################

#set working directory ##########################################
setwd("~/Desktop/RStudio wd")

# install and load required package #############################
##load libraries
library(dplyr)
library(haven)
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

#load imputed data #############################################################
CoV_l_2 <- read_delim("CoV_imp.csv", ";", 
                      escape_double = FALSE, trim_ws = TRUE)

################################################################################
#second part: network analysis##################################################
################################################################################

#data preparation for network analysis #########################################
w1b <- data.frame(CoV_l_2[,2:14])
colnames(w1b) <- c('loneliness','conc','sleep','useful','decision','strain','prob_diff','enjoy','face_prob',
                   'depressed','lose_conf','worthless','gen_happy')
w6b <- data.frame(CoV_l_2[,67:79])
colnames(w6b) <- c('loneliness','conc','sleep','useful','decision','strain','prob_diff','enjoy','face_prob',
                   'depressed','lose_conf','worthless','gen_happy')
w7b <- data.frame(CoV_l_2[,80:92])
colnames(w7b) <- c('loneliness','conc','sleep','useful','decision','strain','prob_diff','enjoy','face_prob',
                   'depressed','lose_conf','worthless','gen_happy')
w10p<- data.frame(CoV_l_2[,119:131])
colnames(w10p) <- c('loneliness','conc','sleep','useful','decision','strain','prob_diff','enjoy','face_prob',
                    'depressed','lose_conf','worthless','gen_happy')

w10p[,1:13] <- lapply(w10p[,1:13],as.numeric)
network_w1 <- estimateNetwork(w10p, default="EBICglasso")

w1b[,1:13] <- lapply(w1b[,1:13],as.numeric)
network_w2 <- estimateNetwork(w1b, default="EBICglasso")

w6b[,1:13] <- lapply(w6b[,1:13],as.numeric)
network_w3 <- estimateNetwork(w6b, default="EBICglasso")

w7b[,1:13] <- lapply(w7b[,1:13],as.numeric)
network_w4 <- estimateNetwork(w7b, default="EBICglasso")

#create networks and include covariates
p10_w1 <- data.frame(w10p, w1b)
w1_6 <- data.frame(w1b, w6b)
w6_7 <- data.frame(w6b, w7b)

p10_w1$gender <- CoV_l_2$sex
w1_6$gender <- CoV_l_2$sex
w6_7$gender <- CoV_l_2$sex

p10_w1$ethnicity <- CoV_l_2$ethnic
w1_6$ethnicity <- CoV_l_2$ethnic
w6_7$ethnicity <- CoV_l_2$ethnic

#longitudinal network analysis #################################################
#1st network
numCovar <- 2
k <- 13
adjMat <- matrix(0, (numCovar+k), (numCovar+k))

for (i in 1:k) {
  set.seed(1)
  lassoreg <- cv.glmnet(data.matrix(p10_w1[,c(1:k,(k*2+1):(k*2+numCovar))]), p10_w1[,(k+i)],
                        family="gaussian", alpha=1, standardize=TRUE)
  lambda <- lassoreg$lambda.min
  adjMat[(1:(k+numCovar)),i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(numCovar+k+1)] 
}

groups <- c(rep("lone", 1), rep("ghq", 12)) 
labels <- c('lon','cnc','slp','use','dec','str','dif','joy','prb',
            'dep','cnf','wth','hap', 'sex', 'eth')

adjMat <- getWmat(adjMat, nNodes=k+NumCovar, labels=labels, directed=T) # note: DVs are in columns
adjMat <- adjMat[1:k, 1:k]
qgraph(adjMat, groups = groups, legend = FALSE,
       colors = c("#E69F00", "#56B4E9"))

adjMat2 <- adjMat
diag(adjMat2) <- 0
nwc1 <- qgraph(adjMat2, groups = groups, legend = FALSE, threshold = .05,
               colors = c("#E69F00", "#56B4E9"))

#2nd network
numCovar <- 2
k <- 13
adjMat3 <- matrix(0, (numCovar+k), (numCovar+k)) 

for (i in 1:k) {
  set.seed(1)
  lassoreg <- cv.glmnet(data.matrix(w1_6[,c(1:k,(k*2+1):(k*2+numCovar))]), w1_6[,(k+i)],
                        family="gaussian", alpha=1, standardize=TRUE)
  lambda <- lassoreg$lambda.min
  adjMat3[(1:(k+numCovar)),i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(numCovar+k+1)] 
}
groups <- c(rep("lone", 1), rep("ghq", 12)) 
labels <- c('lon','cnc','slp','use','dec','str','dif','joy','prb',
            'dep','cnf','wth','hap', 'sex', 'eth')

adjMat3 <- getWmat(adjMat3, nNodes=k+NumCovar, labels=labels, directed=T) # note: DVs are in columns
adjMat3 <- adjMat3[1:k, 1:k]

qgraph(adjMat3, groups = groups, legend = FALSE,
       colors = c("#E69F00", "#56B4E9"))

adjMat4 <- adjMat3
diag(adjMat4) <- 0 

nwc2 <- qgraph(adjMat4, groups = groups, legend = FALSE, threshold = .05,
               colors = c("#E69F00", "#56B4E9"))

#3rd network
numCovar <- 2
k <- 13
adjMat5 <- matrix(0, (numCovar+k), (numCovar+k)) 

for (i in 1:k) {
  set.seed(1)
  lassoreg <- cv.glmnet(data.matrix(w6_7[,c(1:k,(k*2+1):(k*2+numCovar))]), w6_7[,(k+i)],
                        family="gaussian", alpha=1, standardize=TRUE)
  lambda <- lassoreg$lambda.min
  adjMat5[(1:(k+numCovar)),i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(numCovar+k+1)] 
}

groups <- c(rep("lone", 1), rep("ghq", 12)) 
labels <- c('lon','cnc','slp','use','dec','str','dif','joy','prb',
            'dep','cnf','wth','hap', 'sex', 'eth')

adjMat5 <- getWmat(adjMat5, nNodes=k+NumCovar, labels=labels, directed=T) # note: DVs are in columns
adjMat5 <- adjMat5[1:k, 1:k]

qgraph(adjMat5, groups = groups, legend = FALSE,
       colors = c("#E69F00", "#56B4E9"))

adjMat6 <- adjMat5
diag(adjMat6) <- 0 

nwc3 <- qgraph(adjMat6, groups = groups, legend = FALSE, threshold = .05,
               colors = c("#E69F00", "#56B4E9"))

#graphical presentation of the longitudinal networks ###########################
L <- averageLayout(adjMat2,adjMat4,adjMat6)

nwc1 <- qgraph(adjMat2, groups = groups, legend = FALSE, threshold = .05, layout=L, asize=7, vsize=8, label.cex=1,
               colors = c("#E69F00", "#56B4E9"))
nwc2 <- qgraph(adjMat4, groups = groups, legend = FALSE, threshold = .05, layout=L, asize=7, vsize=8, label.cex=1,
               colors = c("#E69F00", "#56B4E9"))
nwc3 <- qgraph(adjMat6, groups = groups, legend = FALSE, threshold = .05, layout=L, asize=7, vsize=8, label.cex=1,
               colors = c("#E69F00", "#56B4E9"))

all_nwc <- list('1 pre-Covid to first peak'=nwc1,'2 first peak to second peak'=nwc2,'3 second peak to third peak'=nwc3)

centralityPlot(all_nwc, include = c("InExpectedInfluence", "OutExpectedInfluence"), scale="z-scores")

################################################################################
#third part: bootstrapping######################################################
################################################################################

#create data frames without sex and ethnicity ##################################
p10_w1a <- data.frame(w10p, w1b)
w1_6a <- data.frame(w1b, w6b)
w6_7a <- data.frame(w6b, w7b)

#bootstrap preparation #########################################################
k <- 13 #k is the number of variables at each time point
CLPN.OR <- function(data) {
  
  adjMatCovCLPN <- matrix(0, k, k) 
  
  for (i in 1:13) {
    lassoreg <- cv.glmnet(as.matrix(data[,1:k]), data[,(k+i)],
                          family="gaussian", alpha=1, standardize=TRUE)
    lambda <- lassoreg$lambda.min
    adjMatCovCLPN[1:k,i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(k+1)] 
  }
  adjMat <- adjMatCovCLPN
  return(adjMat)
}

labels1 <- c('loneliness','conc','sleep','useful','decision','strain','prob_diff','enjoy','face_prob',
             'depressed','lose_conf','worthless','gen_happy')

#estimate networks in bootnet (for bootstrapping) ##############################
net.1 <- estimateNetwork(p10_w1a, fun=CLPN.OR, labels=labels1, directed=T)
plot(net.1)

net.2 <- estimateNetwork(w1_6a, fun=CLPN.OR, labels=labels1, directed=T)
plot(net.2)

net.3 <- estimateNetwork(w6_7a, fun=CLPN.OR, labels=labels1, directed=T)
plot(net.3)

#bootstrapping #################################################################
#1
nonParBoot.w1 <- bootnet(net.1, type="nonparametric", nBoots=1000, directed=T,
                         statistics=c("edge","outExpectedInfluence","inExpectedInfluence"),
                         ncores=4)
caseBoot.w1 <- bootnet(net.1, type="case", nBoots=1000, directed=T,
                       statistics=c("outExpectedInfluence","inExpectedInfluence"))

plot(nonParBoot.w1,order='sample', labels=F)
plot(caseBoot.w1, statistics=c("outExpectedInfluence","inExpectedInfluence"))
plot(nonParBoot.w1,'outExpectedInfluence', plot='difference',order='sample')
plot(nonParBoot.w1,'inExpectedInfluence', plot='difference',order='sample')
plot(nonParBoot.w1, "edge", plot="difference", onlyNonZero = T, order="sample")

#2
nonParBoot.w2 <- bootnet(net.2, type="nonparametric", nBoots=1000, directed=T,
                         statistics=c("edge","outExpectedInfluence","inExpectedInfluence"),
                         ncores=4)
caseBoot.w2 <- bootnet(net.2, type="case", nBoots=1000, directed=T,
                       statistics=c("outExpectedInfluence","inExpectedInfluence"))

plot(nonParBoot.w2,order='sample', labels=F)
plot(caseBoot.w2, statistics=c("outExpectedInfluence","inExpectedInfluence"))
plot(nonParBoot.w2,'outExpectedInfluence', plot='difference',order='sample')
plot(nonParBoot.w2,'inExpectedInfluence', plot='difference',order='sample')
plot(nonParBoot.w2, "edge", plot="difference", onlyNonZero = T, order="sample")

#3
nonParBoot.w3 <- bootnet(net.3, type="nonparametric", nBoots=1000, directed=T,
                         statistics=c("edge","outExpectedInfluence","inExpectedInfluence"),
                         ncores=4)
caseBoot.w3 <- bootnet(net.3, type="case", nBoots=1000, directed=T,
                       statistics=c("outExpectedInfluence","inExpectedInfluence"))

plot(nonParBoot.w3,order='sample', labels=F)
plot(caseBoot.w3, statistics=c("outExpectedInfluence","inExpectedInfluence"))
plot(nonParBoot.w3,'outExpectedInfluence', plot='difference',order='sample')
plot(nonParBoot.w3,'inExpectedInfluence', plot='difference',order='sample')
plot(nonParBoot.w3, "edge", plot="difference", onlyNonZero = T, order="sample")

################################################################################
#fourth part: edge analysis and edge list ######################################
################################################################################

#first coefficient of similarity
cor.test(c(adjMat2[lower.tri(adjMat2, diag=F)], adjMat2[upper.tri(adjMat2, diag=F)]), 
         c(adjMat4[lower.tri(adjMat4, diag=F)], adjMat4[upper.tri(adjMat4, diag=F)]), 
         method="pearson")

paste0(sum(adjMat2>0 & adjMat4>0) + sum(adjMat2<0 & adjMat4<0), " (", 
       round(100*((sum(adjMat2>0 & adjMat4>0) + sum(adjMat2<0 & adjMat4<0)) / sum(adjMat4!=0)),1), "%)")

paste0(sum(adjMat2>0 & adjMat4>0) + sum(adjMat2<0 & adjMat4<0), " (", 
       round(100*((sum(adjMat2>0 & adjMat4>0) + sum(adjMat2<0 & adjMat4<0)) / sum(adjMat2!=0)),1), "%)") 

#second coefficient of similarity
cor.test(c(adjMat4[lower.tri(adjMat4, diag=F)], adjMat4[upper.tri(adjMat4, diag=F)]), 
         c(adjMat6[lower.tri(adjMat6, diag=F)], adjMat6[upper.tri(adjMat6, diag=F)]), 
         method="pearson")

paste0(sum(adjMat4>0 & adjMat6>0) + sum(adjMat4<0 & adjMat6<0), " (", 
       round(100*((sum(adjMat4>0 & adjMat6>0) + sum(adjMat4<0 & adjMat6<0)) / sum(adjMat6!=0)),1), "%)") 

paste0(sum(adjMat4>0 & adjMat6>0) + sum(adjMat4<0 & adjMat6<0), " (", 
       round(100*((sum(adjMat4>0 & adjMat6>0) + sum(adjMat4<0 & adjMat6<0)) / sum(adjMat4!=0)),1), "%)")

#correlation of overall out-EI
cor.test(centrality(adjMat2)$OutExpectedInfluence, 
         centrality(adjMat4)$OutExpectedInfluence, 
         method="pearson") # 

#correlation of overall in-EI
cor.test(centrality(adjMat2)$InExpectedInfluence, 
         centrality(adjMat4)$InExpectedInfluence, 
         method="pearson") # 

#correlation of overall in-EI
cor.test(centrality(adjMat2)$InExpectedInfluence, 
         centrality(adjMat6)$InExpectedInfluence, 
         method="pearson") # 

#identify the strongest edges
diag(adjMat2) <- 1
res.w1 <- order(adjMat2, decreasing = T)[seq_len(225)]
pos.w1 <- arrayInd(res.w1, dim(adjMat2), useNames = TRUE)
posWithLabs.w1 <- data.frame(nodeOut=labels1[pos.w1[,1]], 
                             nodeIn=labels1[pos.w1[,2]],
                             value=adjMat2[res.w1],
                             DomainOut=groups[pos.w1[,1]],
                             DomainIn=groups[pos.w1[,2]])

#identify the strongest edges
diag(adjMat4) <- 1
res.w2 <- order(adjMat4, decreasing = T)[seq_len(225)]
pos.w2 <- arrayInd(res.w2, dim(adjMat4), useNames = TRUE)
posWithLabs.w2 <- data.frame(nodeOut=labels1[pos.w2[,1]], 
                             nodeIn=labels1[pos.w2[,2]],
                             value=adjMat4[res.w2],
                             DomainOut=groups[pos.w2[,1]],
                             DomainIn=groups[pos.w2[,2]])

##identify the strongest edges
diag(adjMat6) <- 1
res.w3 <- order(adjMat6, decreasing = T)[seq_len(225)]
pos.w3 <- arrayInd(res.w3, dim(adjMat6), useNames = TRUE)
posWithLabs.w3 <- data.frame(nodeOut=labels1[pos.w3[,1]], 
                             nodeIn=labels1[pos.w3[,2]],
                             value=adjMat6[res.w3],
                             DomainOut=groups[pos.w3[,1]],
                             DomainIn=groups[pos.w3[,2]])

edges <- data.frame(posWithLabs.w1, posWithLabs.w2, posWithLabs.w3)

write.csv2(edges,"edges_new.csv")

################################################################################
################################################################################
################################################################################
################################################################################