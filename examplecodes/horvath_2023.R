#0. LOADING RELEVANT PACKAGES
library(haven)
library(bootnet)
library(qgraph)
library(NetworkComparisonTest)
library(psych)
library(glmnet)
library(dplyr)
#-------------------------------------------------------------------------------
#1. CROSS-SECTIONAL NETWORK ANALYSIS: WAVE 1, PERSISTENT GAMBLERS
#1.1. Network estimation: Wave 1, persistent gamblers
Data_W1_PG <- read_spss("PGSI Lasso.sav")
Data_W1_PG <- Data_W1_PG[complete.cases(Data_W1_PG),]
Data_W1_PG <- data.frame(Data_W1_PG[,1:9])
colnames(Data_W1_PG) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
Network_W1_PG <- estimateNetwork(Data_W1_PG, default = "IsingFit", rule = "AND", tuning = 0.25)
summary(Network_W1_PG)
plot(Network_W1_PG, title = "Wave 1")
print(Network_W1_PG$graph)
#1.2. Centrality estimates: Wave 1, persistent gamblers
centralityTable(Network_W1_PG, standardized = TRUE)
centralityPlot(Network_W1_PG, scale = c("z-scores"), include = c("Betweenness", "Closeness", "Strength"))
#1.3. Network stability and accuracy analyses
Boot_W1_1_PG <- bootnet(Network_W1_PG, nCores = 8, nBoots = 1000, type = "nonparametric", statistics = c("edge", "strength", "closeness", "betweenness"))
Boot_W1_2_PG <- bootnet(Network_W1_PG, nCores = 8, nBoots = 1000, type = "case", statistics = c("strength", "closeness", "betweenness"))
plot(Boot_W1_1_PG, order = "sample")
plot(Boot_W1_1_PG, "strength", order = "sample")
plot(Boot_W1_1_PG, "closeness", order = "sample")
plot(Boot_W1_1_PG, "betweenness", order = "sample")
plot(Boot_W1_1_PG, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Boot_W1_2_PG, statistics = c("strength", "closeness", "betweenness"))
corStability(Boot_W1_2_PG)
#-------------------------------------------------------------------------------
#2. CROSS-SECTIONAL NETWORK ANALYSIS: WAVE 2, PERSISTENT GAMBLERS
#2.1. Network estimation: Wave 2, persistent gamblers
Data_W2_PG <- read_spss("PGSI Lasso.sav")
Data_W2_PG <- Data_W2_PG[complete.cases(Data_W2_PG),]
Data_W2_PG <- data.frame(Data_W2_PG[,10:18])
colnames(Data_W2_PG) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
Network_W2_PG <- estimateNetwork(Data_W2_PG, default = "IsingFit", rule = "AND", tuning = 0.25)
summary(Network_W2_PG)
plot(Network_W2_PG, title = "Wave 2")
print(Network_W2_PG$graph)
#2.2. Centrality estimates: Wave 2, persistent gamblers
centralityTable(Network_W2_PG, standardized = TRUE)
centralityPlot(Network_W2_PG, scale = c("z-scores"), include = c("Betweenness", "Closeness", "Strength"))
#2.3. Network stability and accuracy analyses: Wave 2, persistent gamblers
Boot_W2_1_PG <- bootnet(Network_W2_PG, nCores = 8, nBoots = 1000, type = "nonparametric", statistics = c("edge", "strength", "closeness", "betweenness"))
Boot_W2_2_PG <- bootnet(Network_W2_PG, nCores = 8, nBoots = 1000, type = "case", statistics = c("strength", "closeness", "betweenness"))
plot(Boot_W2_1_PG, order = "sample")
plot(Boot_W2_1_PG, "strength", order = "sample")
plot(Boot_W2_1_PG, "closeness", order = "sample")
plot(Boot_W2_1_PG, "betweenness", order = "sample")
plot(Boot_W2_1_PG, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Boot_W2_2_PG, statistics = c("strength", "closeness", "betweenness"))
corStability(Boot_W2_2_PG)
#-------------------------------------------------------------------------------
#3. COMPARISON OF CROSS-SECTIONAL NETWORKS FROM WAVES 1 AND 2
#3.1. Joint figure of centrality estimates from Waves 1 and 2
centralityPlot(list("Wave 1" = Network_W1_PG, "Wave 2" = Network_W2_PG), include = c("Betweenness", "Closeness", "Strength"), scale = c("z-scores"))
#3.2. Matrix of edge weights from cross-sectional networks of Waves 1 and 2
EdgeWeights_Matrix <- matrix(data = 0, nrow = 36, ncol = 2)
colnames(EdgeWeights_Matrix) <- c("Wave 1", "Wave 2")
EdgeWeights_W1_PG <- Network_W1_PG$graph
EdgeWeights_Matrix [1:8, 1] <- EdgeWeights_W1_PG [2:9,1]
EdgeWeights_Matrix [9:15, 1] <- EdgeWeights_W1_PG [3:9,2]
EdgeWeights_Matrix [16:21, 1] <- EdgeWeights_W1_PG [4:9,3]
EdgeWeights_Matrix [22:26, 1] <- EdgeWeights_W1_PG [5:9,4]
EdgeWeights_Matrix [27:30, 1] <- EdgeWeights_W1_PG [6:9,5]
EdgeWeights_Matrix [31:33, 1] <- EdgeWeights_W1_PG [7:9,6]
EdgeWeights_Matrix [34:35, 1] <- EdgeWeights_W1_PG [8:9,7]
EdgeWeights_Matrix [36, 1] <- EdgeWeights_W1_PG [9,8]
EdgeWeights_W2_PG <- Network_W2_PG$graph 
EdgeWeights_Matrix [1:8, 2] <- EdgeWeights_W2_PG [2:9,1]
EdgeWeights_Matrix [9:15, 2] <- EdgeWeights_W2_PG [3:9,2]
EdgeWeights_Matrix [16:21, 2] <- EdgeWeights_W2_PG [4:9,3]
EdgeWeights_Matrix [22:26, 2] <- EdgeWeights_W2_PG [5:9,4]
EdgeWeights_Matrix [27:30, 2] <- EdgeWeights_W2_PG [6:9,5]
EdgeWeights_Matrix [31:33, 2] <- EdgeWeights_W2_PG [7:9,6]
EdgeWeights_Matrix [34:35, 2] <- EdgeWeights_W2_PG [8:9,7]
EdgeWeights_Matrix [36, 2] <- EdgeWeights_W2_PG [9,8]
#3.3. Matrix of centrality measures from cross-sectional networks of Waves 1 and 2
Centrality_Matrix <- matrix(data = 0, nrow = 9, ncol = 6)
colnames(Centrality_Matrix) <- c("Betweenness W1", 
                                 "Betweenness W2", 
                                 "Closeness W1", 
                                 "Closeness W2", 
                                 "Strength W1", 
                                 "Strength W2")
CentralityTable_W1_PG <- centralityTable(Network_W1_PG, standardized = TRUE)
CentralityTable_W2_PG <- centralityTable(Network_W2_PG, standardized = TRUE)
Centrality_Matrix [1:9, 1] <- CentralityTable_W1_PG [1:9, 5]
Centrality_Matrix [1:9, 2] <- CentralityTable_W2_PG [1:9, 5]
Centrality_Matrix [1:9, 3] <- CentralityTable_W1_PG [10:18, 5]
Centrality_Matrix [1:9, 4] <- CentralityTable_W2_PG [10:18, 5]
Centrality_Matrix [1:9, 5] <- CentralityTable_W1_PG [19:27, 5]
Centrality_Matrix [1:9, 6] <- CentralityTable_W2_PG [19:27, 5]
#3.4. Matrix of presence vs. absence of edges
EdgePresence_Matrix <- EdgeWeights_Matrix
as.data.frame(EdgePresence_Matrix)
EdgePresence_Matrix <- ifelse(EdgePresence_Matrix > 0.1, 1, 0)
summary(EdgePresence_Matrix)
#3.5. Phi correlations for presence vs. absence of edges across cross-sectional networks of Waves 1 and Waves 2
cor.test(EdgePresence_Matrix [1:36, 1], EdgePresence_Matrix [1:36, 2], method = "pearson") 
#3.6. Spearman correlations between edge weights from cross-sectional networks of Waves 1 and 2
cor.test(EdgeWeights_Matrix [1:36, 1], EdgeWeights_Matrix [1:36, 2], method = "spearman") 
#3.7. Spearman correlations between centrality measures from cross-sectional networks of Waves 1 and 2
cor.test(Centrality_Matrix [1:9, 1], Centrality_Matrix  [1:9, 2], method = "spearman") #BWN
cor.test(Centrality_Matrix [1:9, 3], Centrality_Matrix  [1:9, 4], method = "spearman") #CLN
cor.test(Centrality_Matrix [1:9, 5], Centrality_Matrix  [1:9, 6], method = "spearman") #STR
#3.8. Network invariance test, global strength invariance test, edge and node centrality invariance test
NetworkInvariance_W1PG_W2PG <- NCT(Network_W1_PG, Network_W2_PG, test.edges = TRUE, test.centrality = TRUE, centrality = c("closeness", "betweenness", "strength"), p.adjust.method = "holm")
print(NetworkInvariance_W1PG_W2PG) 
#-------------------------------------------------------------------------------
#4. LONGITDUINAL, CROSS-LAGGED NETWORK ANALYSIS
#4.1. Lasso regression analyses for cross-lagged network analysis 
Data_W1_W2 <- read_spss("PGSI Lasso.sav")
Data_W1_W2 <- Data_W1_W2[complete.cases(Data_W1_W2),]
df <- as.matrix(Data_W1_W2)
k <- 9
adjMat <- matrix(0, k, k)
CLPN.fun <- function(df) {
  for (i in 1:k){
    set.seed(100)
    lassoreg <- cv.glmnet(as.matrix(df[,1:k]), df[,(k+i)], 
                          family = "binomial", alpha = 1, standardize=TRUE, nfolds = 10)
    lambda <- lassoreg$lambda.1se 
    adjMat[1:k,i] <- coef(lassoreg, s = lambda, exact = FALSE)[2:(k+1)]
  }
  return(adjMat)
}
#4.2. Network estimation: Cross-lagged, longitudinal network
labels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
Network_CL <- estimateNetwork(df, fun = CLPN.fun, labels = labels, directed = T)
plot(Network_CL)
summary(Network_CL)
print(Network_CL$graph)
#4.3. Centrality estimates: Cross-lagged, longitudinal network
centralityTable(Network_CL, standardized = TRUE)
centralityPlot(Network_CL, scale = c("z-scores"), include = c("OutStrength", "InStrength", "betweenness", "closeness"))
#4.4. Network stability and accuracy analyses: Cross-lagged, longitudinal network
Boot_CL_1 <- bootnet(Network_CL, directed = T, nCores = 1, nBoots = 1000, type = "nonparametric", statistics = c("edge", "OutStrength", "InStrength"))
plot(Boot_CL_1, order = "sample")
plot(Boot_CL_1, "edge",  plot = "difference", order = "sample")
plot(Boot_CL_1, "outStrength", order = "sample")
plot(Boot_CL_1, "inStrength", order = "sample")
Boot_CL_2 <- bootnet(Network_CL, nCores = 1, nBoots = 1000, type = "case", statistics = c("outStrength", "inStrength"), directed = T)
plot(Boot_CL_2, statistics = c("outStrength", "inStrength"))
corStability(Boot_CL_2)
#-------------------------------------------------------------------------------
