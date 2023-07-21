#################################################################
#Code to accompany Rhemtulla, van Bork, & Cramer (in preparation) 
# "Cross-Lagged Network Models"
#################################################################

# install and load required package #############################
#install.packages("glmnet") #uncomment to install packages for the 1st time
#install.packages("qgraph")
#install.packages("lavaan")
library(glmnet)
library(qgraph)
library(lavaan)
#################################################################

# load and clean data ###########################################
# The example in the paper uses variables from the Iowa Youth and
# Familes Study, which can be accessed at 
# http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/26721
# To replicate the analyses, download and unzip the files under: 
#   "DS3:  Wave A 1989: Child Survey Data"
#   "DS9:  Wave B 1990: Child Survey Data"

#setwd("C:/Users/mijke/Box Sync/__STUDIES__/NetworkModels/CrossLaggedPanelNetworks/IowaYouthFamiliesProjectData")
setwd("C:/Users/...") #set working directory to location of data files
W1child <- read.table(file = "WaveAChild/26721-0003-Data.tsv", sep = "", header = TRUE)
W2child <- read.table(file = "WaveBChild/26721-0009-Data.tsv", sep = "", header = TRUE)

#combine waves 1 and 2 into a single data set
W1W2child<- merge(W1child, W2child, by="CASEID")

myData <- data.frame(W1W2child$CASEID) #set up empty data frame to hold model variables only

#nb original scale has 1 = strongly disagree; 5 = strongly agree 
#6-x transformation => 1 = strongly agree; 5 = strongly disagree

#school commitment variables at wave 1: 
myData$SC01.1 <- 6-W1W2child$at202012 # "I like School a lot" : Reverse
myData$SC02.1 <- W1W2child$at202013 #"School bores me"
myData$SC03.1 <- W1W2child$at202014 #"I don't do well at School"
myData$SC04.1 <- W1W2child$at202015 #"I don't feel like I really belong at School"
myData$SC05.1 <- W1W2child$at202016 #"Homework is a waste of time"
myData$SC06.1 <- 6-W1W2child$at202017 #"I try hard at School" : Reverse
myData$SC07.1 <- 6-W1W2child$at202018 #"I usually finish my homework" : Reverse

#self-esteem variables at wave 1: 
myData$SE01.1 <- 6-W1W2child$at105008 #"I feel that I'm a person of worth...": Reverse
myData$SE02.1 <- 6-W1W2child$at105009 #"I feel that I have a number of good qualities" : Reverse
myData$SE03.1 <- W1W2child$at105010 #"All in all, I am inclined to feel that I'm a failure"
myData$SE04.1 <- 6-W1W2child$at105011 #"I am able to do things as well as most other people": Reverse
myData$SE05.1 <- W1W2child$at105012 #"I feel that I do not have much to be proud of"
myData$SE06.1 <- 6-W1W2child$at105013 #"I take a positive attitude toward myself" : Reverse
myData$SE07.1 <- 6-W1W2child$at105014 #"On the whole, I am satisfied with myself" : Reverse
myData$SE08.1 <- W1W2child$at105015 #"I certainly feel useless at times"
myData$SE09.1 <- W1W2child$at105016 #"I wish I could have more respect for myself"
myData$SE10.1 <- W1W2child$at105017 #"At times I think I am no good at all"

#School commitment variables at wave 2: 
myData$SC01.2 <- 6-W1W2child$bt203012
myData$SC02.2 <- W1W2child$bt203013 
myData$SC03.2 <- W1W2child$bt203014 
myData$SC04.2 <- W1W2child$bt203015 
myData$SC05.2 <- W1W2child$bt203016 
myData$SC06.2 <- 6-W1W2child$bt203017 
myData$SC07.2 <- 6-W1W2child$bt203018 

#self-esteem variables at wave 2: 
myData$SE01.2 <- 6-W1W2child$bt105008
myData$SE02.2 <- 6-W1W2child$bt105009
myData$SE03.2 <- W1W2child$bt105010 
myData$SE04.2 <- 6-W1W2child$bt105011
myData$SE05.2 <- W1W2child$bt105012 
myData$SE06.2 <- 6-W1W2child$bt105013
myData$SE07.2 <- 6-W1W2child$bt105014
myData$SE08.2 <- W1W2child$bt105015 
myData$SE09.2 <- W1W2child$bt105016 
myData$SE10.2 <- W1W2child$bt105017 

#change missingness indicator from 9 (in original data) to 
# NA. Reverse-coded items have missing == (6-9 = -3) so change
# those, too: 
myData <- myData[,-1] #remove ID variable
myData[myData == 9] <- NA 
myData[myData == -3] <- NA 

#remove 3 cases with missing data (glmnet doesn't allow them)
myData <- myData[complete.cases(myData),]
nrow(myData) #421 cases remain

#################################################################

# collapse highly similar variables#############################
myData$SC0102.1 <- myData$SC01.1 + myData$SC02.1 
myData$SC0607.1 <- myData$SC06.1 + myData$SC07.1 
myData$SE0102.1 <- myData$SE01.1 +  myData$SE02.1 
myData$SE0810.1 <- myData$SE08.1 +  myData$SE10.1 

myData$SC0102.2 <- myData$SC01.2 + myData$SC02.2 
myData$SC0607.2 <- myData$SC06.2 + myData$SC07.2 
myData$SE0102.2 <- myData$SE01.2 +  myData$SE02.2 
myData$SE0810.2 <- myData$SE08.2 +  myData$SE10.2 

#remove the original items that are now combined
names(myData)
myData <- myData[,-c(1,2,6:9,15,17:19,23:26,32,34)]

#reorder variables to put all SC wave 1 variables together, etc.: 
myData <- myData[,c(1:3,19:20,4:9,21:22,10:12,23:24,13:18,25:26)]

#re-number variables: 
colnames(myData) <- c("SC01.1", "SC02.1", "SC03.1", "SC04.1", "SC05.1", 
                      "SE01.1", "SE02.1", "SE03.1", "SE04.1", "SE05.1", "SE06.1", "SE07.1", "SE08.1",
                      "SC01.2", "SC02.2", "SC03.2", "SC04.2", "SC05.2", 
                      "SE01.2", "SE02.2", "SE03.2", "SE04.2", "SE05.2", "SE06.2", "SE07.2", "SE08.2")

#text for legends (to keep track of what each variable is)
longlegend <- c("SC1: I don't do well at school", 
                "SC2: I don't feel like I really belong at school", 
                "SC3: Homework is a waste of time", 
                "SC4: In general, I like school a lot (R) + School bores me", 
                "SC5: I try hard at school (R) + I usually finish my homework (R)", 
                "SE1: All in all, I am inclined to feel that I'm a failure",
                "SE2: I am able to do things as well as most other people (R)",
                "SE3: I feel I do not have much to be proud of",
                "SE4: I take a positive attitude toward myself (R)",
                "SE5: On the whole, I am satisfied with myself (R)",
                "SE6: I wish I could have more respect for myself.",
                "SE7: I feel that I'm a person of worth, at least on an equal level with others (R) + I feel that I have a number of good qualities (R)",
                "SE8: I certainly feel useless at times + At times I think I am no good at all")

brieflegend <- c("SC1: Success at school", 
                 "SC2: Belonging at school", 
                 "SC3: Usefulness of homework", 
                 "SC4: Enjoyment of school", 
                 "SC5: Effort at school", 
                 "SE1: Feel like a failure",
                 "SE2: Able to do things",
                 "SE3: Little to be proud of",
                 "SE4: Positive attitude to self",
                 "SE5: Satisfied with myself",
                 "SE6: Respect for myself.",
                 "SE7: Person of worth",
                 "SE8: Feel useless at times")
#################################################################

# save data and legends (not necessary)###########################
save(myData, longlegend, brieflegend, file = "myData.Rda")
#load(file = "myData.Rda") #to load the data and legends without running all of the above code
#################################################################

# estimate network paths##########################################
library(glmnet) #load glmnet package if you haven't already

k <- 13 #k is the number of variables at each time point
adjMat <- matrix(0, k, k) #set up empty matrix of coefficients
rsquarelist <- rep(0, k)
#the following code loops the regularized regression through each
#variable at time 2 as a DV, regressing on each variable at time 1
#as predictors. It assumes that the first k columns in your data
#correspond to the variables at time 1 and the next k correspond
#to the variables at time 2. These variables must be in the same 
#order at each time point.
#please see the paper for technical details

for (i in 1:k){
  set.seed(100)
  lassoreg <- cv.glmnet(as.matrix(myData[,1:k]), myData[,(k+i)], 
                        family = "gaussian", alpha = 1, standardize=TRUE)
  lambda <- lassoreg$lambda.min 
#  rsquare[i] <- lassoreg$glmnet.fit$dev.ratio[which(lassoreg$lambda == lambda)]
  adjMat[1:k,i] <- coef(lassoreg, s = lambda, exact = FALSE)[2:(k+1)]
}
#################################################################

# plot the CLPN ##################################################

#create a 'group' vector that says which variables belong to which construct:
groups <- c(rep("Commitment to School", 5), rep("Self-Esteem", 8)) 

#create a vector of variable labels for the figure
labels <- c("SC1","SC2","SC3","SC4","SC5",
            "SE1","SE2","SE3","SE4","SE5","SE6","SE7", "SE8")

#jpeg("CLPN_ARCLpaths.jpg", width=8, height=4, res=800, units="in")  #(uncomment this code to save plot as jpeg)
layout(mat = matrix(c(1, 2), 1, 2), widths = c(1.5, 1)) #to plot multiple graphs in one figure
qgraph(adjMat, groups = groups, labels = labels, legend = FALSE,
       colors = c("#E69F00", "#56B4E9"))
plot.new()
par(mar = rep(0, 4))
legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
       col = c(rep("#E69F00",5),rep("#56B4E9",8)), y.intersp = 2,
       pch = 19) #add legend to identify variables
#dev.off() #(uncomment this code to save plot as jpeg)

# curved arrows on each variable are the AR paths
# we can remove those to see the CL paths more clearly: 

adjMat2 <- adjMat
diag(adjMat2) <- 0 #set the AR paths to 0 for plotting purposes: 

#jpeg("CLPN_CLpaths.jpg", width=8, height=4, res=800, units="in")  #(uncomment this code to save plot as jpeg)
layout(mat = matrix(c(1, 2), 1, 2), widths = c(1.5, 1)) #to plot multiple graphs in one figure
qgraph(adjMat2, groups = groups, labels = labels, legend = FALSE,
       colors = c("#E69F00", "#56B4E9"))
plot.new()
par(mar = rep(0, 4))
legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
       col = c(rep("#E69F00",5),rep("#56B4E9",8)), y.intersp = 2,
       pch = 19) #add legend to identify variables
#dev.off() #(uncomment this code to save plot as jpeg)

#################################################################

# compute and plot predictability and influence #################
#nodewise predictability = r-squared based on regressions of 
  #each variable on (1) all others at previous timepoint (CL paths only)
  #and (2) based on only other construct(s) at previous timepoint 
library(lavaan) #in case you didn't load it earlier

pred0 <- rep(0, k) #set up empty vectors to hold predictability coefficients
pred1 <- rep(0, k) 
pred2 <- rep(0, k)

for (i in 1:k){  #loop through each variable
  name.i <- names(myData)[k+1] #what is the name of the DV
  group.i <- groups[i] #what construct does the DV belong to
  include.mod0 <- names(myData)[1:k] #include self at previous time point
  include.mod1 <- names(myData)[1:k][-i] #exclude self at previous time point
  include.mod2 <- names(myData)[1:k][groups != group.i] #include predictors belonging to same construct
  betas.mod0 <- adjMat[1:k,i]     #fix regression coefficients to the ones obtained by glmnet
  betas.mod1 <- adjMat[1:k,i][-i] 
  betas.mod2 <- adjMat[1:k,i][groups != group.i]
  mod0 <- paste(name.i, " ~ ", paste(paste(betas.mod0,'*', include.mod0, sep = ""), collapse = "+")) 
  mod1 <- paste(name.i, " ~ ", paste(paste(betas.mod1,'*', include.mod1, sep = ""), collapse = "+")) 
  mod2 <- paste(name.i, " ~ ", paste(paste(betas.mod2,'*', include.mod2, sep = ""), collapse = "+")) 
  fit0 <- sem(mod0, sample.cov = cor(myData), sample.nobs = nrow(myData)) #fit model with subset of predictors to get r-square
  pred0[i] <- inspect(fit0, "rsquare")
  fit1 <- sem(mod1, sample.cov = cor(myData), sample.nobs = nrow(myData)) #fit model with subset of predictors to get r-square
  pred1[i] <- inspect(fit1, "rsquare")
  fit2 <- sem(mod2, sample.cov = cor(myData), sample.nobs = nrow(myData)) #fit model with subset of predictors to get r-square
  pred2[i] <- inspect(fit2, "rsquare")
}  

pred0
pred1
pred2

#nodewise influence: sum squared outgoing regression paths/node
infl0 <- apply(adjMat^2, 1, sum) #all outgoing nodes including AR
infl1 <- apply(adjMat2^2, 1, sum) #all outgoing nodes excluding AR

adjMat3 <- adjMat2 #adjMat3 sets paths within the same construct to 0
adjMat3[groups[row(adjMat3)] == groups[col(adjMat3)]] <- 0
infl2 <- apply(adjMat3^2, 1, sum) #rows = T1 predictors; columns = T2 DVs

infl0
infl1
infl2



#create a plot of predictability and influence: 
#jpeg("PredictabilityInfluencePlot.jpg", width=8, height=6, res=800, units="in")  #(uncomment this code to save plot as jpeg)
layout(mat = matrix(c(1, 2, 3, 3), 2, 2), widths = c(1, 1)) #to plot multiple graphs in one figure
par(mar = c(2, 4, 0, 0), las = 1) #set margins
barplot(rev(pred1), horiz = TRUE, names.arg = rev(labels),
        col = rainbow(k), ylab = "predictability")
barplot(rev(infl1), horiz = TRUE, names.arg = rev(labels), 
        col = rainbow(k), ylab = "influence")
plot.new()
legend(x = "center", inset = c(0, 0), bty = "n", fill=rev(rainbow(k)),
       legend = brieflegend, cex = 1.2) #add legend to identify variables
#dev.off() #(uncomment this code to save plot as jpeg)


#jpeg("PredInflPlot_CrossConstruct.jpg", width=8, height=6, res=800, units="in")  #(uncomment this code to save plot as jpeg)
layout(mat = matrix(c(1, 2, 3, 3), 2, 2), widths = c(1, 1)) #to plot multiple graphs in one figure
par(mar = c(2, 4, 0, 0), las = 1) #set margins
barplot(rev(pred2), horiz = TRUE, names.arg = rev(labels),
        col = rainbow(k), ylab = "predictability")
barplot(rev(infl2), horiz = TRUE, names.arg = rev(labels), 
        col = rainbow(k), ylab = "influence")
plot.new()
legend(x = "center", inset = c(0, 0), bty = "n", fill=rev(rainbow(k)),
       legend = brieflegend, cex = 1.2) #add legend to identify variables
#dev.off() #(uncomment this code to save plot as jpeg)
#################################################################


# estimate CLPM with latent variables ###########################
library(lavaan)

CLPM.mod <- '
          SC.1 =~ SC01.1 + SC02.1 + SC03.1 + SC04.1 + SC05.1 
          SE.1 =~ SE01.1 + SE02.1 + SE03.1 + SE04.1 + SE05.1 + SE06.1 + SE07.1 + SE08.1 
          SC.2 =~ SC01.2 + SC02.2 + SC03.2 + SC04.2 + SC05.2 
          SE.2 =~ SE01.2 + SE02.2 + SE03.2 + SE04.2 + SE05.2 + SE06.2 + SE07.2 + SE08.2
          SC01.1 ~~ SC01.2  #let residuals of indicators covary across time
          SC02.1 ~~ SC02.2
          SC03.1 ~~ SC03.2
          SC04.1 ~~ SC04.2 
          SC05.1 ~~ SC05.2 
          SE01.1 ~~ SE01.2 
          SE02.1 ~~ SE02.2 
          SE03.1 ~~ SE03.2
          SE04.1 ~~ SE04.2
          SE05.1 ~~ SE05.2
          SE06.1 ~~ SE06.2
          SE07.1 ~~ SE07.2
          SE08.1 ~~ SE08.2 
          SC.2 ~ SC.1 + SE.1 #latent variable regressions of T2 on T1
          SE.2 ~ SC.1 + SE.1
          '

CLPM.fit <- sem(CLPM.mod, data = myData, estimator = "MLM", std.lv = TRUE)
summary(CLPM.fit, standardized = TRUE,fit = TRUE)
#################################################################
