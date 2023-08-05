##################################################################################
#############  Within and across-day dynamics of depressive symptoms  ############
#############         and related psychopathological processes:       ############
############# A dynamic network approach during the COVID-19 pandemic ############ 
#############                                                         ############
#############             Omid V. Ebrahimi, Julian Burger             ############ 
#############           Asle Hoffart & Sverre Urnes Johnson           ############
#############                                                         ############
#############                   Code and analyses by:                 ############
#############             Omid V. Ebrahimi & Julian Burger            ############
##################################################################################


##################################################################################
############################ Data preparation end ################################
##################################################################################

### Set working directory
setwd("C:/Users/OmidEbrahimi/Dynamic Network Model of Depressive Symptomatology/R")

### Load cleaned data.
Data <- read.csv("Data - Tidy and Ready.csv")


##################################################################################
###################### Multi-level vector autoregressive models ################## 
######################       Assumptions and preparation        ##################
######################                  Start                   ##################
##################################################################################

library(ggplot2)

pdf("Cumulative length of time-series per person.pdf", height = 6, width = 9)

### Plot
ggplot(counts) + geom_point(aes(x=counts[,1], y=counts[,3]))
               + labs(title = "Cumulative length of time-series
                               per person across the study participants",
                      x = "Cumulative length of time-series",
                      y = "Number of participants")
               + coord_cartesian(ylim=c(0, 2000))
               + geom_vline(xintercept = 30, linetype = 2, color = "black")
               + theme_bw() + theme(plot.title = element_text(size=12))

dev.off()

### Topological overlap among items
library("networktools")
library("dplyr")
inspectTopologicalOverlap <-  Data %>% 
            dplyr::select(lonely, phq1, phq2, phq4, phq6, tpq22,
                          bis6, r_wrq, interconf, swemwbs6, 
                          emoreg, swemwbs2, suffinfoaccess, 
                          some, ntimeofflinesocial,
                          ntimedigitalsocial, physact)

### Check if matrix is positive definite
correlationMatrix <- cor(x = inspectTopologicalOverlap,
                         use = c("pairwise.complete.obs"))

is.positive.definite(correlationMatrix)


## Use goldbricker function to search 
inspectTO <- goldbricker(inspectTopologicalOverlap, p = 0.05,
                         method = "hittner2003", threshold=0.25,
                         corMin=.50)

inspectTO
# ----> [1] "No suggested reductions" 

### Packages
library("mlVAR")
library("dplyr")
library("qgraph")

### Investigate number of cumulative time-series provided per participant
Data$na <- !is.na(Data$lonely)
agg <- aggregate(na ~ ID, data = Data, sum)
hist(agg$na)
counts <- matrix(NA, 41, 3)
colnames(counts) <- c("ts.length", "count", "cumsum")

for(i in 1:41){
  counts[i,1] <- 40-i+1
  counts[i,2] <- length(which(agg$na==40-i+1))
}

counts[,3] <- cumsum(counts[,2])

layout(1)
plot(counts[,1], counts[,3], type = "p",   
     ylab = "cumulative observations",
     xlab = "length of time-series",
     ylim = c(0,2000), lwd = 2)
abline(v = 30, col = "red", lwd = 2, lty = 2)

counts

### Select participants with 30 valid entries
sel <- 30 
sel.ID <- agg[which(agg$na >= sel),][,1]
Data <- Data[Data$ID %in% sel.ID,]

summary(Data)

### Check for linear and weekend trends

## Variables to investigate
vars <- colnames(Data)[1:34] 

week.code <- c(rep(0, 5), rep(1, 2))
week.whole <- rep(week.code, 6) 
week <- week.whole[-c(1,2)]

Data$week <- rep(week, length(unique(Data$ID))) 

## Create new datafile that will contain the detrended values
Data_detrended <- Data 

fixed_week <- numeric()
fixed_day <- numeric()

### Loop
for(i in 1:length(vars)){
  formula <- as.formula(paste0(vars[i], " ~ week + Day"))
    lmRes <- lm(formula, data = Data) 
    Data_detrended[,i] <- Data[,vars[i]]
                        - (lmRes$coefficients[1]
                        + lmRes$coefficients[2]
                        * Data[["week"]]
                        + lmRes$coefficients[3]
                        * Data[["Day"]])

  fixed_week[i] <- lmRes$coefficients[2]
  fixed_day[i] <- lmRes$coefficients[3]
}


##################################################################################
###################### Multi-level vector autoregressive models ##################
######################       Assumptions and preparation        ##################
######################                  End                     ##################
##################################################################################

### Estimate mlVAR model

## Node description
namesObject <- c("Lonely",
                 "Anhedonia", "Depressed", "Lethargy (Energyless)", "Worthlessness",
                 "Sleep Satisfaction", "Productivity", "In-person Social Contact",
                 "Digital Social Contact", "Social Media", "Sufficient Informatinon",
                 "Relatedness", 
                 "Rumination", "Helplessness", "Emotion Regulation Difficulties",
                 "Interpersonal Conflict", "Physical Activity")

## Node labels
labelsObject <- c("Loneliness",
                  "Anhedonia", "Depressed", "Lethargy", "Worthless",
                  "SleepSatis", "Productive", "InPSocCon", "DigSocCon", "SocMedia",
                  "SufficInfo", "Relatedness","Rumination", "Helpless", 
                  "EmoRegDiff", "InterpConf", "PhysicalAct")


## Grouping object for visualization
groupObject <- c(rep("Psychological states", 1), 
                 rep("Depressive symptoms", 4),
                 rep("Psychological states", 2),
                 rep("Situational and contextual variables", 4),
                 rep("Psychological states", 1),
                 rep("Psychpathological processes", 3),
                 rep("Situational and contextual variables", 1),
                 rep("Physical Activity", 1))


### Rename columns
names(Data_detrended)[names(Data_detrended) 
                      == "lonely"] <- "Lonely"
names(Data_detrended)[names(Data_detrended) 
                      == "phq1"] <- "Anhedonia"
names(Data_detrended)[names(Data_detrended) 
                      == "phq2"] <- "Depressed"
names(Data_detrended)[names(Data_detrended) 
                      == "phq4"] <- "Lethargy"
names(Data_detrended)[names(Data_detrended) 
                      == "tpq22"] <- "Helpless"
names(Data_detrended)[names(Data_detrended) 
                      == "bis6"] <- "SleepSatisfaction"
names(Data_detrended)[names(Data_detrended) 
                      == "r_wrq"] <- "Rumination"
names(Data_detrended)[names(Data_detrended) 
                      == "interconf"] <- "InterpersonalConflict"
names(Data_detrended)[names(Data_detrended) 
                      == "swemwbs6"] <- "Relatedness"
names(Data_detrended)[names(Data_detrended) 
                      == "emoreg"] <- "EmotionRegulationDiff"
names(Data_detrended)[names(Data_detrended) 
                      == "swemwbs2"] <- "Productivity"
names(Data_detrended)[names(Data_detrended) 
                      == "suffinfoaccess"] <- "SufficientInfo"
names(Data_detrended)[names(Data_detrended) 
                      == "some"] <- "SocialMedia"
names(Data_detrended)[names(Data_detrended) 
                      == "ntimeofflinesocial"] <- "OfflineSocialContact"
names(Data_detrended)[names(Data_detrended) 
                      == "ntimedigitalsocial"] <- "DigitalSocialContact"
names(Data_detrended)[names(Data_detrended) 
                      == "physact"] <- "PhysicalActivity"


### Multi-level vector autoregressive network models
start.time <- Sys.time() 
model.orthogonal <- mlVAR(Data_detrended,
                          vars = names,
                          idvar = "ID",
                          temporal = "orthogonal",
                          contemporaneous = "orthogonal")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Set up to retrieve average layout 
graph.temp <- plot(model.orthogonal, 
                   "temporal", theme = "colorblind",
                   vsize = 7, labels = 1:length(names))
graph.cont <- plot(model.orthogonal,
                   "contemporaneous", theme = "colorblind",
                   vsize = 7, labels = 1:length(names))
graph.betw <- plot(model.orthogonal, "between", 
                   theme = "colorblind", vsize = 7, labels = 1:length(names))
layout <- averageLayout(graph.temp, graph.cont, graph.betw)


### Plot the networks
pdf("networks.pdf", height = 20, width = 60)
par(mfrow=c(1,4))

pdf("Temporal network.pdf", height = 10, width = 16)

plot(model.orthogonal, 
     "temporal",
     maximum = 0.4, 
     minimum = 0.04, 
     cut = 0.05,
     nodeNames = namesObject,
     groups = groupObject,
     labels = labelsObject,
     theme = "colorblind", 
     layout = layout, 
     label.scale.equal = TRUE,
     label.cex = 1.1, 
     legend.cex = 0.5,
     width = 12,
     height = 8,
     GLratio = 2,
     vsize = 7.1,
     curveAll = TRUE,
     mar = rep(4,4))

title(main = "Temporal network", 
      sub = "Max = 0.4, Min = 0.04, Cut = 0.05",
      cex.main = 1.5,
      cex.sub = 1.2)

dev.off()

pdf("Contemporaneous network.pdf", height = 10, width = 16)

plot(model.orthogonal, 
     "contemporaneous", 
     maximum = 0.4, 
     minimum = 0.04, 
     nodeNames = namesObject,
     groups = groupObject,
     labels = labelsObject,
     theme = "colorblind", 
     layout = layout, 
     label.scale.equal = TRUE,
     label.cex = 1.1, 
     legend.cex = 0.5,
     width = 12,
     height = 8,
     GLratio = 2,
     vsize = 7.1, 
     mar = rep(4,4))

title(main = "Contemporaneous network", 
      sub = "Max = 0.4, Min = 0.04",
      cex.main = 1.5,
      cex.sub = 1.2)

dev.off()

pdf("Between-subjects network.pdf", height = 10, width = 16)
plot(model.orthogonal, 
     "between", 
     maximum = 0.4, 
     minimum = 0.04, 
     nodeNames = namesObject,
     groups = groupObject,
     labels = labelsObject,
     theme = "colorblind", 
     layout = layout, 
     label.scale.equal = TRUE,
     label.cex = 1.1, 
     legend.cex = 0.5,
     width = 12,
     height = 8,
     GLratio = 2,
     vsize = 7.1,
     mar = rep(4,4))

title(main = "Between-subjects network", 
      sub = "Max = 0.4, Min = 0.04",
      cex.main = 1.5,
      cex.sub = 1.2)

dev.off()
dev.off()

### Matrices
## Temporal
View(model.orthogonal$results$Beta$mean[,,1])
write.csv(model.orthogonal$results$Beta$mean[,,1], "1) Temporal effects.csv")

## Contemporaneous
View(model.orthogonal$results$Theta[["pcor"]][["mean"]])
write.csv(model.orthogonal$results$Theta[["pcor"]][["mean"]], "1) Contemporaneous effects.csv")

## Between-subjects network
View(model.orthogonal$results[["Omega_mu"]][["pcor"]][["mean"]])
write.csv(model.orthogonal$results[["Omega_mu"]][["pcor"]][["mean"]], "1) Between-subject effects.csv")


##################################################################################
######################        Radar plots for networks      ######################
######################  A brief guide on how to visualize   ######################
######################                Start                 ######################
##################################################################################


### Radar plot for contemporaneous network:
##  Here are some instructions for those who find this useful
##  and may want to check it out in the future.
##  Step by step guide provided below. 
##  Good luck with your project!
##  Best wishes

## Step 1: Extract your strength estimates
#  (As this is the contemporaenous plot with undirected effects and symmetrical weights matrix, you can take out either OutDegree or InDegree. 
#  They both represent strength in this particular case. 
contStrength <- centrality(graph.cont)$OutDegree

## Step 2: Load package for radar chart
library("fmsb")

## Step 3: Consider using unstandardized centrality estimates
#  in accordance with best practice guidelines as detailed by 
#  the Amsterdam PsychoSystems team. See here: 
#  https://psyarxiv.com/4y9nz/)
#  I have followed these guidelines. So I will use unstandardized centrality estimates.

## Step 4: 
#  Create a dataframe with the betweenStrength vector converted to a data.frame 
#  with 17 column (i.e., number of nodes in your network and 1 row).

dataRadarChartCont <- as.data.frame(matrix(contStrength, ncol = 17, nrow = 1))

## Step 5: Provide the node names for the df just made above.
colnames(dataRadarChartCont) <- c("Lonely", "Anhedonia", "Depressed", "Lethargy",
                                  "Worthlessness","Sleep Satisfaction",
                                  "Productivity", "In-person Social Contact",
                                  "Digital Social Contact", "Social Media",
                                  "Sufficient Information", "Relatedness", 
                                  "Rumination", "Helpless",
                                  "Emotion Regulation Difficulties",
                                  "Interpersonal Conflict", "PhysicalActivity")

## Step 6: Identify the maximum value for among your centrality estimates (i.e., the highest strength value across all nodes)
#  (look at your estimates that you just extracted.
#  (Mine is in object contStrength in the above).
#  Provide the max value (my maximum is ~ 1.23, rounded up to 1.25 (This will be the maximum
#  outer limit of my radar plot)).
#  and the minimum value (should always be 0),
#  and repeat this value in the dataframe for the total number of
#  strength coefficients you have (one for each node: 17):

dataRadarChartCont <- rbind(rep(1.25, 17) , rep(0, 17), dataRadarChartCont)

# Save as PDF
pdf("Strength radar plot - Contemporaneous network.pdf", height = 10, width = 16)

## Step 7: Customized radar chart
#  The default doesn't look too good. More importantly, 
#  you must correctly provide input values
#  based on your calculated model coefficients. 
#  Here is how and some tips for customization:

# Seg = sets the number of segments (the most inner segment/polygons is the
# origin, so it does not count). This means you have now set 5 additional circles
# (6 inner circles in total if you count the inner polygons depicting 0)

radarchart(dataRadarChartCont, seg = 5, axistype = 1, 
           
           # Customize polygon
           pcol = rgb(33, 118, 192, max = 255) , plwd=2,

           # Customize the grid. The three arguments below are
           # just non-important customizations: linetype, color of properties of 
           # the plot (e.g., color of the net (grey here)). 
           cglcol = "grey", cglty = 1, axislabcol = "black",
           
           # ** This one is important and not arbitrary. We know our
           #    maximum value was ~ 1.25. This will now serve as our outer polygon,
           #    minimum value is 0 (we start from zero), and ** since we have 5 
           #    circles (not counting the origin), we want to adjust the polygons to 
           #    increase by units of 0.25:
           #    0.25 (unit increase) * 5 (additional) = 1.25 (Maximum value)
           
           caxislabels = seq(0,1.25,0.25), # 0.25 * 5 = 1.25 
                                           # In sum: the number of inner circles
                                           # (excluding the first)
                                           # must be multiplied by a constant
                                           # increasing at a fixed rate as you move
                                           # up the circles. For our study, this is:
                                           # 0.25 (the constant; think of it as
                                           # your y-axis). 0.25 * 5 = 1.25
           
           # This sets the width of a net
           cglwd = 0.8,
           
           # Font size of labels
           vlcex=0.8)

# Title
title(main = "Strength Centrality (Contemporaneous network)", 
      cex.main = 1.5,
      cex.sub = 1.2)

# Close PDF device
dev.off()

### Step 8: Double check and compare with centrality plot:
centralityPlot(graph.cont, scale = "raw",
               include = c("Strength")) # Looks good.


## Between-subjects network: Extract - strength weights
betweenStrength <- centrality(graph.betw)$OutDegree

dataRadarChartBetw <- as.data.frame(matrix(betweenStrength, ncol = 17, nrow = 1))
colnames(dataRadarChartBetw) <- c("Lonely", "Anhedonia", "Depressed", "Lethargy",
                                  "Worthlessness","Sleep Satisfaction",
                                  "Productivity", "In-person Social Contact",
                                  "Digital Social Contact", "Social Media",
                                  "Sufficient Information", "Relatedness", 
                                  "Rumination", "Helpless",
                                  "Emotion Regulation Difficulties",
                                  "Interpersonal Conflict", "PhysicalActivity")

dataRadarChartBetw <- rbind(rep(1.75, 17) , rep(0, 17) , dataRadarChartBetw)

## Save as PDF
pdf("Strength radar plot - Between-subjects network.pdf", height = 10, width = 16)

radarchart(dataRadarChartBetw, seg = 7, axistype = 1, 
           
           pcol = rgb(33, 118, 192, max = 255) , plwd=2,
           
           cglcol="grey", cglty = 1, axislabcol = "black",
           caxislabels = seq(0,1.75,0.25),
           
           cglwd = 0.8,
           
           vlcex=0.8)


# Title
title(main = "Strength Centrality (Between-subject network)", 
      cex.main = 1.5,
      cex.sub = 1.2)

dev.off()

### OutStrength:
outStrength <- centrality(graph.temp)$OutDegree
dataRadarChartOS <- as.data.frame(matrix(outStrength, ncol=17, nrow = 1))
colnames(dataRadarChartOS) <- c("Lonely", "Anhedonia", "Depressed", "Lethargy",
                                "Worthlessness","Sleep Satisfaction",
                                "Productivity", "In-person Social Contact",
                                "Digital Social Contact", "Social Media",
                                "Sufficient Information", "Relatedness", 
                                "Rumination", "Helpless",
                                "Emotion Regulation Difficulties",
                                "Interpersonal Conflict", "PhysicalActivity")

dataRadarChartOS <- rbind(rep(0.30, 17) , rep(0, 17) , dataRadarChartOS)

# Save as PDF
pdf("Outstrength radar plot.pdf", height = 10, width = 16)

radarchart(dataRadarChartOS, seg = 6, axistype = 1, 
           
           # customize polygon
           pcol = rgb(33, 118, 192, max = 255) , plwd=2,
           
           cglcol="grey", cglty = 1, axislabcol = "black",
           caxislabels = seq(0,0.30,0.05),
           cglwd = 0.8,
           
           vlcex=0.8)


title(main = "OutStrength Centrality", 
      cex.main = 1.5,
      cex.sub = 1.2)

dev.off()


### InStrength:
inStrength <- centrality(graph.temp)$InDegree

dataRadarChartIS <- as.data.frame(matrix(inStrength, ncol=17, nrow = 1))
colnames(dataRadarChartIS) <- c("Lonely", "Anhedonia", "Depressed", "Lethargy",
                                "Worthlessness","Sleep Satisfaction",
                                "Productivity", "In-person Social Contact",
                                "Digital Social Contact", "Social Media",
                                "Sufficient Information", "Relatedness", 
                                "Rumination", "Helpless",
                                "Emotion Regulation Difficulties",
                                "Interpersonal Conflict", "PhysicalActivity")

dataRadarChartIS <- rbind(rep(0.30, 17) , rep(0, 17) , dataRadarChartIS)

# Save as PDF
pdf("InStrength radar plot.pdf", height = 10, width = 16)

# Customized radar chart
radarchart(dataRadarChartIS, seg = 6, axistype = 1, 
           
           # customize polygon
           pcol = rgb(33, 118, 192, max = 255) , plwd=2,
           
           cglcol="grey", cglty = 1, axislabcol = "black",
           caxislabels = seq(0,0.30,0.05), 
           cglwd = 0.8,
           
           vlcex=0.8)

title(main = "InStrength Centrality", 
      cex.main = 1.5,
      cex.sub = 1.2)


dev.off()

##################################################################################
######################              Radar plots             ######################
######################   A brief guide on how to visualize  ######################
######################                  End                 ######################
##################################################################################


##################################################################################
######################### Time-series analyses: Start ############################
##################################################################################

### Load cleaned Time-series Data.
DataTSMain <- read.csv("Data Prepared for Time-series - Tidy and Ready.csv")

### Load necessary packages.
library("readxl")
library("dplyr")
library("plyr") 
library("ggplot2")
library("reshape")
library("viridis") 

### Time-series visualizations
##  Remove redundant variables and select time-variant variables.

Data2 <- DataTSMain %>% 
  dplyr::select(ID, Day, lonely, phq1, phq2, phq4, phq6, 
                tpq22, bis6, r_wrq, interconf, swemwbs6, 
                emoreg, swemwbs2, suffinfoaccess, some, 
                ntimeofflinesocial, ntimedigitalsocial, physact)

### Reformat data
DataTS <- melt(Data2, id=c("ID", "Day"))

### Rename column 3 (variable) to "Variable".
colnames(DataTS)[3] <- "Variable"

### Convert 'Variable' into string/character.
DataTS[,3] <- as.character(DataTS[,3])  

### Rename variables.
DataTS[DataTS=="lonely"]  <- "Lonely" 
DataTS[DataTS=="phq1"]  <- "Anhedonia"
DataTS[DataTS=="phq2"]  <- "Depressed Mood"
DataTS[DataTS=="phq4"]  <- "Lethargy"
DataTS[DataTS=="phq6"]  <- "Worthlessness"
DataTS[DataTS=="tpq22"]  <- "Helpless"
DataTS[DataTS=="bis6"]  <- "Sleep satisfaction"
DataTS[DataTS=="r_wrq"]  <- "Rumination"
DataTS[DataTS=="interconf"]  <- "Interpersonal conflict"
DataTS[DataTS=="swemwbs6"]  <- "Relatedness"
DataTS[DataTS=="emoreg"]  <- "Emotion reg. difficulties"
DataTS[DataTS=="swemwbs2"]  <- "Productivity"
DataTS[DataTS=="suffinfoaccess"]  <- "Sufficient information"
DataTS[DataTS=="some"]  <- "Social Media use"
DataTS[DataTS=="ntimeofflinesocial"]  <- "In-person social contact"
DataTS[DataTS=="ntimedigitalsocial"]  <- "Digital social contact"
DataTS[DataTS=="physact"]  <- "Physical activity"

## Inspect:
View(unique(DataTS$Variable)) # Perfect.

### Get the mean across participants for each variable per day across
##  the 40 day measurement period.
DataTS2 <- aggregate(DataTS, by = list(DataTS$Day, DataTS$Variable), FUN=mean,na.rm=T)

### Delete irrelevant variables and 
##  redudant columns with duplicate info.
DataTS2 <- DataTS2 [, -c(3, 4, 5)]

### Rename to correct labels.
colnames(DataTS2) <- c("Day", "Variable", "Mean")

### Create path to store figures.
FiguresPath <- "C:/Users/OmidEbrahimi
                /Dynamic Network Model of Depressive Symptomatology/R/Figures/"

### Code credit:
##  We wish to thank Eiko Fried, Faidra Papanikolaou and Sacha Epskamp 
##  for granting us permission to re-use and adapt their code concerning
##  time-series visualizations.
##  Check out their excellent paper here:
##  https://journals.sagepub.com/doi/10.1177/21677026211017839

### Time-series figures

### Depressive symptoms and loneliness
##  Starts pdf object 
pdf(paste0(FiguresPath, 
           "1) Time-series - Part 1.pdf"),
            width = 8.125, height = 5) 

## Plot
ggplot(filter(DataTS2 %>% 
                filter(Variable == "Anhedonia" |
                         Variable == "Depressed Mood" |
                         Variable == "Lethargy" |
                         Variable == "Worthlessness" |
                         Variable == "Lonely")),
                aes(Day, Mean, colour=Variable)) +
                xlab("Days") +
                geom_smooth(method = "loess", span = 0.1, 
                            se = FALSE, size = .6) +
                theme_light() +
                theme(panel.grid.minor.y = element_blank(),
                      panel.grid.minor.x = element_blank(), 
                      panel.grid.major.x = element_blank()) +
                scale_x_continuous(
                  breaks = c(2, 4, 6, 8, 10, 12, 14, 
                             16, 18, 20, 22, 24, 26, 
                             28, 30, 32, 34, 36, 38, 40)) +
                geom_vline(aes(xintercept = 6), 
                           color='lightgrey', alpha = 1, 
                           linetype = 5, size = .5) + 
                annotate("text", x = 6, y=1.4, hjust = 0,
                         size = 1.7, color='#444444',
                         label = " Winter vacation for East \n and Northern Norway") +
                geom_vline(aes(xintercept = 12), 
                           color='lightgrey', alpha = 1, 
                           linetype = 5, size = .5) +
                geom_vline(aes(xintercept = 13), 
                           color = 'darkgrey', alpha = 1, 
                           linetype = 10, size = .5) + 
                annotate("text", x = 13, y = 1.4, hjust = 0,
                         size = 1.7, color = '#444444',
                         label = "  Winter vacation for \n  West and Mid-Norway ") +
                geom_vline(aes(xintercept = 19), 
                           color = 'darkgrey', alpha = 1, 
                           linetype = 10, size = .5) + 
                geom_vline(aes(xintercept = 20), 
                           color='#777777', alpha = 1, 
                           linetype = 1, size = .5) + 
                annotate("text", x = 20, y=1.4, hjust = 0,
                         size = 1.7, color='#444444',
                         label = " Uninterrupted period with viral mitigation protocols") + 
                geom_vline(aes(xintercept = 38), 
                           color = '#777777', alpha = 1, 
                           linetype = 1, size = .5) + 
                annotate("text", x = 38, y = 1.4, hjust = 0,
                         size = 1.7, color = '#444444',
                         label = " Easter holidays \n start date \n in Norway")


dev.off()
dev.off()
dev.off()


#### Mechanistic variables part 1 / contextual variables
pdf(paste0(FiguresPath, 
           "2) Time-series - Part 2.pdf"),
           width = 8.125, height = 5) 

## Plot
ggplot(filter(DataTS2 %>% 
                filter(Variable == "Sleep satisfaction" |
                         Variable == "Productivity" |
                         Variable == "Sufficient information" |
                         Variable == "Social Media use" |
                         Variable == "In-person social contact" | 
                         Variable == "Digital social contact")),
                aes(Day, Mean, colour = Variable, linetype = Variable)) +
                xlab("Days") +
                geom_smooth(method = "loess", span = 0.1, 
                            se = FALSE, size=.8) +
                theme_light() +
                theme(panel.grid.minor.y = element_blank(), 
                      panel.grid.minor.x = element_blank(), 
                      panel.grid.major.x = element_blank()) +
                scale_x_continuous(
                  breaks = c(2, 4, 6, 8, 10, 12, 14,
                             16, 18, 20, 22, 24, 26,
                             28, 30, 32, 34, 36, 38, 40)) +
                geom_vline(aes(xintercept = 6), 
                           color='lightgrey', alpha = 1, 
                           linetype = 5, size = .5) + 
                annotate("text", x = 6, y=1.4, hjust = 0,
                         size = 1.7, color='#444444',
                         label = " Winter vacation for East \n and Northern Norway") +
                geom_vline(aes(xintercept = 12), 
                           color='lightgrey', alpha = 1, 
                           linetype = 5, size = .5) +
                geom_vline(aes(xintercept = 13), 
                           color = 'darkgrey', alpha = 1, 
                           linetype = 10, size = .5) + 
                annotate("text", x = 13, y = 1.4, hjust = 0,
                         size = 1.7, color = '#444444',
                         label = "  Winter vacation for \n  West and Mid-Norway ") +
                geom_vline(aes(xintercept = 19), 
                           color = 'darkgrey', alpha = 1, 
                           linetype = 10, size = .5) + 
                geom_vline(aes(xintercept = 20), 
                           color='#777777', alpha = 1, 
                           linetype = 1, size = .5) + 
                annotate("text", x = 20, y=1.4, hjust = 0,
                         size = 1.7, color='#444444',
                         label = " Uninterrupted period with viral mitigation protocols") + 
                geom_vline(aes(xintercept = 38), 
                           color = '#777777', alpha = 1, 
                           linetype = 1, size = .5) + 
                annotate("text", x = 38, y = 1.4, hjust = 0,
                         size = 1.7, color = '#444444',
                         label = " Easter holidays \n start date \n in Norway")

dev.off()
dev.off()
dev.off()


#### Mechanistic variables and contextual part 2
pdf(paste0(FiguresPath, 
           "3)  Time-series - Part 3.pdf"),
           width = 8.125, height = 5) 

## Plot
ggplot(filter(DataTS2 %>% 
                filter(Variable == "Helpless" |
                         Variable == "Rumination" |
                         Variable == "Interpersonal conflict" |
                         Variable == "Relatedness" |
                         Variable == "Emotion reg. difficulties" | 
                         Variable == "Physical activity")),
                aes(Day, Mean, colour = Variable)) +
                xlab("Days") +
                geom_smooth(method = "loess", span = 0.1, 
                            se = FALSE, size = .6) +
                theme_light() +
                theme(panel.grid.minor.y = element_blank(), 
                      panel.grid.minor.x = element_blank(), 
                      panel.grid.major.x = element_blank()) +
                scale_x_continuous(
                  breaks = c(2, 4, 6, 8, 10, 12, 14,
                             16, 18, 20, 22, 24, 26, 
                             28, 30, 32, 34, 36, 38, 40)) +
                geom_vline(aes(xintercept = 6), 
                           color='lightgrey', alpha = 1, 
                           linetype = 5, size = .5) + 
                annotate("text", x = 6, y=1.1, hjust = 0,
                         size = 1.6, color='#444444',
                         label = " Winter vacation for East \n and Northern Norway") +
                geom_vline(aes(xintercept = 12), 
                           color='lightgrey', alpha = 1, 
                           linetype = 5, size = .5) +
                geom_vline(aes(xintercept = 13), 
                           color = 'darkgrey', alpha = 1, 
                           linetype = 10, size = .5) + 
                annotate("text", x = 13, y = 1.1, hjust = 0,
                         size = 1.6, color = '#444444',
                         label = "  Winter vacation for \n  West and Mid-Norway ") +
                geom_vline(aes(xintercept = 19), 
                           color = 'darkgrey', alpha = 1, 
                           linetype = 10, size = .5) + 
                geom_vline(aes(xintercept = 20), 
                           color='#777777', alpha = 1, 
                           linetype = 1, size = .5) + 
                annotate("text", x = 20, y = 1.1, hjust = 0,
                         size = 1.6, color='#444444',
                         label = " Uninterrupted period with viral mitigation protocols") + 
                geom_vline(aes(xintercept = 38), 
                           color = '#777777', alpha = 1, 
                           linetype = 1, size = .5) + 
                annotate("text", x = 38, y = 1.1, hjust = 0,
                         size = 1.6, color = '#444444',
                         label = " Easter holidays \n start date \n in Norway")


dev.off()
dev.off()
dev.off()

par(mfrow=c(3,1))

pdf(paste0(FiguresPath, 
           "Figure 1.pdf"),
    width = 8.125) 

dev.off()
dev.off()
dev.off()

##################################################################################
########################## Time-series analyses: End #############################
##################################################################################


##################################################################################
######################        Periodic-specific patterns    ######################    
######################              and associations        ######################
######################         across the study period      ######################
######################                  Start               ######################
##################################################################################

### Periodic-specific patterns and  
##  associations across the study period

## Investigations of:
## Overall associations with strict viral mitigation protocols and holiday period
#  a) Linear (per day) increase across period of strict mitigation protocols
#  b) Linear (per day) associations with holiday periods encompassed in the study 
#  were mitigation protocols were lightened.

### Load package
library("nlme")
library("lmerTest")
options(scipen=100)

## Variables to investigate
vars <- select(Data,
               c('lonely', 'phq1', 'phq2', 'phq4', 'phq6', 
                 'tpq22', 'bis6', 'r_wrq', 'interconf', 'swemwbs6', 
                 'emoreg', 'swemwbs2', 'suffinfoaccess', 'some', 
                 'ntimeofflinesocial', 'ntimedigitalsocial', 'physact'))[0,]

## Inspect Linear lockdown effect: increase as a function 
#  of length (per day) of lockdown? 
#  Lockdown linearly associated with detrimental symptoms. Summary:
#  Loneliness, all symptoms (e.g., depressed mood, anhedonia) and 
#  detrimental mechanisms (e.g., emotion regulation difficulties,
#  helplessness) were detrimentally associated with
#  (i.e., increases in detrimental variables such as depressed mood, 
#  decreases with favorable variables such as relatedness) lockdown,
#  during the consecutive
#  lockdown period with linear increase
#  per day (ps < .001) for all symptoms and mechanisms,
#  with the exception of lethargy, information access needs,
#  rumination sleep satisfaction (ps > .05). 
#  One favorable association: Productivity increased.

### Inspect lockdown effects and their significance
LinearLockDownEffect <- lmer(phq1 ~ LockdownPeriod
                             + (1 | ID), data = Data) # Detrimental association
                                                      # (increase)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(phq2 ~ LockdownPeriod
                             + (1 | ID), data = Data) # Detrimental association
                                                      # (increase)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(lonely ~ LockdownPeriod
                             + (1 | ID), data = Data) # Detrimental association
                                                      # (increase)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(phq4 ~ LockdownPeriod
                             + (1 | ID), data = Data) # Not significant
                                                      # p = .071
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(phq6 ~ LockdownPeriod
                             + (1 | ID), data = Data) # Detrimental association
                                                      # (increase)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(bis6 ~ LockdownPeriod
                             + (1 | ID), data = Data) # No association
                                                      # with sleep problems
                                                      # p = 0.084
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(tpq22 ~ LockdownPeriod
                             + (1 | ID), data = Data) # Detrimental association
                                                      # (increase)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(r_wrq ~ LockdownPeriod
                             + (1 | ID), data = Data) # No change
                                                      # p = .086
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(emoreg ~ LockdownPeriod
                             + (1 | ID), data = Data) # Detrimental association
                                                      # (increase)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(interconf ~ LockdownPeriod
                             + (1 | ID), data = Data) # Detrimental association
                                                      # (increase)
                                                      # p < .001
summary(LinearLockDownEffect)



LinearLockDownEffect <- lmer(suffinfoaccess ~ LockdownPeriod
                             + (1 | ID), data = Data) # No association
                                                      # p = .07
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(some ~ LockdownPeriod
                             + (1 | ID), data = Data) # Sign association
                                                      # (Passive social media
                                                      # use increased)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(physact ~ LockdownPeriod
                             + (1 | ID), data = Data) # Favorable association
                                                      # (Physical act increased
                                                      # (per day)
                                                      # during lockdown, although
                                                      # being associated with 
                                                      # less overall activity
                                                      # during lockdowns.
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(swemwbs6 ~ LockdownPeriod
                             + (1 | ID), data = Data) # Sign association
                                                      # (Relatedness decreased)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(swemwbs2 ~ LockdownPeriod
                             + (1 | ID), data = Data) # Positive association
                                                      # (Producitivty increased
                                                      # per day 
                                                      # during lockdown. 
                                                      # Interesting.)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(ntimeofflinesocial ~ LockdownPeriod
                             + (1 | ID), data = Data) # Sign association
                                                      # (Decrease)
                                                      # p < .001
summary(LinearLockDownEffect)


LinearLockDownEffect <- lmer(ntimedigitalsocial ~ LockdownPeriod
                             + (1 | ID), data = Data) # Sign association
                                                      # (increase)
                                                      # p < .001
summary(LinearLockDownEffect)


## Holiday effect? Yes
#  All variables revealed significant favorable 
#  (i.e., negative variables decreasing, positive variables
#  increasing; e.g., symptoms decreasing (per day
#  during holidays) during the holiday periods (ps < .001),
#  with the exceptions of productivity which meaningfully
#  decreased (break from work), and that online contact decreased while 
#  offline increased, as meaningfully opposed to the lockdown period.

LinearHolidayEffect <- lmer(phq1 ~ LinearHoliday
                      + (1 | ID), data = Data) # Favorable 
                                               # association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(phq2 ~ LinearHoliday
                      + (1 | ID), data = Data)  # Favorable 
                                               # association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(lonely ~ LinearHoliday
                      + (1 | ID), data = Data) # Favorable association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(phq4 ~ LinearHoliday
                      + (1 | ID), data = Data) # Favorable association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(phq6 ~ LinearHoliday
                      + (1 | ID), data = Data)  # Favorable association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(bis6 ~ LinearHoliday
                      + (1 | ID), data = Data)  # Sign association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(tpq22 ~ LinearHoliday
                      + (1 | ID), data = Data)  # Favorable association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(r_wrq ~ LinearHoliday
                      + (1 | ID), data = Data)  # Favorable association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(emoreg ~ LinearHoliday
                      + (1 | ID), data = Data)  # Favorable association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(interconf ~ LinearHoliday
                      + (1 | ID), data = Data)  # Favorable association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)



LinearHolidayEffect <- lmer(suffinfoaccess ~ LinearHoliday
                      + (1 | ID), data = Data) # Sign association
                                               # p = 0.0284

summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(some ~ LinearHoliday
                      + (1 | ID), data = Data) # Sign association
                                               # (Passive social media
                                               # use increased)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(physact ~ LinearHoliday
                      + (1 | ID), data = Data) # Favorable association
                                               # (Physical act increased
                                               # (per day)
                                               # during holidays
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(swemwbs6 ~ LinearHoliday
                      + (1 | ID), data = Data) # Sign association
                                               # (Relatedness increased)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(swemwbs2 ~ LinearHoliday
                      + (1 | ID), data = Data) # Positive association
                                               # (Producitivty decreased
                                               # per day 
                                               # during holidays 
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(ntimeofflinesocial ~ LinearHoliday
                      + (1 | ID), data = Data) # Sign association
                                               # (in-person 
                                               # contact increased)
                                               # p < .001
summary(LinearHolidayEffect)


LinearHolidayEffect <- lmer(ntimedigitalsocial ~ LinearHoliday
                      + (1 | ID), data = Data) # Sign association
                                               # (decrease)
                                               # p < .001
summary(LinearHolidayEffect)




##################################################################################
######################        Periodic-specific patterns    ######################    
######################              and associations        ######################
######################         across the study period      ######################
######################                  End                 ######################
##################################################################################




##################################################################################
######################        Sensitivity analysis       #########################
######################          Random half-split        #########################
######################          of participants          #########################
######################                Start              #########################
##################################################################################

### Analysis 1: Split sample in two random halves
##  ID var of participants already shuffled and randomized
##  Do odd-even split to obtain every other participant

#   Select every other participant
nth_element <- function(vector, starting_position, n) { 
  vector[seq(starting_position, length(vector), n)] 
}

# S1 is vector of IDs of sample 1, S2 IDs of sample 2
sampleHalf1 <- nth_element(unique(Data_detrended$ID), 1, 2)
sampleHalf2 <- nth_element(unique(Data_detrended$ID), 2, 2)
data_sampleHalf1 <- Data_detrended[Data_detrended$ID %in% sampleHalf1,]
data_sampleHalf2 <- Data_detrended[Data_detrended$ID %in% sampleHalf2,]

# Estimate the models
model.sampleHalf1 <- mlVAR(data_sampleHalf1,
                           vars = names,
                           idvar = "ID",
                           temporal = "orthogonal",
                           contemporaneous = "orthogonal")

model.sampleHalf2 <- mlVAR(data_sampleHalf2,
                           vars = names,
                           idvar = "ID",
                           temporal = "orthogonal",
                           contemporaneous = "orthogonal")


### Weights matrices sample half 1:
##  Temporal
View(model.sampleHalf1$results$Beta$mean[,,1])

## Contemporaneous
View(model.sampleHalf1$results$Theta[["pcor"]][["mean"]])

## Between-subjects network
View(model.sampleHalf1$results[["Omega_mu"]][["pcor"]][["mean"]])


### Weights matrices sample half 2
##  Temporal
View(model.sampleHalf2$results$Beta$mean[,,1])

## Contemporaneous
View(model.sampleHalf2$results$Theta[["pcor"]][["mean"]])

## Between-subjects network
View(model.sampleHalf2$results[["Omega_mu"]][["pcor"]][["mean"]])

### Weights matrices sample halves

# Sample half 1
sampleHalf1TemporalWeightsMatrix <- model.sampleHalf1$results$Beta$mean[,,1]
sampleHalf1ContempWeightsMatrix <- model.sampleHalf1$results$Theta[["pcor"]][["mean"]]
sampleHalf1BetweenWeightsMatrix <- model.sampleHalf1$results[["Omega_mu"]][["pcor"]][["mean"]]

# Sample half 2
sampleHalf2TemporalWeightsMatrix <- model.sampleHalf2$results$Beta$mean[,,1]
sampleHalf2ContempWeightsMatrix <- model.sampleHalf2$results$Theta[["pcor"]][["mean"]]
sampleHalf2BetweenWeightsMatrix <- model.sampleHalf2$results[["Omega_mu"]][["pcor"]][["mean"]]

# Correlate weight matrices
cor(sampleHalf1TemporalWeightsMatrix, sampleHalf2TemporalWeightsMatrix)
cor.test(sampleHalf1TemporalWeightsMatrix, sampleHalf2TemporalWeightsMatrix) # Correlation: 0.930!

cor(sampleHalf1ContempWeightsMatrix, sampleHalf2ContempWeightsMatrix)
cor.test(sampleHalf1ContempWeightsMatrix, sampleHalf2ContempWeightsMatrix) # Correlation: 0.999!

cor(sampleHalf1BetweenWeightsMatrix, sampleHalf2BetweenWeightsMatrix)
cor.test(sampleHalf1BetweenWeightsMatrix, sampleHalf2BetweenWeightsMatrix) # Correlation: 0.969!

### Centrality estimates replication - Sample half (Store in objects to compare)
# Sample half 1
graph.tempSample1 <- plot(model.sampleHalf1, "temporal", theme = "colorblind", vsize = 7, labels = 1:length(names))

# Out-Strength
sample1TempOutStrength <- centrality(graph.tempSample1)$OutDegree
sample1TempOutStrength <- data.matrix(sample1TempOutStrength)

# In-Strength
sample1TempInStrength <- centrality(graph.tempSample1)$InDegree
sample1TempInStrength <- data.matrix(sample1TempInStrength)

# Contemporaneous - Strength
graph.contSample1 <- plot(model.sampleHalf1, "contemporaneous", theme = "colorblind", vsize = 7, labels = 1:length(names))
sample1contStrength <- centrality(graph.contSample1)$OutDegree
sample1contStrength <- data.matrix(sample1contStrength)

# Between - Strength
graph.betwSample1 <- plot(model.sampleHalf1, "between",  theme = "colorblind", vsize = 7, labels = 1:length(names))
sample1BetwStrength <- centrality(graph.betwSample1)$OutDegree
sample1BetwStrength <- data.matrix(sample1BetwStrength)



# Sample half 1
graph.tempSample2 <- plot(model.sampleHalf2, "temporal", theme = "colorblind", vsize = 7, labels = 1:length(names))

# Out-Strength
sample2TempOutStrength <- centrality(graph.tempSample2)$OutDegree
sample2TempOutStrength <- data.matrix(sample2TempOutStrength)

# In-Strength
sample2TempInStrength <- centrality(graph.tempSample2)$InDegree
sample2TempInStrength <- data.matrix(sample2TempInStrength)

# Contemporaneous - Strength
graph.contSample2 <- plot(model.sampleHalf2, "contemporaneous", theme = "colorblind", vsize = 7, labels = 1:length(names))
sample2contStrength <- centrality(graph.contSample2)$OutDegree
sample2contStrength <- data.matrix(sample2contStrength)

# Between - Strength
graph.betwSample2 <- plot(model.sampleHalf2, "between", theme = "colorblind", vsize = 7, labels = 1:length(names))
sample2BetwStrength <- centrality(graph.betwSample2)$OutDegree
sample2BetwStrength <- data.matrix(sample2BetwStrength)


## Correlate centrality estimate matrices:
cor(sample1TempOutStrength, sample2TempOutStrength)
cor.test(sample1TempOutStrength, sample2TempOutStrength) # Correlation: 0.800!

cor(sample1TempInStrength, sample2TempInStrength)
cor.test(sample1TempInStrength, sample2TempInStrength) # Correlation: 0.960!

cor(sample1contStrength, sample2contStrength)
cor.test(sample1contStrength, sample2contStrength) # Correlation: 0.994!

cor(sample1BetwStrength, sample2BetwStrength)
cor.test(sample1BetwStrength, sample2BetwStrength) # Correlation: 0.880!




##################################################################################
######################        Sensitivity analysis       #########################
######################          Random half-split        #########################
######################          of participants          #########################
######################                End                #########################
##################################################################################




##################################################################################
######################        Sensitivity analysis       #########################
######################            Half-split             #########################
######################          of time-series           #########################
######################               Start               #########################
##################################################################################

### Analysis 2: Split time period into first and latter half:
data_TSHalf1 <- Data_detrended[Data_detrended$Day %in% 1:20,]
data_TSHalf2 <- Data_detrended[Data_detrended$Day %in% 21:40,]

# Estimate the models
model.TSHalf1 <- mlVAR(data_TSHalf1,
                       vars = names,
                       idvar = "ID",
                       temporal = "orthogonal",
                       contemporaneous = "orthogonal")

model.TSHalf2 <- mlVAR(data_TSHalf2,
                       vars = names,
                       idvar = "ID",
                       temporal = "orthogonal",
                       contemporaneous = "orthogonal")


### Weights matrices TS half 1
##  Temporal
View(model.TSHalf1$results$Beta$mean[,,1])

## Contemporaneous
View(model.TSHalf1$results$Theta[["pcor"]][["mean"]])

## Between-subjects network
View(model.TSHalf1$results[["Omega_mu"]][["pcor"]][["mean"]])

### Weights matrices TS half 2

##  Temporal
View(model.TSHalf2$results$Beta$mean[,,1])

## Contemporaneous
View(model.TSHalf2$results$Theta[["pcor"]][["mean"]])

## Between-subjects network
View(model.TSHalf2$results[["Omega_mu"]][["pcor"]][["mean"]])

### Weights matrices sample halves:

# TS half 1
sampleTSHalf1TemporalWeightsMatrix <- model.TSHalf1$results$Beta$mean[,,1]
sampleTSHalf1ContempWeightsMatrix <- model.TSHalf1$results$Theta[["pcor"]][["mean"]]
sampleTSHalf1BetweenWeightsMatrix <- model.TSHalf1$results[["Omega_mu"]][["pcor"]][["mean"]]

# TS half 2
sampleTSHalf2TemporalWeightsMatrix <- model.TSHalf2$results$Beta$mean[,,1]
sampleTSHalf2ContempWeightsMatrix <- model.TSHalf2$results$Theta[["pcor"]][["mean"]]
sampleTSHalf2BetweenWeightsMatrix <- model.TSHalf2$results[["Omega_mu"]][["pcor"]][["mean"]]

# Correlate weight matrices
cor(sampleTSHalf1TemporalWeightsMatrix, sampleTSHalf2TemporalWeightsMatrix)
cor.test(sampleTSHalf1TemporalWeightsMatrix, sampleTSHalf2TemporalWeightsMatrix) # Correlation: 0.920!

cor(sampleTSHalf1ContempWeightsMatrix, sampleTSHalf2ContempWeightsMatrix)
cor.test(sampleTSHalf1ContempWeightsMatrix, sampleTSHalf2ContempWeightsMatrix) # Correlation: 0.999!

cor(sampleTSHalf1BetweenWeightsMatrix, sampleTSHalf2BetweenWeightsMatrix)
cor.test(sampleTSHalf1BetweenWeightsMatrix, sampleTSHalf2BetweenWeightsMatrix) # Correlation: 0.996!

### Centrality estimates replication - Time-series split-half
##  TS Half 1:
graph.tempTS1 <- plot(model.TSHalf1, "temporal", theme = "colorblind", vsize = 7, labels = 1:length(names))

# Out-Strength
TS1TempOutStrength <- centrality(graph.tempTS1)$OutDegree
TS1TempOutStrength <- data.matrix(TS1TempOutStrength)

# In-Strength
TS1TempInStrength <- centrality(graph.tempTS1)$InDegree
TS1TempInStrength <- data.matrix(TS1TempInStrength)

# Contemporaneous - Strength
graph.contTS1 <- plot(model.TSHalf1, "contemporaneous", theme = "colorblind", vsize = 7, labels = 1:length(names))
TS1contStrength <- centrality(graph.contTS1)$OutDegree
TS1contStrength <- data.matrix(TS1contStrength)

# Between - Strength
graph.betwTS1 <- plot(model.TSHalf1, "between", theme = "colorblind", vsize = 7, labels = 1:length(names))
TS1BetwStrength <- centrality(graph.betwTS1)$OutDegree
TS1BetwStrength <- data.matrix(TS1BetwStrength)

## TS Half 2:
graph.tempTS2 <- plot(model.TSHalf2, "temporal", theme = "colorblind", vsize = 7, labels = 1:length(names))

# Out-Strength
TS2TempOutStrength <- centrality(graph.tempTS2)$OutDegree
TS2TempOutStrength <- data.matrix(TS2TempOutStrength)

# In-Strength
TS2TempInStrength <- centrality(graph.tempTS2)$InDegree
TS2TempInStrength <- data.matrix(TS2TempInStrength)

# Contemporaneous - Strength
graph.contTS2 <- plot(model.TSHalf2, "contemporaneous", theme = "colorblind", vsize = 7, labels = 1:length(names))
TS2contStrength <- centrality(graph.contTS2)$OutDegree
TS2contStrength <- data.matrix(TS2contStrength)

# Between - Strength
graph.betwTS2 <- plot(model.TSHalf2, "between", theme = "colorblind", vsize = 7, labels = 1:length(names))
TS2BetwStrength <- centrality(graph.betwTS2)$OutDegree
TS2BetwStrength <- data.matrix(TS2BetwStrength)

## Correlate centrality estimate matrices:
cor(TS1TempOutStrength, TS2TempOutStrength)
cor.test(TS1TempOutStrength, TS2TempOutStrength) # Correlation: 0.880!

cor(TS1TempInStrength, TS2TempInStrength)
cor.test(TS1TempInStrength, TS2TempInStrength) # Correlation: 0.890!

cor(TS1contStrength, TS2contStrength)
cor.test(TS1contStrength, TS2contStrength) # Correlation: 0.996!

cor(TS1BetwStrength, TS2BetwStrength)
cor.test(TS1BetwStrength, TS2BetwStrength) # Correlation: 0.970!




##################################################################################
######################        Sensitivity analysis       #########################
######################            Half-split             #########################
######################          of time-series           #########################
######################               End                 #########################
##################################################################################






##################################################################################
#############  Within and across-day dynamics of depressive symptoms  ############
#############         and related psychopathological processes:       ############
############# A dynamic network approach during the COVID-19 pandemic ############ 
#############                                                         ############
#############             Omid V. Ebrahimi, Julian Burger             ############ 
#############           Asle Hoffart, Sverre Urnes Johnson            ############
#############                                                         ############
#############                   Code and analyses by:                 ############
#############             Omid V. Ebrahimi & Julian Burger            ############
##################################################################################