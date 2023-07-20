#CODE FOR RESULTS FOR Internalizing Symptoms, Wellbeing and Correlates in Adolescence:
#A Multiverse Exploration via Cross-lagged Panel Network Models



library(psych)
library(dplyr)
library(gdata)
library(qgraph)
library(ggplot2)
library(stringr)
library(frequency)
library(tidyr)
library(ggrepel)

#--------------------------------------data descriptives NEED REAL DATA
allDesc<- describe(allpossF[, 2:79])
allDesc<- as.data.frame(allDesc)
allDesc<- allDesc[, c(3, 4, 11)]
allDesc<- round(allDesc, 2)

brack<-rep("(", 78)
et<- rep(")", 78)
allDesc$MSD<- paste(allDesc$mean, brack, allDesc$sd, et, sep = "")


allDesc$item<- rownames(allDesc)
allDesc<- allDesc[c(1:24, 73, 74, 25:48, 75, 76, 49:72, 77, 78),]


allItems<- rep(c("optimismA", "useful", "relaxed", "problemA", "think", "close", "mindA", "restless",
                 "somatic", "withdrawn", "worry", "unhappy", "distractedA", "nervous", "bully",
                 "scared", "distractedB", "stress", "problemB", "optimismB", "home", "school",
                 "peer", "help", "mindB", "feelings"), 3)

times<- c(rep("T1", 22), rep("T2", 22), rep("T3", 22))
allItems<- paste(allItems, times, sep = "_")
allDesc$abbrev<- allItems
allDesc<- allDesc%>%arrange(abbrev)
allDesc<- allDesc[, c(6, 5, 4, 3)]


write.csv(allDesc, file = "allDesc.csv")
freq(allpossF)
#--------------extract results---------


# dataframe fits ----------------------------------------------------------
#models: unpruned, pruned, pruned configural, pruned constrained, stepup, stepup configural, stepup constrained
#example for 3 dfs
dfs<- c("df14", "df15", "df16")
dfFit<- rep(dfs, 7)
modname<- c(rep("unpruned",3), rep( "pruned",3), rep("pruned configural",3), rep( "pruned constrained", 3),
            rep("stepup",3), rep( "stepup configural", 3), rep( "stepup constrained", 3))

myNames<- paste(dfs, modname, sep = ".")

fit141516<- data.frame(matrix(unlist(c(unprunedfit, prunedfit, prunedconfig.fit, prunedconstrained.fit,
                                       stepupfit, stepupconfig.fit, stepupconstrained.fit)), 
                              nrow=length(c(unprunedfit, prunedfit, prunedconfig.fit, prunedconstrained.fit,
                                            stepupfit, stepupconfig.fit, stepupconstrained.fit)), byrow=T))

colnames(fit141516)<- names(prunedconfig.fit[[1]])
rownames(fit141516)<- myNames


# network descriptives --------------------------------------------------


desc141516<- data.frame(matrix(unlist(c(prunedbet.desc, prunedcont.desc, prunedtemp.desc,
                                        stepupbet.desc, stepupcont.desc, stepuptemp.desc)), 
                               nrow=length(c(prunedbet.desc, prunedcont.desc, prunedtemp.desc,
                                             stepupbet.desc, stepupcont.desc, stepuptemp.desc)), byrow=T))

mods<- c(rep("pruned", 9), rep("stepup", 9))
dfsDesc<- rep(dfs, 6)
matrix<- c(rep("between", 3), rep("contemporaneous", 3), rep("temporal", 3))
matrix<- rep(matrix, 2)
descNames<- paste(dfsDesc, mods, matrix, sep= ".")

colnames(desc141516)<- colnames(prunedbet.desc[[1]])
rownames(desc141516)<- descNames



# invariance --------------------------------------------------------------

pruned.comparison<- lapply(prunedcomparison, na.omit)
stepup.comparison<- lapply(stepupcomparison, na.omit)

comparison141516<- data.frame(matrix(unlist(c(pruned.comparison, stepup.comparison)), 
                                     nrow=length(c(pruned.comparison, stepup.comparison)),
                                     byrow=T))

dfs<- c("df14", "df15", "df16")
mod<- c(rep("pruned", 3), rep("stepup", 3))  
df<- rep(dfs, 2)
condiNames<- paste(df, mod, sep = ".")

colnames(comparison141516)<- colnames(pruned.comparison[[1]])
rownames(comparison141516)<- condiNames

ICs141516<- data.frame(matrix(unlist(c(prunedICs, stepupICs)), 
                              nrow=length(c(prunedICs, stepupICs)),
                              byrow=T))

colnames(out141516)<- condiNames

colnames(ICs141516)<- colnames(prunedICs[[1]])
rownames(ICs141516)<- condiNames

invariance141516<- cbind(comparison141516, ICs141516)


# save --------------------------------------------------------------------


save(out141516, file = "out141516.RData")
save(fit141516, file= "fit141516.RData")
save(desc141516, file= "desc141516.RData")
save(invariance141516, file = "invariance141516.RData")

# combine results---------------------------------------------------------
#note objects reflect chunks broken up
#into to manage computation time
multiverseFit<- rbind(fit1234, fit5, fit678, fit91011, fit12, fit13, fit141516) 
write.csv(multiverseFit, file = "multiversefit.csv")

multiverseDesc<- rbind(desc1234, desc5, desc678, desc91011, desc12, desc13, desc141516)
write.csv(multiverseDesc, file = "multiverseDesc.csv")

multiverseInvar<- rbind(invariance1234, invariance5, invariance678, invariance91011, invariance12, invariance13, invariance141516)
write.csv(multiverseInvar, file = "mulitverseInvar.csv")

# average fits ------------------------------------------------------------

multiFit<- read.csv(file = "multiversefit.csv")
unprunedFit<- multiFit[1:16,]
prunedFit<- multiFit[17:32, ]
stepupFit<- multiFit[33:48,]
prunedConfigFit<- multiFit[49:64,]
stepConfigFit<- multiFit[65:80,]
prunedConstFit<- multiFit[81:96,]
stepConstFit<- multiFit[97:112,]

modelTypes<- list(unprunedFit, prunedFit, stepupFit, prunedConfigFit, stepConfigFit, prunedConstFit, stepConstFit )

averageFits<- list()

for (i in 1:7) {
descript<- as.data.frame(describe(modelTypes[[i]]))
averageFits[[i]]<- descript[c("chisq", "df", "cfi", "rmsea", "aic.ll", "bic"), c(2:4,8,9 )]
}

write.csv(averageFits, file = "aveFits.csv")

#describe network parameters----------------------------------------------
multiDesc<- read.csv("multiverseDesc.csv")
#between networks
betweensDesc<- multiDesc%>% filter(str_detect(model, 'between'))
betweenMins<-describe(abs(betweensDesc$min))
betweenMaxs<- describe(abs(betweensDesc$max))
betweenMofM<- describe(betweensDesc$mean)

#check negatives
negativesB<- list()
lengthNB<- list()
#find negatives
for (i in 1:32) {
   network<- lowerTriangle(betweens[[i]])
   negativesB[[i]]<- which(network<0, arr.ind = T)
   lengthNB[[i]]<- length(negatives[[i]])
}

#contemporaneous networks
contempsDesc<- multiDesc%>% filter(str_detect(model, 'contemporaneous'))
contempMins<- describe(abs(contempsDesc$min))
contempsMaxs<- describe(abs(contempsDesc$max))
contempsMofM<- describe(contempsDesc$mean)

negativesC<- list()
lengthCN<- list()
for (i in 1:32) {
   network<- lowerTriangle(contemps[[i]])
   negativesC[[i]]<- which(network<0, arr.ind = T)
   lengthCN[[i]]<- length(negativesC[[i]])
   
}

lowerTriangle(contemps[[1]])[50] #identify example negative parameter present across conditions

#temporal networks
temporalsDesc<- multiDesc%>% filter(str_detect(model, 'temporal'))
temporalMins<- describe(abs(temporalsDesc$min))
temporalsMaxs<- describe(abs(temporalsDesc$max))
temporalMofM<- describe(temporalsDesc$mean)

negativesT<- list()
lengthTN<- list()
for (i in 1:32) {
   network<- temporals[[i]]
   negativesT[[i]]<- which(network<0, arr.ind = T)
   lengthTN[[i]]<- length(negativesT[[i]])/2
   
}

#check non-sig results
load("S7parametersAll.RData")
nsPara<- list()

for (i in 1:32) {
   condition<- parameters.ALL[[i]]
   index<- which(condition$p>.01, arr.ind = T)
   nsPara[[i]]<- condition[index, 12:14]
}



n.NS<- lapply(nsPara, nrow)

describe(as.numeric(mapply(paste, n.NS, collapse= ",")))


allNSpara<- bind_rows(nsPara, .id = "column_label")
allNSpara<- allNSpara%>% unite(con, matrix, row, col, sep = "_")

freq.ns.par<- table(allNSpara$con)
max(freq.ns.par)

# post-hoc invariance CFI/RMSEA-------------------------------------------
Fits<-read.csv("multiverseFit.csv")
Inv<- Fits[49:112,]
Inv<- Inv%>% mutate(condition= sapply(strsplit(as.character(X), " "), `[`, 1))
Inv<- Inv%>% mutate(model= sapply(strsplit(as.character(X), " "), `[`, 2))
Inv<- Inv[, c(22, 23, 37, 38)]

InvWide<- pivot_wider(Inv, names_from = model, values_from = c(cfi, rmsea))
InvWide<- InvWide%>% mutate(dCFI= cfi_constrained-cfi_configural)
InvWide<- InvWide%>% mutate(dRMSEA= rmsea_configural-rmsea_constrained)
descCFI<- describe(InvWide$dCFI)
descRMSEA<- describe(InvWide$dRMSEA)


# plotting networks -------------------------------------------------------

#Create lists of each of the networks across conditions- note code relates to analysis objects, actual analysis
#was performed in chunks with different objects- this should theoretically work if possible to run main analysis
#loop in a single go. Code used can be requested from the first author but is more difficult to read

 temporals<- list(prunedtemporal[[1]], stepuptemporal[[1]],
                  prunedtemporal[[2]], stepuptemporal[[2]],
                  prunedtemporal[[3]], stepuptemporal[[3]],
                  prunedtemporal[[4]], stepuptemporal[[4]],
                  prunedtemporal[[5]], stepuptemporal[[5]],
                  prunedtemporal[[6]], stepuptemporal[[6]],
                  prunedtemporal[[7]], stepuptemporal[[7]],
                  prunedtemporal[[8]], stepuptemporal[[8]],
                  prunedtemporal[[9]], stepuptemporal[[9]], 
                  prunedtemporal[[10]], stepuptemporal[[10]],
                  prunedtemporal[[11]], stepuptemporal[[11]],
                  prunedtemporal[[12]], stepuptemporal[[12]],
                  prunedtemporal[[13]], stepuptemporal[[13]],
                  prunedtemporal[[14]], stepuptemporal[[14]],
                  prunedtemporal[[15]], stepuptemporal[[15]],
                  prunedtemporal[[16]], stepuptemporal[[16]])

betweens<- list(prunedbetween[[1]], stepupbetween[[1]],
                prunedbetween[[2]], stepupbetween[[2]],
                prunedbetween[[3]], stepupbetween[[3]],
                prunedbetween[[4]], stepupbetween[[4]],
                prunedbetween[[5]], stepupbetween[[5]],
                prunedbetween[[6]], stepupbetween[[6]],
                prunedbetween[[7]], stepupbetween[[7]],
                prunedbetween[[8]], stepupbetween[[8]],
                prunedbetween[[9]], stepupbetween[[9]], 
                prunedbetween[[10]], stepupbetween[[10]],
                prunedbetween[[11]], stepupbetween[[11]],
                prunedbetween[[12]], stepupbetween[[12]],
                prunedbetween[[13]], stepupbetween[[13]],
                prunedbetween[[14]], stepupbetween[[14]],
                prunedbetween[[15]], stepupbetween[[15]],
                prunedbetween[[16]], stepuptemporal[[16]]
               )

contemps<- list(prunedcontemporaneous[[1]], stepupcontemporaneous[[1]],
                prunedcontemporaneous[[2]], stepupcontemporaneous[[2]],
                prunedcontemporaneous[[3]], stepupcontemporaneous[[3]],
                prunedcontemporaneous[[4]], stepupcontemporaneous[[4]],
                prunedcontemporaneous[[5]], stepupcontemporaneous[[5]],
                prunedcontemporaneous[[6]], stepupcontemporaneous[[6]],
                prunedcontemporaneous[[7]], stepupcontemporaneous[[7]],
                prunedcontemporaneous[[8]], stepupcontemporaneous[[8]],
                prunedcontemporaneous[[9]], stepupcontemporaneous[[9]], 
                prunedcontemporaneous[[10]], stepupcontemporaneous[[10]],
                prunedcontemporaneous[[11]], stepupcontemporaneous[[11]],
                prunedcontemporaneous[[12]], stepupcontemporaneous[[12]],
                prunedcontemporaneous[[13]], stepupcontemporaneous[[13]],
                prunedcontemporaneous[[14]], stepupcontemporaneous[[14]],
                prunedcontemporaneous[[15]], stepupcontemporaneous[[15]],
                prunedcontemporaneous[[16]], stepupcontemporaneous[[16]]
                )
 
 edgeInclude <- function(x){
   n <- length(x)
   res <- 1/n * Reduce("+",lapply(x,function(x)x!=0))
   100 * res
 }
 
incPtemps<- edgeInclude(temporals)
toZerotemps<- which(incPtemps<50, arr.ind = T)

length(incPtemps[incPtemps!=0])
#182 parameters appear in any model
ests<- incPtemps[incPtemps!=0]
length(ests[ests>=50])
#182 parameters appear in at least 50% of models
#possible parameters is 484 so 37.60% of parameters retained in average PDC


tempToPlot<- list()

for (i in 1:32){
   temporals[[i]][toZerotemps]<- 0
   tempToPlot[[i]]<- temporals[[i]]
}



aveTemp<- Reduce("+", tempToPlot) / length(tempToPlot)
write.csv(aveTemp, file = "averageTemporalNet.csv")



incPbets<- edgeInclude(betweens)
tozeroBets<- which(incPbets<50, arr.ind = T)

betsToPlot<- list()   
   
for (i in 1:32) {
   betweens[[i]][tozeroBets]<- 0
   betsToPlot[[i]]<- betweens[[i]]
}

aveBet<- Reduce("+", betsToPlot)/length(betsToPlot)
write.csv(aveBet, file = "averageBetweenNet.csv")

lowerT<- lowerTriangle(incPbets)
ests<- lowerT[lowerT!=0]
length(ests)
#6 parameters appear across all conditions
length(ests[ests>=50])
#6 parameters appear in at least 50% of models
#possible parameters is 231 so 2.60% of parameters retained in average between



incPcont<-edgeInclude(contemps)
tezeroCont<- which(incPcont<50, arr.ind = T)

contToPlot<- list()
for (i in 1:32) {
   contemps[[i]][tezeroCont]<- 0
   contToPlot[[i]]<- contemps[[i]]
}


aveCont<- Reduce("+", contToPlot)/length(contToPlot)
write.csv(aveCont, file = "averageContempNet.csv")



lowerT<- lowerTriangle(incPcont)
ests<- lowerT[lowerT!=0]
length(ests)
#134 parameters appear across all conditions
length(ests[ests>=50])
#134 parameters appear in at least 50% of models
#possible parameters is 231 so 58.01% of parameters retained in average contemporaneous

negatives<- list()
length<- list()
#find negatives
for (i in 1:32) {
   network<- lowerTriangle(contemps[[i]])
   negatives[[i]]<- which(network<0, arr.ind = T)
   length[[i]]<- length(negatives[[i]])
}

freq(unlist(negatives))

names<- c("useful", "relaxed", "think", "close", "somatic", "worry", "unhappy", "nervous", "scared", "restless", "withdrawn", 
          "bully", "stress", "feelings", "home", "school", "peer", "help", "optimism", "problem", "mind", "distracted")

max <- max(c(abs(aveTemp),abs(aveCont),abs(aveBet)))
layout<- averageLayout(aveTemp, aveCont, aveBet, layout = "spring")
groups<- c(rep("wb", 4), rep("int", 7), rep("cor", 7), "wb", "cor", "wb", "int")


tempPlot<- qgraph(aveTemp, labels = names,
       theme = "colorblind", repulsion = 0.7, maximum= max, layout= layout, groups= groups, color= c("#FFCCFF", "#FFFFCC", "#99CCCC"),
       vsize = 8, 
       label.cex = 1.1,
       legend = FALSE,
       filetype= "pdf",
       filename= "tempPlot")



contempPlot<- qgraph(aveCont, labels = names,
       theme = "colorblind", repulsion = 0.7, maximum= max, layout= layout, groups= groups, color= c("#FFCCFF", "#FFFFCC", "#99CCCC"),
       vsize = 8, label.cex = 1.1,
       filetype= "pdf",
       filename= "contempPlot",
       legend = FALSE)


betPlot<- qgraph(aveBet, labels = names,
       theme = "colorblind", repulsion = 0.7, maximum= max, layout= layout, groups= groups, color= c("#FFCCFF", "#FFFFCC", "#99CCCC"),
       vsize = 8, label.cex = 1.1,
       filetype= "pdf",
       filename= "betPlot",
       legend = FALSE)


save(temporals, file = "temporals.RData")
save(betweens, file = "betweens.RData")
save(contemps, file = "contemps.RData")

aveBetdesc<-crossNetDescript(aveBet)
aveContdesc<- crossNetDescript(aveCont)
aveTempdesc<- temporalDescript(aveTemp)

#CENTRALITY PLOTS
# In strength -------------------------------------------------------------
#note again that this code relates to the example analysis code given while the reported analysis (available on request)
#used slightly different objects due to computation time

InStrength<- cbind(PDCcentP[[1]]$InStrength, PDCcentS[[1]]$InStrength, 
                   PDCcentP[[2]]$InStrength, PDCcentS[[2]]$InStrength, 
                   PDCcentP[[3]]$InStrength, PDCcentS[[3]]$InStrength,
                   PDCcentP[[4]]$InStrength, PDCcentS[[4]]$InStrength,
                   PDCcentP[[5]]$InStrength, PDCcentS[[5]]$InStrength,
                   PDCcentP[[6]]$InStrength, PDCcentS[[6]]$InStrength,
                   PDCcentP[[7]]$InStrength, PDCcentS[[7]]$InStrength,
                   PDCcentP[[8]]$InStrength, PDCcentS[[8]]$InStrength,
                   PDCcentP[[9]]$InStrength, PDCcentS[[9]]$InStrength,
                   PDCcentP[[10]]$InStrength, PDCcentS[[10]]$InStrength,
                   PDCcentP[[11]]$InStrength, PDCcentS[[11]]$InStrength,
                   PDCcentP[[12]]$InStrength, PDCcentS[[12]]$InStrength,
                   PDCcentP[[13]]$InStrength, PDCcentS[[13]]$InStrength,
                   PDCcentP[[14]]$InStrength, PDCcentS[[14]]$InStrength,
                   PDCcentP[[15]]$InStrength, PDCcentS[[15]]$InStrength,
                   PDCcentP[[16]]$InStrength, PDCcentS[[16]]$InStrength)
rownames(InStrength)<- names

dfs<- rep("df", 32)
n<- c(1, 1, 2, 2, 3,3 , 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13,13, 14, 14, 15, 15, 16, 16)
dfs<- paste(dfs, n, sep = "")
algos<- rep(c("prune", "stepup"), 16)
mods<- paste(dfs, algos, sep=".")
colnames(InStrength)<- mods

InStrength<- as.data.frame(InStrength)
InStrforDescript<- t(InStrength)

describe(InStrforDescript)



InStrforDescript<- as.data.frame(InStrforDescript)
InStrforDescript<- InStrforDescript%>%mutate(condition= rownames(InStrforDescript))
InStrengL<- InStrforDescript%>%pivot_longer(cols = names, names_to = "node", values_to = "InStrength")


InStrDes<- describe(InStrforDescript[, 1:22])
InStrDes<- InStrDes%>% mutate(node= rownames(InStrDes))
InStrDes<- InStrDes %>% arrange(desc(max))




InStrengL<- InStrengL %>%
   mutate(label = if_else(condition == max(condition), as.character(node), NA_character_))
nodesToKeep<- c("think", "worry", "mind", "stress", "problem", "unhappy", "useful", "feelings", "relaxed", "distracted", "help")

labels<- filter(InStrengL, node== nodesToKeep[1]|node==nodesToKeep[2]|node==nodesToKeep[3]|node==nodesToKeep[4]|
                   node==nodesToKeep[5]|node==nodesToKeep[6]|node==nodesToKeep[7]|node==nodesToKeep[8]
                |node==nodesToKeep[9]|node==nodesToKeep[10]|node==nodesToKeep[11])

InStrengthPlot<-ggplot(data= InStrengL, aes(x = condition, y = InStrength, group = node)) +
   geom_line(aes(color = node, alpha = 1), size = 1) +
   theme(legend.position = "none") +
   labs(x = "Condition",
        y = "In Strength",
        title = "Multiverse In Strength")+
   theme(panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))+
   scale_x_discrete(labels= NULL, expand = expansion(c(0,.1)))+
   geom_text_repel(data= labels, aes(label = label),
                   nudge_x = 4.5,
                   na.rm = TRUE, size= 4, fontface= "plain", color= "grey30")+
   scale_color_manual(values =  c( rep("grey", 2), #bully, close
                                   "#f0abff", #distracted
                                   "#f0a600", #feelings
                                   "#e35adc", #help
                                   "grey", #home
                                   "#496300", #mind
                                   "grey", # nervous
                                   "grey", # optimism
                                   "grey", # peer
                                   "#df0042", #problem
                                   "#02beed", #relaxed
                                   "grey", # restless
                                   "grey", # scared
                                   "grey", #school
                                   "grey", #somatic
                                   "#a43300", #stress
                                   "#006b4e", #think
                                   "#f2b3d9", #unhappy
                                   "#f2bc72", #useful
                                   "grey", #withdrawn
                                   "#793d6d"))+ #worry
   theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16),
         plot.title = element_text(size=22))
ggsave(InStrengthPlot, filename = "InStrengthPlot.pdf", width = 9, height= 5, units = "in")


# out strength ------------------------------------------------------------

OutStrength<-cbind(PDCcentP[[1]]$OutStrength, PDCcentS[[1]]$OutStrength, 
                   PDCcentP[[2]]$OutStrength, PDCcentS[[2]]$OutStrength, 
                   PDCcentP[[3]]$OutStrength, PDCcentS[[3]]$OutStrength,
                   PDCcentP[[4]]$OutStrength, PDCcentS[[4]]$OutStrength,
                   PDCcentP[[5]]$OutStrength, PDCcentS[[5]]$OutStrength,
                   PDCcentP[[6]]$OutStrength, PDCcentS[[6]]$OutStrength,
                   PDCcentP[[7]]$OutStrength, PDCcentS[[7]]$OutStrength,
                   PDCcentP[[8]]$OutStrength, PDCcentS[[8]]$OutStrength,
                   PDCcentP[[9]]$OutStrength, PDCcentS[[9]]$OutStrength,
                   PDCcentP[[10]]$OutStrength, PDCcentS[[10]]$OutStrength,
                   PDCcentP[[11]]$OutStrength, PDCcentS[[11]]$OutStrength,
                   PDCcentP[[12]]$OutStrength, PDCcentS[[12]]$OutStrength,
                   PDCcentP[[13]]$OutStrength, PDCcentS[[13]]$OutStrength,
                   PDCcentP[[14]]$OutStrength, PDCcentS[[14]]$OutStrength,
                   PDCcentP[[15]]$OutStrength, PDCcentS[[15]]$OutStrength,
                   PDCcentP[[16]]$OutStrength, PDCcentS[[16]]$OutStrength)
rownames(OutStrength)<- names

OutStrength<- as.data.frame(OutStrength)
colnames(OutStrength)<- mods

OutStrforDescript<- t(OutStrength)

describe(OutStrforDescript)



OutStrforDescript<- as.data.frame(OutStrforDescript)
OutStrforDescript<- OutStrforDescript%>%mutate(condition= rownames(OutStrforDescript))
OutStrengL<- OutStrforDescript%>%pivot_longer(cols = names, names_to = "node", values_to = "OutStrength")


OutStrDes<- describe(OutStrforDescript[, 1:22])
OutStrDes<- OutStrDes%>% mutate(node= rownames(OutStrDes))
OutStrDes<- OutStrDes %>% arrange(desc(max))

OutStrengL<- OutStrengL %>%
   mutate(label = if_else(condition == max(condition), as.character(node), NA_character_))
nodesOKeep<- c("feelings", "stress", "home", "worry", "help", "useful", "nervous", "scared", "somatic", "relaxed", "distracted")
labelsO<- filter(OutStrengL, node== nodesOKeep[1]|node==nodesOKeep[2]|node==nodesOKeep[3]|node==nodesOKeep[4]|
                    node==nodesOKeep[5]|node==nodesOKeep[6]|node==nodesOKeep[7]|node==nodesOKeep[8]
                 |node==nodesOKeep[9]|node==nodesOKeep[10]|node==nodesOKeep[11])

OutStrengthPlot<-ggplot(data= OutStrengL, aes(x = condition, y = OutStrength, group = node)) +
   geom_line(aes(color = node, alpha = 1), size = 1) +
   theme(legend.position = "none") +
   labs(x = "Condition",
        y = "Out Strength",
        title = "Multiverse Out Strength")+
   theme(panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))+
   scale_x_discrete(labels= NULL, expand = expansion(c(0,.1)))+
   geom_text_repel(data= labelsO,aes(label = label),
                   nudge_x = 4.5,
                   na.rm = TRUE, size= 4.8, fontface= "plain", color= "grey30")+
   scale_color_manual(values =  c( rep("grey", 2), #bully, close
                                   "#f0abff", #distracted
                                   "#f0a600", #feelings
                                   "#e35adc", #help
                                   "#496300", #home
                                   "grey", #mind
                                   "#df0042", # nervous
                                   "grey", # optimism
                                   "grey", # peer
                                   "grey", #problem
                                   "#02beed", #relaxed
                                   "grey", # restless
                                   "#a43300", # scared
                                   "grey", #school
                                   "#006b4e", #somatic
                                   "#f2b3d9", #stress
                                   "grey", #think
                                   "grey", #unhappy
                                   "#f2bc72", #useful
                                   "grey", #withdrawn
                                   "#793d6d"))+ #worry
   theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16),
         plot.title = element_text(size=22))
ggsave(OutStrengthPlot, filename = "OutStrengthPlot.pdf",width = 9, height= 6, units = "in")


# contemporaneous strength ------------------------------------------------

centStrength<- cbind(contCentP[[1]]$Strength, contCentS[[1]]$Strength, 
                     contCentP[[2]]$Strength, contCentS[[2]]$Strength, 
                     contCentP[[3]]$Strength, contCentS[[3]]$Strength,
                     contCentP[[4]]$Strength, contCentS[[4]]$Strength,
                     contCentP[[5]]$Strength, contCentS[[5]]$Strength,
                     contCentP[[6]]$Strength, contCentS[[6]]$Strength,
                     contCentP[[7]]$Strength, contCentS[[7]]$Strength,
                     contCentP[[8]]$Strength, contCentS[[8]]$Strength,
                     contCentP[[9]]$Strength, contCentS[[9]]$Strength,
                     contCentP[[10]]$Strength, contCentS[[10]]$Strength,
                     contCentP[[11]]$Strength, contCentS[[11]]$Strength,
                     contCentP[[12]]$Strength, contCentS[[12]]$Strength,
                     contCentP[[13]]$Strength, contCentS[[13]]$Strength,
                     contCentP[[14]]$Strength, contCentS[[14]]$Strength,
                     contCentP[[15]]$Strength, contCentS[[15]]$Strength,
                     contCentP[[16]]$Strength, contCentS[[16]]$Strength)

ContStrength<- do.call(cbind.data.frame, centStrength)
names(ContStrength)


rownames(ContStrength)<- names


colnames(ContStrength)<- mods


ContStrforDescript<- t(ContStrength)

describe(ContStrforDescript)



ContStrforDescript<- as.data.frame(ContStrforDescript)
ContStrforDescript<- ContStrforDescript%>%mutate(condition= rownames(ContStrforDescript))
ContStrengL<- ContStrforDescript%>%pivot_longer(cols = names, names_to = "node", values_to = "ContStrength")


ContStrDes<- describe(ContStrforDescript[, 1:22])
ContStrDes<- ContStrDes%>% mutate(node= rownames(ContStrDes))
ContStrDes<- ContStrDes %>% arrange(desc(max))

ContStrengL<- ContStrengL %>%
   mutate(label = if_else(condition == max(condition), as.character(node), NA_character_))

nodesCkeep<- filter(ContStrDes, max>.90)
nodesCkeep<- nodesCkeep$node
labelsC<- filter(ContStrengL, node== nodesCkeep[1]|node==nodesCkeep[2]|node==nodesCkeep[3]|node==nodesCkeep[4]|
                    node==nodesCkeep[5]|node==nodesCkeep[6]|node==nodesCkeep[7]|node==nodesCkeep[8]
                 |node==nodesCkeep[9]|node==nodesCkeep[10]|node==nodesCkeep[11])

ContStrengthPlot<-ggplot(data= ContStrengL, aes(x = condition, y = ContStrength, group = node)) +
   geom_line(aes(color = node, alpha = 1), size = 1) +
   theme(legend.position = "none") +
   labs(x = "condition",
        y = "Cont strength",
        title = "Multiverse Contemporaneous strength")+
   theme(panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))+
   scale_x_discrete(labels= NULL, expand = expansion(c(0,.1)))+
   geom_text_repel(data= labelsC, aes(label = label),
                   nudge_x = 4.5,
                   na.rm = TRUE, size= 4.8, fontface= "plain", color= "grey30")+
   scale_color_manual(values =  c( "grey", #bully
                                   "#f0abff",# close
                                   "grey", #distracted
                                   "#f0a600", #feelings
                                   "#e35adc", #help
                                   "grey", #home
                                   "#496300", #mind
                                   "grey", # nervous
                                   "grey", # optimism
                                   "grey", # peer
                                   "#df0042", #problem
                                   "#02beed", #relaxed
                                   "grey", # restless
                                   "grey", # scared
                                   "grey", #school
                                   "grey", #somatic
                                   "#f2b3d9", #stress
                                   "#a43300", #think
                                   "#006b4e", #unhappy
                                   "#f2bc72", #useful
                                   "grey", #withdrawn
                                   "#793d6d"))+ #worry
   theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16),
         plot.title = element_text(size=22))



ggsave(ContStrengthPlot, file= "contStrengPlot.pdf", width = 297, height= 210, units = "mm")


# correlation weight matrices ---------------------------------------------

crossNetDescript<- function(network){
   vector<- as.vector(lowerTriangle(network))
   estimates<- vector[vector !=0]
   descriptives<- describe(estimates)
   return(descriptives)
}
#496 correlations per network
tempVecs<- lapply(temporals, as.vector)
tempDF<- data.frame(do.call(cbind, tempVecs))
tempSpearman<-cor(tempDF, method = "spearman")
tempCorDesc<-crossNetDescript(tempSpearman)

write.csv(tempSpearman, file = "spearman correlations 32 temporal networks.csv")

contempVecs<- lapply(contemps, lowerTriangle)
contempDF<- data.frame(do.call(cbind, contempVecs))
contempSpearman<- cor(contempDF, method= "spearman")
contempCorDesc<- crossNetDescript(contempSpearman)

write.csv(contempSpearman, file = "spearman correlations 32 contemporaneous networks.csv")


betweenVecs<- lapply(betweens, lowerTriangle)
betweenDF<- data.frame(do.call(cbind, betweenVecs))
betweenSpearman<- cor(betweenDF, method = "spearman")
betweenCorDesc<- crossNetDescript(betweenSpearman)

write.csv(betweenSpearman, file = "spearman correlations 32 between networks.csv")

