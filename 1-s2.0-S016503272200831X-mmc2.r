library(mlVAR); library(qgraph); library(lme4); library(lmerTest)

#Check for Time Trends
m1 <- lmer(Cheerful~ Time + (Time | Name),data=dataset); m2 <- lmer(Content~ Time + (Time | Name),data=dataset); m3 <-lmer(Sad~ Time + (Time | Name),data=dataset); m4 <- lmer(Anxious ~ Time + (Time | Name),data=dataset);  m5 <- lmer(ActivityT~ Time + (Time | Name),data=dataset)

summary(m1);summary(m2);summary(m3);summary(m4);summary(m5)



##Temporal and Contemporaneous Network Analyses for PA and NA
fit1 <- mlVAR(dataset, vars = c("Cheerful", "Content", "Sad", "Anxious"), idvar = "Name", lags = 1)
g1<-plot(fit1,"temporal", label.scale.equal=T, label.cex=1.2, posCol= "black", negCol="black", negDashed=T, fade=F)
g2<-plot(fit1,"contemporaneous", label.scale.equal=T, label.cex=1.2, posCol= "black", negCol="black", negDashed=T, fade=F)

summary(fit1)
centrality_auto(g1); centrality_auto(g2)
centralityPlot(g1, include=c("OutStrength", "InStrength", "Betweenness"), scale="raw"); centralityPlot(g2, include=c("Strength", "Betweenness"), scale="raw")

##Temporal and Contemporaneous Network Analyses for PA, NA, and Physical Activity
fit2 <- mlVAR(dataset, vars = c("Cheerful", "Content", "Sad", "Anxious", "ActivityT"), idvar = "Name", lags = 1)
g3 <- plot(fit2,"temporal",label.scale.equal=T, label.cex=1.2, posCol= "black", negCol="black", negDashed=T,fade=F)
g4 <-plot(fit2,"contemporaneous",label.scale.equal=T, label.cex=1.2, posCol= "black", negCol="black", negDashed=T,fade=F)

summary(fit2)
centrality_auto(g3); centrality_auto(g4)
centralityPlot(g3, include=c("OutStrength", "InStrength", "Betweenness"), scale="raw"); centralityPlot(g4, include=c("Strength", "Betweenness"), scale="raw")



##Permutation Tests
library(MKinfer)  #MKinfer has a Monte Carlo permutation T-Test
# Outstrength PA vs NA Temporal
OS1 <- centrality_auto(g1)$node.centrality$OutStrength
os <- data.frame(OS1,t = factor(c("PA", "PA", "NA", "NA")))
perm.t.test(OS1~t, data=os)

# Strength PA vs NA Contemporaneous
S1 <- centrality_auto(g2)$node.centrality$Strength
s <- data.frame(S1,t = factor(c("PA", "PA", "NA", "NA")))
perm.t.test(S1~t, data=s)


