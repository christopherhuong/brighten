
# code to run for  --------------------------------------------------------
#Internalizing Symptoms, Wellbeing and Correlates in Adolescence: A Multiverse
#Exploration via Cross-lagged Panel Network Models 
#synthetic gaussian data (S4) is provided to check the code but should not be
#considered for exploration or replication with models running more quickly and fitting better than actual data
#due to the simulation method (done in psychonetrics from a subset of fitted models from real data)

#session info: 
#R version 3.6.1 (2019-07-05)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS Catalina 10.15.5

#Matrix products: default
#BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

#locale:
#  [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
 # [1] lavaan_0.6-5        qgraph_1.6.5        gdata_2.18.0        psychonetrics_0.7.1 frequency_0.3.1    
#[6] ggplot2_3.3.0       DT_0.12             knitr_1.28          rmarkdown_2.1       psych_1.9.12.31    
#[11] tidyr_1.0.2         dplyr_0.8.5        

#loaded via a namespace (and not attached):
 # [1] nlme_3.1-140        RColorBrewer_1.1-2  numDeriv_2016.8-1.1 tools_3.6.1         backports_1.1.5    
#[6] R6_2.5.0            rpart_4.1-15        d3Network_0.5.2.1   Hmisc_4.4-0         mgcv_1.8-28        
#[11] colorspace_1.4-1    nnet_7.3-12         withr_2.3.0         tidyselect_1.0.0    gridExtra_2.3      
#[16] mnormt_1.5-6        compiler_3.6.1      fdrtool_1.2.15      cli_2.2.0           htmlTable_1.13.3   
#[21] scales_1.1.1        checkmate_2.0.0     mvtnorm_1.1-0       pbapply_1.4-2       stringr_1.4.0      
#[26] digest_0.6.27       pbivnorm_0.6.0      foreign_0.8-71      minqa_1.2.4         base64enc_0.1-3    
#[31] jpeg_0.1-8.1        pkgconfig_2.0.3     htmltools_0.4.0     lme4_1.1-21         GA_3.2             
#[36] htmlwidgets_1.5.1   rlang_0.4.9         rstudioapi_0.13     huge_1.3.4.1        combinat_0.0-8     
#[41] gtools_3.8.2        acepack_1.4.1       magrittr_2.0.1      Formula_1.2-3       Matrix_1.2-17      
#[46] Rcpp_1.0.5          munsell_0.5.0       fansi_0.4.1         abind_1.4-5         lifecycle_0.2.0    
#[51] ucminf_1.1-4        yaml_2.2.1          stringi_1.5.3       whisker_0.4         MASS_7.3-51.4      
#[56] plyr_1.8.6          matrixcalc_1.0-3    grid_3.6.1          parallel_3.6.1      crayon_1.3.4       
#[61] lattice_0.20-38     IsingSampler_0.2.1  splines_3.6.1       pillar_1.4.7        igraph_1.2.5       
#[66] boot_1.3-22         rjson_0.2.20        VCA_1.4.2           corpcor_1.6.9       BDgraph_2.62       
#[71] reshape2_1.4.3      codetools_0.2-16    stats4_3.6.1        glue_1.4.2          evaluate_0.14      
#[76] latticeExtra_0.6-29 data.table_1.12.8   png_0.1-7           vctrs_0.3.5         nloptr_1.2.2.1     
#[81] foreach_1.5.0       gtable_0.3.0        purrr_0.3.4         assertthat_0.2.1    xfun_0.12          
#[86] survival_3.1-11     glasso_1.11         tibble_3.0.4        optimx_2020-2.2     iterators_1.0.12   
#[91] cluster_2.1.0       ellipsis_0.3.1  

# libraries ----------------------------------------------------

library(dplyr)
library(tidyr)
library(psych)
library(frequency)
library(psychonetrics)
library(gdata)
library(qgraph)
library(lavaan)

# load data: --------------------------------------------------------------

Data<- read.csv("examplecodes/black2021/S4FakeData.csv", row.names = 1L, check.names = FALSE)


# code from actual analysis -----------------------------------------------
#Loop is set for a single dataframe with comments due to computation time.
#Code for all 16 datasets is provided and can be run by creating removing the comments from dflist

optimism1<- c("WB1_1", "WB2_1", "WB3_1")
optimism2<- c("PSS1_3", "PSS2_3", "PSS3_3")
useful<- c("WB1_2", "WB2_2", "WB3_2")
relaxed<- c("WB1_3", "WB2_3", "WB3_3")
problem1<- c("WB1_4", "WB2_4", "WB3_4")
problem2<- c("PSS1_2", "PSS2_2", "PSS3_2")
think<- c("WB1_5", "WB2_5", "WB3_5")
close<- c("WB1_6", "WB2_6", "WB3_6")
mind1<- c("WB1_7", "WB2_7", "WB3_7")
mind2<- c("ER1_2or", "ER2_2or", "ER3_2or")
somatic<- c("SDQ1_3", "SDQ2_3", "SDQ3_3")
worry<- c("SDQ1_8", "SDQ2_8", "SDQ3_8")
unhappy<- c("SDQ1_13", "SDQ2_13", "SDQ3_13")
nervous<- c("SDQ1_16", "SDQ2_16", "SDQ3_16")
scared<- c("SDQ1_24", "SDQ2_24", "SDQ3_24")
restless<- c("SDQ1_2", "SDQ2_2", "SDQ3_2")
withdrawn<-c("SDQ1_6", "SDQ2_6", "SDQ3_6")
bully<- c("SDQ1_19", "SDQ2_19", "SDQ3_19")
distracted1<- c("SDQ1_15", "SDQ2_15", "SDQ3_15")
distracted2<- c("SDQ1_25", "SDQ2_25", "SDQ3_25")
stress<- c("ER1_3", "ER2_3", "ER3_3")
feelings<- c("ER1_1or", "ER2_1or", "ER3_1or")
home<- c("HOME1_4", "HOME2_4", "HOME3_4")
school<- c("SCH1_1", "SCH2_1", "SCH3_1")
peer<- c("PEER1_8", "PEER2_8", "PEER3_8")
help<- c("PROB1_1", "PROB2_1", "PROB3_1")
gender<- "Gender"
coreItems<- c(useful, relaxed, think, close, somatic, worry, unhappy, nervous, scared, restless, withdrawn, 
              bully, stress, feelings, home, school, peer, help)


df1<- Data[,c(gender,coreItems, optimism1, problem1, mind1, distracted1)]
df2<- Data[, c(gender, coreItems, optimism1, problem1, mind1, distracted2)]
df3<- Data[, c(gender, coreItems, optimism1, problem1, mind2, distracted1)]
df4<- Data[, c(gender, coreItems, optimism1, problem1, mind2, distracted2)]
df5<- Data[, c(gender, coreItems, optimism1, problem2, mind1, distracted1)]
df6<- Data[, c(gender, coreItems, optimism1, problem2, mind1, distracted2)]
df7<- Data[, c(gender, coreItems, optimism1, problem2, mind2, distracted1)]
df8<- Data[, c(gender, coreItems, optimism1, problem2, mind2, distracted2)]
df9<- Data[, c(gender, coreItems, optimism2, problem1, mind1, distracted1)]
df10<- Data[, c(gender, coreItems, optimism2, problem1, mind1, distracted2)]
df11<- Data[, c(gender, coreItems, optimism2, problem1, mind2, distracted1)]
df12<- Data[, c(gender, coreItems, optimism2, problem1, mind2, distracted2)]
df13<- Data[, c(gender, coreItems, optimism2, problem2, mind1, distracted1)]
df14<- Data[, c(gender, coreItems, optimism2, problem2, mind1, distracted2)]
df15<- Data[, c(gender, coreItems, optimism2, problem2, mind2, distracted1)]
df16<- Data[, c(gender, coreItems, optimism2, problem2, mind2, distracted2)]


names<- c("useful", "relaxed", "think", "close", "somatic", "worry", "unhappy", "nervous", "scared", "restless", "withdrawn", 
          "bully", "stress", "feelings", "home", "school", "peer", "help", "optimism", "problem", "mind", "distracted")

# multiverse helper functions ---------------------------------------------

namedTemporal<- function(model, vrbls){
  temporal<- getmatrix(model, "PDC")
  rownames(temporal)<- vrbls
  colnames(temporal)<- vrbls
  return(temporal)
}

namedContemporaneous<- function(model, vrbls){
  contemporaneous<- getmatrix(model, "omega_zeta_within")
  rownames(contemporaneous)<- vrbls
  colnames(contemporaneous)<- vrbls
  return(contemporaneous)
}

namedBetween<- function(model, vrbls){
  between<- getmatrix(model, "omega_zeta_between")
  rownames(between)<- vrbls
  colnames(between)<- vrbls
  return(between)
}



temporalDescript<- function(temporal){
  vector<- as.vector(temporal)
  estimates<- vector[vector !=0]
  descriptives<- describe(estimates)
  return(descriptives)
}



crossNetDescript<- function(network){
  vector<- as.vector(lowerTriangle(network))
  estimates<- vector[vector !=0]
  descriptives<- describe(estimates)
  return(descriptives)
}


# loop outputs ------------------------------------------------------------

#about data
vbls<- list()

#unpruned
unprunedfit<- list()

#pruned
prunedfit<- list()
prunedparameters<- list()

prunedtemporal<- list()
prunedcontemporaneous<- list()
prunedbetween<- list()
prunedtemp.desc<- list()
prunedcont.desc<- list()
prunedbet.desc<- list()

#pruned centrality
PDCcentP<- list()
contCentP<- list()
betCentP<- list()

#pruned invariance
prunedconfig.fit<- list()
prunedconstrained.fit<- list()
prunedcomparison<- list()
prunedICs<- list()

#stepup
stepupfit<- list()
stepupparameters<- list()

stepuptemporal<- list()
stepupcontemporaneous<- list()
stepupbetween<- list()
stepuptemp.desc<- list()
stepupcont.desc<- list()
stepupbet.desc<- list()

#stepup centrality
PDCcentS<- list()
contCentS<- list()
betCentS<- list()

#stepup invariance
stepupconfig.fit<- list()
stepupconstrained.fit<- list()
stepupcomparison<- list()
stepupICs<- list()

dfList<- list(df1
#, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16
)#shorter
#lists (e.g. df1-df3) were used in reality due to compuation time. 

# FIML loop ---------------------------------------------------------------



for (i in 1:length(dfList)){
  #data, variables and design matrix prep
  df<- dfList[[i]]
  dfNG<- df[, 2:67] #drop gender to make design matrix in next step
  des<- matrix(as.vector(colnames(dfNG)),22, byrow = T) #design matrix
  vbls[[i]]<- colnames(dfNG[, c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64)])
  
  #unpruned model, get fit
  umod<- panelgvar(dfNG, vars = des, estimator= "FIML", missing="pairwise")%>% runmodel()# run unpruned 
  unprunedfit[[i]]<- umod@fitmeasures
  
  #pruned model, get fit, parameters, and matrices
  pruned<- umod%>%prune(recursive= T)
  prunedfit[[i]]<- pruned@fitmeasures
  prunedparameters[[i]]<- pruned%>% parameters()
  
  prunedtemporal[[i]]<- namedTemporal(pruned, names)
  prunedcontemporaneous[[i]]<- namedContemporaneous(pruned, names)
  prunedbetween[[i]]<- namedBetween(pruned, names)
  prunedtemp.desc[[i]]<- temporalDescript(prunedtemporal[[i]])
  prunedcont.desc[[i]]<- crossNetDescript(prunedcontemporaneous[[i]])
  prunedbet.desc[[i]]<- crossNetDescript(prunedbetween[[i]])
  
  #pruned centrality
  PDCcentp<- centrality_auto(prunedtemporal[[i]])
  PDCcentP[[i]]<- PDCcentp$node.centrality
  contCentp<- centrality_auto(getmatrix(pruned, "omega_zeta_within"))
  contCentP[[i]]<- contCentp$node.centrality
  betCentp<- centrality_auto(getmatrix(pruned, "omega_zeta_between"))
  betCentP[[i]]<- betCentp$node.centrality
  
  #pruned adjacency matrices
  prune.adjacencyT <- 1*(getmatrix(pruned, "beta")!=0)
  prune.adjacencyB<- 1*(getmatrix(pruned, "omega_zeta_between")!=0)
  prune.adjacencyC<- 1*(getmatrix(pruned, "omega_zeta_within")!=0)
  
  # pruned invariance
  genderComp<- df%>% drop_na(Gender)
  
  Pconfigural<- panelgvar(genderComp, vars= des, estimator= "FIML", missing= "pairwise", groups= "Gender",
                          beta= prune.adjacencyT, omega_zeta_within= prune.adjacencyC, omega_zeta_between= prune.adjacencyB)%>% runmodel
  prunedconfig.fit[[i]]<- Pconfigural@fitmeasures
  
  Pconstrained<- Pconfigural%>% groupequal("beta")%>% groupequal("omega_zeta_within")%>% groupequal("omega_zeta_between")%>% runmodel()
  prunedconstrained.fit[[i]]<- Pconstrained@fitmeasures
  
  prunedcomparison[[i]]<- psychonetrics::compare(Pconfigural= Pconfigural, Pconstrained= Pconstrained)
  
  aicDif<- Pconfigural@fitmeasures$aic.ll-Pconstrained@fitmeasures$aic.ll
  bicDif<- Pconfigural@fitmeasures$bic-Pconstrained@fitmeasures$bic
  prunedICs[[i]]<- cbind(aicDif, bicDif)
  
  #stepup model, get fit, parameters and matrices
  stepup<- pruned%>% stepup()
  stepupfit[[i]]<- stepup@fitmeasures
  stepupparameters[[i]]<- stepup%>% parameters()
  
  stepuptemporal[[i]]<- namedTemporal(stepup, names)
  stepupcontemporaneous[[i]]<- namedContemporaneous(stepup, names)
  stepupbetween[[i]]<- namedBetween(stepup, names)
  stepuptemp.desc[[i]]<- temporalDescript(stepuptemporal[[i]])
  stepupcont.desc[[i]]<- crossNetDescript(stepupcontemporaneous[[i]])
  stepupbet.desc[[i]]<- crossNetDescript(stepupbetween[[i]])
  
  #stepup centrality
  PDCcents<- centrality_auto(stepuptemporal[[i]])
  PDCcentS[[i]]<- PDCcents$node.centrality
  contCents<- centrality_auto(getmatrix(stepup, "omega_zeta_within"))
  contCentS[[i]]<- contCents$node.centrality
  betCents<- centrality_auto(getmatrix(stepup, "omega_zeta_between"))
  betCentS[[i]]<- betCents$node.centrality
  
  
  #stepup adjacency matrices
  stepupadjacencyT <- 1*(getmatrix(stepup, "beta")!=0)
  stepupadjacencyB<- 1*(getmatrix(stepup, "omega_zeta_between")!=0)
  stepupadjacencyC<- 1*(getmatrix(stepup, "omega_zeta_within")!=0)
  
  # pruned invariance
  Sconfigural<- panelgvar(genderComp, vars= des, estimator= "FIML", missing= "pairwise", groups= "Gender",
                          beta= stepupadjacencyT, omega_zeta_within= stepupadjacencyC, omega_zeta_between= stepupadjacencyB)%>% runmodel
  stepupconfig.fit[[i]]<- Sconfigural@fitmeasures
  
  Sconstrained<- Sconfigural%>% groupequal("beta")%>% groupequal("omega_zeta_within")%>% groupequal("omega_zeta_between")%>% runmodel()
  stepupconstrained.fit[[i]]<- Sconstrained@fitmeasures
  
  stepupcomparison<- psychonetrics::compare(Sconfigural= Sconfigural, Sconstrained= Sconstrained)
  
  SaicDif<- Sconfigural@fitmeasures$aic.ll-Sconstrained@fitmeasures$aic.ll
  SbicDif<- Sconfigural@fitmeasures$bic-Sconstrained@fitmeasures$bic
  stepupICs[[i]]<- cbind(SaicDif, SbicDif)
}





# idk ---------------------------------------------------------------------




umod <- panelgvar(dfNG, vars = des, estimator= "FIML", missing="pairwise") %>% 
  runmodel()# run unpruned 
unprunedfit <- umod@fitmeasures
