library(tidyverse)
library(psych)
# library(NetworkComparisonTest)
library(networktools)
library(psychonetrics)
library(mgm)
library(bootnet)
library(psychTools)
library(glmnet)
library(qgraph)
library(lavaan)
library(lmerTest)
library(tseries)
library(mlVAR)

load('phq_imp.RData')
load('phq_imp_long.RData')
dat_w <- phq_imp             #wide
dat_l <- phq_imp_long        #long
rm(phq_imp, phq_imp_long)




# node redundancy ---------------------------------------------------------

corpcor::is.positive.definite(cor_auto(select(dat_l,phq1:phq9))) #true
gb <- goldbricker(select(dat_l,phq1:phq9), p = 0.05, method = "hittner2003", threshold=0.25, corMin=.50)
gb # no suggested reductions 

# assumption checks -------------------------------------------------------
# normality 
ks.test(dat_l$phq1, rnorm(length(dat_l$phq1))) # sig
ks.test(dat_l$phq2, rnorm(length(dat_l$phq2))) # sig
ks.test(dat_l$phq3, rnorm(length(dat_l$phq3))) # sig
ks.test(dat_l$phq4, rnorm(length(dat_l$phq4))) # sig
ks.test(dat_l$phq5, rnorm(length(dat_l$phq5))) # sig
ks.test(dat_l$phq6, rnorm(length(dat_l$phq6))) # sig
ks.test(dat_l$phq7, rnorm(length(dat_l$phq7))) # sig
ks.test(dat_l$phq8, rnorm(length(dat_l$phq8))) # sig
ks.test(dat_l$phq9, rnorm(length(dat_l$phq9))) # sig
#none are normal


# histograms
vars_list <- colnames(dplyr::select(dat_l, phq1:phq9))
for (i in vars_list) {
  hist(dat_l[, i], xlab = i, main = "", col = "lightblue", border = "white")
}

# skewness kurtosis
skew_kurt = data.frame()
for (v in vars_list){
  var_norm = c(moments::skewness(dat_l[[v]], na.rm = T), moments::kurtosis(dat_l[[v]], na.rm = T))
  skew_kurt = rbind(skew_kurt, var_norm)
}
colnames(skew_kurt) <- c("Skew", "Kurtosis")
rownames(skew_kurt) <- print(vars_list)
print(skew_kurt)

# transform variables
dat_log <- dat_l %>%
  mutate(across(.cols = all_of(vars_list), ~. + 1)) %>% # can't log transform if response = 0
  mutate(across(.cols = all_of(vars_list), log))

# skewness kurtosis
skew_kurt_log = data.frame()
for (v in vars_list){
  var_norm = c(moments::skewness(dat_log[[v]], na.rm = T), moments::kurtosis(dat_log[[v]], na.rm = T))
  skew_kurt_log = rbind(skew_kurt_log, var_norm)
}
colnames(skew_kurt_log) <- c("Skew", "Kurtosis")
rownames(skew_kurt_log) <- print(vars_list)
print(skew_kurt_log)

# Almost all kurtosis values still over 2

# Checking if data now normal with a normality test (here: shapiro-wilk test, more recommended: 
for (v in vars_list){
  hist(dat_log[[v]], main = paste0("Histogram: ", v))
  print(paste0(v, "  p-value: ", shapiro.test(dat_log[[v]])[2], 2))
}



# Gaussian transformation (from LambertW package; article: https://doi.org/10.1155/2015/909231)
library(LambertW)
data_gaussian <- dat_l %>%
  mutate(phq1 = Gaussianize(data = phq1, type = "s", method = "MLE")) %>%
  mutate(phq2 = Gaussianize(data = phq2, type = "s", method = "MLE")) %>%
  mutate(phq3 = Gaussianize(data = phq3, type = "s", method = "MLE")) %>%
  mutate(phq4 = Gaussianize(data = phq4, type = "s", method = "MLE")) %>%
  mutate(phq5 = Gaussianize(data = phq5, type = "s", method = "MLE")) %>%
  mutate(phq6 = Gaussianize(data = phq6, type = "s", method = "MLE")) %>%
  mutate(phq7 = Gaussianize(data = phq7, type = "s", method = "MLE")) %>%
  mutate(phq8 = Gaussianize(data = phq8, type = "hh", method = "MLE")) %>%
  mutate(phq9 = Gaussianize(data = phq9, type = "hh", method = "MLE")) %>%
  mutate(across(.cols = all_of(vars_list), as.numeric))


skew_kurt_cor = data.frame()
for (v in vars_list){
  var_norm = c(skewness(data_gaussian[[v]]), kurtosis(data_gaussian[[v]]))
  skew_kurt_cor = rbind(skew_kurt_cor, var_norm)
}
colnames(skew_kurt_cor) <- c("Skew", "Kurtosis")
rownames(skew_kurt_cor) <- print(vars_list)
print(skew_kurt_cor) # much better overall

# better but not all are within +/-2

# Checking if data now normal: 
for (v in vars_list){
  hist(data_gaussian[[v]], main = paste0("Histogram: ", v))
  print(paste0(v, "  p-value: ", shapiro.test(data_gaussian[[v]])[2]))
} 

# ehhh, move on with raw data




# check time trends -------------------------------------------------------

# using lmer
lmer(phq1 ~ week + (week | participant_id),data=dat_l,
           control = lmerControl(optimizer = "bobyqa")) %>% summary()



lmer(phq2 ~ week + (week | participant_id),data=dat_l,
           control = lmerControl(optimizer = "bobyqa")) %>% summary()











# via KPSS test
kpss.test(dat_l$phq1, null = c('Trend'))
#The p-value is 0.1. Since this value is not less than .05, we fail 
#to reject the null hypothesis of the KPSS test.
#This means we can assume that the time series is trend stationary.

kpss.test(dat_l$phq2, null = c('Trend'))
kpss.test(dat_l$phq3, null = c('Trend'))
kpss.test(dat_l$phq4, null = c('Trend'))
kpss.test(dat_l$phq5, null = c('Trend')) #non stationary
kpss.test(dat_l$phq6, null = c('Trend'))
kpss.test(dat_l$phq7, null = c('Trend'))
kpss.test(dat_l$phq8, null = c('Trend'))
kpss.test(dat_l$phq9, null = c('Trend'))


dat_l %>%
  filter(participant_id == "BLUE-00050") %>%
  pull(phq1) %>%
  kpss.test(null = c("Trend"))

# may have to test per participant?


####################################################################################
##############################      networks ---------------------------------------
####################################################################################
load('phq_imp.RData')
dat_w <- phq_imp             #wide
rm(phq_imp, phq_imp_long)




vars <- c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9")


dat <- dat_w[, 11:73]

# Sample column names
column_names <- colnames(dat)
# Extract the numeric values from the "X" part of the column names
numeric_suffix <- as.numeric(sub("phq(\\d+)_.*", "\\1", column_names))
# Sort column names based on the numeric suffix
sorted_column_names <- column_names[order(numeric_suffix)]
# Print the sorted column names
dat <- dat [, sorted_column_names]

desmat <- matrix(as.vector(colnames(dat)),9, byrow = T)


corpcor::is.positive.definite(cor_auto(dat)) #true


umod <- panelgvar(data = dat, vars = desmat, estimator= "FIML", missing="pairwise") %>% 
  runmodel()# run unpruned 
unprunedfit <- umod@fitmeasures
umod %>% parameters


pmod <- umod %>% 
  runmodel %>% 
  prune(alpha = 0.001, adjust = "none", recursive = FALSE)  

# Search strategy
if ("modelsearch" == "stepup"){
  pmod <- pmod %>%  stepup(alpha = 0.001, criterion = "bic")
} else if ("modelsearch" == "modelsearch"){
  pmod <- pmod %>%  modelsearch(prunealpha = 0.001, addalpha = 0.001)
} # takes too long wtf





# 
# save(pmod, file = "pmod.RData")
# save(umod, file = "umod.RData")
# save(unprunedfit, file = "unprunedfit.RData")




