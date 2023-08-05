library(tidyverse)
library(psych)
library(NetworkComparisonTest)
library(networktools)
library(psychonetrics)
library(mgm)
library(bootnet)
library(qgraph)
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
dat_l1 <- dat_l %>%
  mutate(week = case_when(week == 0 ~ 1,
                          week == 2 ~ 2,
                          week == 4 ~ 3,
                          week == 6 ~ 4,
                          week == 8 ~ 5,
                          week == 10 ~ 6,
                          week == 12 ~ 7))


vars <- c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9")


dat_l1 <- dat_l1 %>%
  select(c(participant_id, week, all_of(vars)))

mod <- ml_ts_lvgvar(dat_l1, beepvar = "week", idvar = "participant_id",
                    vars = vars,
                    estimator = "FIML",
                    standardize = "none")







mod <- mlVAR(dat_l1, idvar = "participant_id",
             beepvar = "week", 
             lags = 1,
             vars = vars)



temporal_plot <-plot(mod,"temporal", label.scale.equal=T, label.cex=1.2, posCol= "black", negCol="black", negDashed=T, fade=F)
contemp_plot <-plot(mod,"contemporaneous", label.scale.equal=T, label.cex=1.2, posCol= "black", negCol="black", negDashed=T, fade=F)





dat_l %>%
  select(all_of(vars)) %>%
  describe()














