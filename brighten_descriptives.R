library(tidyverse)








# stationarity check ------------------------------------------------------

# Adding within-person means to the data
# (In case need to detrend data, add the WP mean to the residuals so there's no mean level = 0)
data_corrected <- dat_l %>%
  group_by(participant_id) %>% 
  dplyr::mutate(across(.cols = all_of(vars_list), ~ mean(.x, na.rm=TRUE),
                       .names = "{.col}.wp_mean"))



esm_before_stationary <- data_corrected
esm_after_detrend <- data_corrected
data_stat <- data_corrected




{
  bonferroni_n <- 10 
  kpss_tests <- list()
  kpss_p <- list()
  
  # loop over each variable
  for(y in names(subset(data_stat, select = vars_list))){
    # test stationarity
    kpss_p_values <- c()
    lm_trends <- list()
    lm_summary <- list()
    
    nid <- 1
    
    #KPSS Test for Trend Stationarity
    # https://nwfsc-timeseries.github.io/atsa-labs/sec-boxjenkins-kpss.html
    # H0: trend stationarity 
    # H1: not trend stationary
    for(i in unique(data_stat$participant_id))
    {
      # test whether there is a significant trend in the answers of the subject so we can adapt the null hypothesis of the kpss test
      Data <- data_stat[data_stat$participant_id == i,]
      # first to avoid errors, test whether mean of participant answers = 0, sd > 0, and all answers aren't NA 
      if(mean(Data[[y]], na.rm = TRUE) > 0 & sd(Data[[y]], na.rm = TRUE) > 0 & !all(is.na(Data[[y]]))){
        lm_trends[[y]] <- lm(as.formula(paste0(y," ~ n_obs")), data = Data, na.action=na.omit) 
        lm_summary[[y]] <- summary(lm_trends[[y]])
        
        # if there is a trend, then we test for trend stationarity
        # lm_summary[[y]]$coefficients[2,4] -> gives the p.value of the regression
        if(lm_summary[[y]]$coefficients[2,4]< 0.05/bonferroni_n ){
          print(paste0("Subject ", i, " has a trend (will check if it's nonstationary)")) 
          
          kpss_tests[[y]][[nid]] <- tseries::kpss.test(na.exclude(data_stat[[y]][data_stat$participant_id == i]), lshort = TRUE, null = "Trend")
          kpss_p_values <- c(kpss_p_values, kpss_tests[[y]][[nid]]$p.value)
        } else {
          # otherwise
          print(paste0("Subject ", i, " has no trend for variable ", y)) 
          kpss_tests[[y]][[nid]] <- tseries::kpss.test(na.exclude(data_stat[[y]][data_stat$participant_id == i]), lshort = TRUE, null = "Level")
          kpss_p_values <- c(kpss_p_values, kpss_tests[[y]][[nid]]$p.value)
        }
        
      } else{
        print(paste0("Subject ", i, " has a mean or SD of 0 for the variable ", y))
        
        kpss_tests[[y]][[nid]] <- tseries::kpss.test(na.exclude(data_stat[[y]][data_stat$participant_id == i]), lshort = TRUE, null = "Level")
        kpss_p_values <- c(kpss_p_values, kpss_tests[[y]][[nid]]$p.value)
      }
      
      
      
      nid <- nid + 1
    }
    # put them together in matrix
    kpss_p[[y]]  <- cbind(unique(data$participant_id),kpss_p_values )
    print(paste0(y, " : ", sum(kpss_p[[y]] [,2] < 0.05/bonferroni_n), " participants with non stationary data"))
    
    
    #_______________________________________________________
    # DETREND for participants that have non stationary data
    
    detrendslm_trends <- list()
    detrendslm_summary <- list()
    detrendslm_trends1 <- list()
    
    for(i in 1:length(unique(kpss_p[[y]][,1])[kpss_p[[y]][,2] < 0.05/bonferroni_n]))
    {
      
      # only do this if there are participants with non-stationary data for that particular variable: 
      if((sum(kpss_p[[y]] [,2] < 0.05/bonferroni_n) > 0)) { 
        
        Data1 <- data_stat[data_stat$participant_id == unique(kpss_p[[y]][,1])[kpss_p[[y]][,2] < 0.05/bonferroni_n][i],]
        
        detrendslm_trends[[y]] <- lm(as.formula(paste0(y," ~ n_obs")), data = Data1)
        plot(Data1$n_obs,Data1[[y]], type="b", ylim=c(0,100), main = "before")
        abline(detrendslm_trends[[y]], col="red")
        print(paste0("Looping through subject #", i, " for variable ", y))
        
        
        Data1[[y]][!is.na(Data1[[y]])] <- residuals(detrendslm_trends[[y]])
        
        detrendslm_trends1[[y]] <- lm(as.formula(paste0(y," ~ n_obs")), data = Data1)
        
        plot(Data1$n_obs,Data1[[y]], type="b", ylim=c(-100,100), main = "after")
        abline(detrendslm_trends1[[y]], col="red")
        
        replace <- as.data.frame(data_stat[data_stat$participant_id == unique(kpss_p[[y]][,1])[kpss_p[[y]][,2] < 0.05/bonferroni_n][i], y])
        
        replace[!is.na(replace)] <- Data1[[y]][!is.na(Data1[[y]])]
        replace <- replace + Data1[[paste0(y,".wp_mean")]][kpss_p[[y]][,2] < 0.05/bonferroni_n][i]
        
        length(Data1$n_obs)
        
        plot(Data1$n_obs, replace[[y]], type="b", ylim=c(-100,100), main = "after + mean")
        abline(detrendslm_trends1[[y]], col="red")
        
        
        esm_after_detrend[esm_after_detrend$participant_id == unique(kpss_p[[y]][,1])[kpss_p[[y]][,2] < 0.05/bonferroni_n][i], y] <- as.integer(replace[[y]])
        print(past0("Detrended data for participant "))
      } else {
        print(paste0("No participants needed detrending for variable ", y))
      }
    }
    
  }
  
}



