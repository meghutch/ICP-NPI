bin_npi <- function(df) { 
  df$NPi_bins2 <- ifelse(df$minNPi < 3, "< 3",
                         ifelse(df$minNPi >= 3 & df$minNPi <= 5, "3-5",NA))
  
  df$NPi_bins3 <- ifelse(df$minNPi < 1, "< 1",
                         ifelse(df$minNPi >= 1 & df$minNPi < 3, "1-3",
                                ifelse(df$minNPi >= 3 & df$minNPi <= 5, "3-5",NA)))
  
  # set reference level
  df$NPi_bins2 <- as.factor(df$NPi_bins2)
  df$NPi_bins2 <- relevel(df$NPi_bins2,"3-5")
  
  # set reference level
  df$NPi_bins3 <- as.factor(df$NPi_bins3)
  df$NPi_bins3 <- relevel(df$NPi_bins3,"3-5")
  
  return(df)
}

tidy_results <- function(results) { 
  
  results <- results %>% 
    select(-effect) %>%
    mutate(estimate = round(estimate, 2),
           std.error = round(estimate, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 5))
  
  datatable(results, 
            options = list(dom = 't',ordering = F))
  
}

monitoring_timediff <- function(df) {
  df <- df %>% 
    mutate(timediff = difftime(Date_NPi, first_icp_date, unit = "hours"))
  
  df$timediff <- ifelse(df$timediff < 0, 0, df$timediff)
  
  df <- df %>% filter(timediff <= 96)
  
  return(df)
  
}

obs_overtime <- function(df) { 
  
  df <- df %>%
    mutate(sluggish = if_else(minNPi < 3, 1, 0)) %>%
    mutate(npi_one = if_else(minNPi < 1, 1, 0)) %>%
    mutate(hour = 0) %>%
    #mutate(hour = if_else(timediff == 0, 0, hour)) %>%
    mutate(hour = if_else(timediff >= 0 & timediff <= 12, 12, hour)) %>%
    mutate(hour = if_else(timediff > 12 & timediff <= 24, 24, hour)) %>%
    mutate(hour = if_else(timediff > 24 & timediff <= 36, 36, hour)) %>%
    mutate(hour = if_else(timediff > 36 & timediff <= 48, 48, hour)) %>%
    mutate(hour = if_else(timediff > 48 & timediff <= 60, 60, hour)) %>%
    mutate(hour = if_else(timediff > 60 & timediff <= 72, 72, hour)) %>%
    mutate(hour = if_else(timediff > 72 & timediff <= 84, 84, hour)) %>%
    mutate(hour = if_else(timediff > 84 & timediff <= 96, 96, hour)) %>%
    mutate(hour = if_else(timediff > 96 & timediff <= 108, 108, hour)) %>%
    mutate(hour = if_else(timediff > 108 & timediff <= 120, 120, hour)) %>%
    mutate(hour = if_else(timediff > 120 & timediff <= 132, 132, hour)) %>%
    mutate(hour = if_else(timediff > 132 & timediff <= 144, 144, hour)) %>%
    mutate(hour = if_else(timediff > 144 & timediff <= 156, 156, hour)) %>%
    mutate(hour = if_else(timediff > 156 & timediff <= 168, 168, hour)) %>%
    mutate(hour = if_else(timediff > 168 & timediff <= 180, 180, hour)) %>%
    mutate(hour = if_else(timediff > 180 & timediff <= 192, 192, hour)) %>%
    mutate(hour = if_else(timediff > 192, 193, hour)) %>%
    group_by(hour) %>%
    mutate(count = n()) %>%
    # number of spikes over time
    mutate(count_spikes = sum(spike)) %>%
    # count sluggish overtime
    mutate(count_sluggish = sum(sluggish)) %>%
    # count NPi of 0 overtime
    mutate(count_npi1 = sum(npi_one)) %>%
    # count unique number of patients
    group_by(hour) %>%
    mutate(count_pts = n_distinct(Study_ID)) %>%
    # group by 24 hours
    ungroup() %>%
    mutate(hour24 = 0) %>% 
    mutate(hour24 = if_else(hour == 12 | hour == 24, 24, hour24)) %>%
    mutate(hour24 = if_else(hour == 36 | hour == 48, 48, hour24)) %>%
    mutate(hour24 = if_else(hour == 60 | hour == 72, 72, hour24)) %>%
    mutate(hour24 = if_else(hour == 84 | hour == 96, 96, hour24)) %>%
    mutate(hour24 = if_else(hour == 108 | hour == 120, 120, hour24)) %>%
    mutate(hour24 = if_else(hour == 132 | hour == 144, 144, hour24)) %>%
    mutate(hour24 = if_else(hour == 156 | hour == 168, 168, hour24)) %>%
    mutate(hour24 = if_else(hour == 180 | hour == 192, 192, hour24)) %>%
    mutate(hour24 = if_else(timediff > 192, 193, hour24))
  
  return(df)
}

#' @param hours - hours from start of ICP monitoring to subset the data frame
#' @examples
#' sample_size(24) - The function will subset paired measurements that occurred after the first 24 hours from initiation of ICP monitoring
#' sample_size(0) - If hours = 0, we will not filter by time and all measurements will be used


sample_size <- function(df, hours) { 
  
  # filter measurements that occurred within x hours of ICP monitoring
  if(hours > 0) { 
    df <- df %>%
      filter(timediff > hours-24 & timediff <= hours)
  }
  
  # observations, patients
  m <- nrow(df)
  n = nrow(data.frame(unique(df$Study_ID))) 
  
  m_spike <- nrow(df %>% filter(spike == 1))
  
  # Observations < 1
  npi_less1 <- nrow(df %>% filter(minNPi < 1))
  npi_less1_n <- nrow(df %>% filter(minNPi < 1) %>% distinct(Study_ID))
  
  # Observations >= 1 & < 3
  npi_13 <- nrow(df %>% filter(minNPi >= 1 & minNPi < 3))
  npi_13_n <- nrow(df %>% filter(minNPi >= 1 & minNPi < 3) %>% distinct(Study_ID))
  
  # Observations >= 3 
  npi3 <- nrow(df %>% filter(minNPi >= 3))
  npi3_n <- nrow(df %>% filter(minNPi >= 3) %>% distinct(Study_ID))
  
  # spike observations
  npi_less1_spikes <- nrow(df %>% 
                             filter(minNPi < 1 & ICP > 20))
  
  npi_13_spikes <- nrow(df %>%
                          filter(minNPi >=1 & minNPi < 3) %>%
                          filter(ICP > 20))
  
  npi3_spikes <- nrow(df %>%
                        filter(minNPi >= 3) %>%
                        filter(ICP > 20))
  
  
  
  obs <- as.data.frame(cbind(m, n, m_spike, 
                             npi_less1_n, npi_less1, npi_less1_spikes,
                             npi_13_n, npi_13, npi_13_spikes,
                             npi3_n, npi3, npi3_spikes))
  
  
  colnames(obs) <- c("m", "n", "m_spike",  
                     "Pts_Bin1", "m_Bin1", "m_Spikes_Bin1",
                     "Pts_Bin2",  "m_Bin2", "m_Spikes_Bin2" ,
                     "Pts_Bin3", "m_Bin3", "m_Spikes_Bin3")
  
  return(obs)
}





#' @param hours - hours from start of ICP monitoring to subset the data frame
#' @param interval - time interval to subset the analysis
#' @param npi_bin - binned variable (NPi either binary, or divided into 3 bins)
#' @examples
#' lr_bin(24, 24, "NPi_bins2) - The function will examine the Association between NPi (binned into sluggish vs non-sluggish) and ICP within the first 24 hours from initiation of ICP monitoring and then at every 24 hours. 
#' lr_bin(24, 12, "NPi_bins3) - The function will examine the Association between NPi (binned into 3 categories: NPi < 1, NPi 1-3, and NPi 3-5) and ICP within the first 24 hours from initiation of ICP monitoring and then at every 12 hours
#' lr_bin(0, "NPi_bins3") - If hours = 0, we will not filter by time and all measurements will be used. In this case, we also look specifically at NPi binned into 3 categories (NPi < 1, NPi 1-3, and NPi 3-5)

lr_bin <- function(df, hours, interval, npi_bin) { 
  
  # filter measurements that occurred within x hours of ICP monitoring
  if(hours > 0) { 
    df <- df %>%
      filter(timediff > hours-interval & timediff <= hours)
  }
  
  lr_mod <- glmer(spike ~ df[[npi_bin]] + (1|Study_ID), 
                  family = "binomial", df)
  
  if (npi_bin == "NPi_bins2") {
    
    # Odds ratio
    odds_Bin1 <- round(exp(summary(lr_mod)$coef[2,1]), 2) 
    
    # p-values
    p_Bin1 <- round(summary(lr_mod)$coef[2,4], 5) 
    
    # confidence intervals
    ci <- round(exp(confint(lr_mod, method = "Wald")), 2)
    ci_Bin1 <- t(as.data.frame(ci[3, 1:2]))
    
    results <- as.data.frame(cbind(odds_Bin1, ci_Bin1, p_Bin1))
    rownames(results) <- paste(hours)
    
    colnames(results) <- c("OR", "CI (2.5%)", "CI (97.5%)","p_value")
    
  } else {
    
    # Odds ratio
    odds_Bin1 <- round(exp(summary(lr_mod)$coef[2,1]), 2) 
    odds_Bin2 <- round(exp(summary(lr_mod)$coef[3,1]), 2) 
    
    # p-values
    p_Bin1 <- round(summary(lr_mod)$coef[2,4], 5)
    p_Bin2 <- round(summary(lr_mod)$coef[3,4], 5) 
    
    # confidence intervals
    ci <- round(exp(confint(lr_mod, method = "Wald")), 2)
    ci_Bin1 <- t(as.data.frame(ci[3, 1:2]))
    ci_Bin2 <- t(as.data.frame(ci[4, 1:2]))
    
    results <- as.data.frame(cbind(odds_Bin1, ci_Bin1, p_Bin1, odds_Bin2, ci_Bin2, p_Bin2))
    #rownames(results) <- paste(analysis_label)
    rownames(results) <- paste(hours)
    
    colnames(results) <- c("OR", "CI (2.5%)", "CI (97.5%)","p_value", 
                           "OR_Bin2", "CI (2.5%)_Bin2", "CI (97.5%)_Bin2","p_Bin2")
    
  }
  
  # abstract random effects
  re <- as.data.frame(ranef(lr_mod))
  re <- re %>% 
    select(grp, condval, condsd) %>%
    arrange(desc(condval)) %>%
    mutate(time = paste0(hours))
  colnames(re)[1] <- "Study_ID"
  
  lr_results <- list(results, re)
  
  return(lr_results)
  
}

# EVD Drainage
evd_drainage <- function(df) { 
  
  df <- left_join(df, evd, by = 'Study_ID') %>%
    mutate(timediff_icp = difftime(Date_EVD, Date_ICP, unit = "hours"),
           timediff_npi = difftime(Date_EVD, Date_NPi, unit = "hours"),
           EVD_ICP = if_else(timediff_icp >= -1 & timediff_icp <= 0, 1, 0),
           EVD_NPi = if_else(timediff_npi >= -1 & timediff_npi <= 0, 1, 0),
           # sometimes there are NA's if patient does not have EVD drainage
           EVD_ICP = if_else(is.na(EVD_ICP), 0, EVD_ICP),
           EVD_NPi = if_else(is.na(EVD_NPi), 0, EVD_NPi),
           EVD_Both = if_else(EVD_ICP == 1 & EVD_NPi == 1, 1, 0),
           EVD_Either = if_else(EVD_ICP == 1 | EVD_NPi == 1, 1, 0)) %>% 
    group_by(Study_ID, Date_ICP, Date_NPi) %>% 
    mutate(EVD_ICP = max(EVD_ICP),
           EVD_NPi = max(EVD_NPi),
           EVD_Both = max(EVD_Both),
           EVD_Either = max(EVD_Either)) %>%
    distinct(Study_ID,Date_NPi, Date_ICP, minNPi, ICP, first_icp_date, spike, Diagnosis,
             EVD_ICP, EVD_NPi, EVD_Both, EVD_Either) %>% 
    ungroup()
  
  return(df)
  
  }

## Surgeries

hemi_crani <- function(df) { 
 
  df <- df %>% 
    left_join(., demo %>% select(Study_ID, DateSurgery), by = "Study_ID") %>% 
  mutate(hemi_crani_before = if_else(DateSurgery < Date_NPi, 1, 0),
         hemi_crani_before = if_else(is.na(hemi_crani_before), 0, hemi_crani_before))
  
  return(df)
  
  }


sensitivity_time_function <- function(df, time) {

  sens <- df %>% 
    filter(hour24 == time) %>%
    mutate(TP = if_else(minNPi < 3 & ICP > 22, 1, 0),
           FP = if_else(minNPi < 3 & ICP <= 22, 1, 0),
           TN = if_else(minNPi >= 3 & ICP <= 22, 1, 0),
           FN = if_else(minNPi >= 3 & ICP >= 22, 1, 0))
  
  sensitivity = sum(sens$TP)/sum(sens$TP + sens$FN)
  specificity = sum(sens$TN)/sum(sens$TN + sens$FP)
  ppv = sum(sens$TP)/sum(sens$TP + sens$FP)
  npv = sum(sens$TN)/sum(sens$TN + sens$FN)
  
  sensitivity_results <- cbind(sensitivity, 
                               specificity,
                               ppv, 
                               npv)
  
  sensitivity_results <- sensitivity_results %>% 
    unlist() %>%
    data.frame()
  
  sensitivity_results$hour24 <- paste(time)
  
  return(sensitivity_results)
  
}
