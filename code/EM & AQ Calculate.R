#### load the data ####

require(dplyr)
require(tidyr)
require(broom)
require(tidyverse)
require(TTR)
require(zoo)


ifrs_data_cleaned <- as.data.frame(read.csv("ifrs_cleaned.csv", fileEncoding = "UTF-8-BOM"))

#### Add AQ & EM Variables ####


# Check if balance sheet years are consecutive  

ifrs_data_cleaned$yyyy <- as.numeric(substr(ifrs_data_cleaned$yyyymm,1,4))

ifrs_data_cleaned <- as.data.frame(ifrs_data_cleaned %>% group_by(company) %>% mutate(diff_year = c(1,diff(yyyy)),
                                                   tag_of_section = c(cumsum(diff_year-1)))
                 )


ifrs_data_tse  <- ifrs_data_cleaned %>% filter(tse_otc == "TSE")
ifrs_data_otc <- ifrs_data_cleaned %>% filter(tse_otc == "OTC")



ifrs_data_tse$STDEBT <- ifelse(is.na(ifrs_data_tse$short_term_loan),0,ifrs_data_tse$short_term_loan) + 
  ifelse(is.na(ifrs_data_tse$account_payable),0,ifrs_data_tse$account_payable) + 
  ifelse(is.na(ifrs_data_tse$current_portion_of_long_term_debt),0,ifrs_data_tse$current_portion_of_long_term_debt)

ifrs_data_otc$STDEBT <- ifelse(is.na(ifrs_data_otc$short_term_loan),0,ifrs_data_otc$short_term_loan) + 
  ifelse(is.na(ifrs_data_otc$account_payable),0,ifrs_data_otc$account_payable) + 
  ifelse(is.na(ifrs_data_otc$current_portion_of_long_term_debt),0,ifrs_data_otc$current_portion_of_long_term_debt)



ifrs_data_tse$ROA_ebitd <- ifrs_data_tse$ROA_ebitd/100
ifrs_data_tse$ROA_ebid <- ifrs_data_tse$ROA_ebid/100
ifrs_data_tse$ROA_ebi <- ifrs_data_tse$ROA_ebi/100

ifrs_data_otc$ROA_ebitd <- ifrs_data_otc$ROA_ebitd/100
ifrs_data_otc$ROA_ebid <- ifrs_data_otc$ROA_ebid/100
ifrs_data_otc$ROA_ebi <- ifrs_data_otc$ROA_ebi/100


add_AQ_EM_columns <- function(data){
  data %>% group_by(company,tag_of_section) %>% arrange(yyyymm) %>% filter(length(company) >= 3) %>% mutate(   ## AQ Variables
    delta_CA = c(NA,diff(current_assets)),
    delta_CL = c(NA,diff(current_liability)),
    delta_cash = c(NA,diff(cash)),
    delta_STDEBT = c(NA,diff(STDEBT)),
    
    accruals_t = delta_CA-delta_CL-delta_cash+delta_STDEBT,
    accruals_t_minus_1 = c(NA,accruals_t[-length(company)]),
    accruals_t_minus_2 = c(rep(NA,2),accruals_t[-c(length(company),length(company)-1)]),
    
    CFO_t = operating_cf,
    CFO_t_minus_1 = c(NA,operating_cf[-length(company)]),
    CFO_t_minus_2 = c(rep(NA,2),operating_cf[-c(length(company),length(company)-1)]),
    
    TA_t = total_asset,
    TA_t_minus_1 = c(NA,total_asset[-length(company)]),
    
    earning_t = ebit,
    earning_t_minus_1 = c(NA,ebit[-length(company)]),
    earning_t_minus_2 = c(rep(NA,2),ebit[-c(length(company),length(company)-1)]),
    earning_t_minus_3 = c(rep(NA,3),ebit[-c(length(company),length(company)-1,length(company)-2)]),
    
    
    operating_earning_t = operating_profit,
    operating_earning_t_minus_1 = c(NA,operating_profit[-length(company)]),
    operating_earning_t_minus_2 = c(rep(NA,2),operating_profit[-c(length(company),length(company)-1)]),
    operating_earning_t_minus_3 = c(rep(NA,3),operating_profit[-c(length(company),length(company)-1,length(company)-2)]),
    
    ## EM Vairables
    EM_total_accruals = total_profit-operating_cf,
    delta_REV = c(NA, diff(gross_sales)),
    delta_REC = c(NA, diff(account_receivable)),
    delta_REV_minus_delta_REC = delta_REV-delta_REC,
    
    PPE_t = ppe,
    ROA_t_minus_1 = c(NA, ROA_ebi[-length(company)])
    
  )
  
}

ifrs_data_tse_add_AQ_EM_columns <- as.data.frame(add_AQ_EM_columns(ifrs_data_tse))
ifrs_data_otc_add_AQ_EM_columns <- as.data.frame(add_AQ_EM_columns(ifrs_data_otc))





#### AQ Measure ####

#### DD Measure (Standard Deviation of Accruals Residuals) ####

DD_measure_resid <- function(data,is_all_sectors){ 
  
  # Industry-level = 0 while all sectors = 1
  grp <- quos(yyyymm, tse_industry_code)[[1:(is_all_sectors+1)]] 
  
  data %>% 
    drop_na(accruals_t, CFO_t, CFO_t_minus_1, CFO_t_minus_2, TA_t_minus_1) %>% 
    
    group_by(UQ(grp)) %>%
    
    mutate(y = accruals_t/TA_t_minus_1,
           x1 = CFO_t_minus_1/TA_t_minus_1,
           x2 = CFO_t/TA_t_minus_1,
           x3 = CFO_t_minus_2/TA_t_minus_1,
           DD_resid  = residuals(lm(y ~ x1 + x2 + x3, data = .))
    ) %>% select(company,company_abbreviation,yyyymm,tse_industry_name,tse_industry_code,DD_resid,tag_of_section)

  
}

# TSE industry level (DD Measure Residual)
DD_measure_resid_tse <- as.data.frame(DD_measure_resid(ifrs_data_tse_add_AQ_EM_columns,0))

# TSE all company (DD Measure Residual)
DD_measure_resid_tse_all_sectors <- as.data.frame(DD_measure_resid(ifrs_data_tse_add_AQ_EM_columns,1)) 


# OTC industry level (DD Measure Residual)
DD_measure_resid_otc <- as.data.frame(DD_measure_resid(ifrs_data_otc_add_AQ_EM_columns,0))

# OTC all company (DD Measure Residual)
DD_measure_resid_otc_all_sectors <- as.data.frame(DD_measure_resid(ifrs_data_otc_add_AQ_EM_columns,1))




DD_measure <- function(data){
  
  data %>% group_by(company,tag_of_section) %>% arrange(yyyymm) %>% 
    mutate(#DD_resid_t_minus_1 = c(NA, DD_resid[-length(company)]),
           #DD_resid_t_plus_1 = c(DD_resid[-1],NA),
           #DD_measure = apply(cbind(DD_resid_t_minus_1,DD_resid,DD_resid_t_plus_1),1,sd)
           
           DD_measure = if(length(company)>=3){ c(rep(NA,2),rollapply(DD_resid,3,sd))}else{
             rep(NA,length(company))
           }
           
           ) 
  
}

# TSE "company itself" level  (DD Measure)
DD_measure_tse <- as.data.frame(DD_measure(DD_measure_resid_tse))

# OTC "company itself" (DD Measure)
DD_measure_otc <- as.data.frame(DD_measure(DD_measure_resid_otc))





#### Standard Deviation of Accruals ####

SD_accruals <- function(data){
  data %>% group_by(company,tag_of_section) %>% arrange(yyyymm) %>% 
    mutate(#accrual_ta_t_minus_1 = accruals_t_minus_1/TA_t,
           #accrual_ta_t = accruals_t/TA_t,
           #accrual_ta_t_plus_1 = accruals_t_plus_1/TA_t,
           
           #SD_accruals = apply(cbind(accrual_ta_t_minus_1,accrual_ta_t,accrual_ta_t_plus_1),1,sd)
            
          SD_accruals = if(length(company)>=3){ c(rep(NA,2),rollapply(accruals_t/TA_t,3,sd))}else{rep(NA,length(company))}
    ) %>% 
    ungroup()  %>% 
    select(c(company,company_abbreviation,yyyymm,tse_industry_name,tse_industry_code,SD_accruals, TWN50, TM100, TF001))
  
}

# TSE "company itself" level  (Standard Deviation of Accruals)
SD_accruals_tse <- as.data.frame(SD_accruals(ifrs_data_tse_add_AQ_EM_columns))


# OTC "company itself" (Standard Deviation of Accruals)
SD_accruals_otc<- as.data.frame(SD_accruals(ifrs_data_otc_add_AQ_EM_columns))


#### Persistence ####

Persistence_resid <- function(data,is_all_sectors){

  grp <- quos(yyyymm, tse_industry_code)[[1:(is_all_sectors+1)]] 
  
  data %>% 
    drop_na(earning_t,earning_t_minus_1,TA_t_minus_1) %>% 
    group_by(UQ(grp)) %>%
    
    mutate(y = earning_t/TA_t_minus_1,
           x1 = earning_t_minus_1/TA_t_minus_1,
           Persistence_resid = residuals(lm(y ~ x1 , data = .))
    ) %>% select(company,company_abbreviation,yyyymm,tse_industry_name,tse_industry_code,Persistence_resid)
  
  
}

# TSE industry level (Persistence)
Persistence_resid_tse <- as.data.frame(Persistence_resid(ifrs_data_tse_add_AQ_EM_columns,0))

# TSE all sectors (Persistence)
Persistence_resid_tse_all_sectors <- as.data.frame(Persistence_resid(ifrs_data_tse_add_AQ_EM_columns,1))

# OTC industry level (Persistence)
Persistence_resid_otc <- as.data.frame(Persistence_resid(ifrs_data_otc_add_AQ_EM_columns,0))

# TSE all sectors (Persistence)
Persistence_resid_otc_all_sectors <- as.data.frame(Persistence_resid(ifrs_data_otc_add_AQ_EM_columns,1))


#### Differential Persistence  ####


Diff_persistence_resid <- function(data,is_all_sectors){
  
  grp <- quos(yyyymm, tse_industry_code)[[1:(is_all_sectors+1)]] 
  
  data %>% 
    drop_na(earning_t, accruals_t_minus_1 , TA_t_minus_1, CFO_t_minus_1) %>% 
    
    group_by(UQ(grp)) %>%
    
    mutate(y = earning_t/TA_t_minus_1,
           x1 = accruals_t_minus_1/TA_t_minus_1,
           x2 = CFO_t_minus_1/TA_t_minus_1,
           Diff_persistence_resid  = residuals(lm(y ~ x1 + x2 , data = .))
    ) %>% select(company,company_abbreviation,yyyymm,tse_industry_name,tse_industry_code,Diff_persistence_resid)

}

# TSE industry level (Differential Persistence)
Diff_persistence_resid_tse <- as.data.frame(Diff_persistence_resid(ifrs_data_tse_add_AQ_EM_columns,0))

# TSE all sectors level (Differential Persistence)
Diff_persistence_resid_tse_all_sectors <- as.data.frame(Diff_persistence_resid(ifrs_data_tse_add_AQ_EM_columns,1))

# OTC industry level (Differential Persistence)
Diff_persistence_resid_otc <- as.data.frame(Diff_persistence_resid(ifrs_data_otc_add_AQ_EM_columns,0))

# OTC all sectors (Differential Persistence)
Diff_persistence_resid_otc_all_sectors <- as.data.frame(Diff_persistence_resid(ifrs_data_otc_add_AQ_EM_columns,1))



#### Earning Slopes  &  YoY  ####

earning_ratio <- function(data){
  
  data %>%
    
    group_by(company,tag_of_section) %>%
    arrange(yyyymm) %>%
    
    mutate(
      ## Earning Slope 
      ebit_slope_t = (earning_t-earning_t_minus_1)/earning_t_minus_1,
      ebit_slope_t_minus_1 = (earning_t_minus_1-earning_t_minus_2)/earning_t_minus_2,
      ebit_slope_t_minus_2 = (earning_t_minus_2-earning_t_minus_3)/earning_t_minus_3,
      opacity_ebit_slope = abs(ebit_slope_t) + abs(ebit_slope_t_minus_1) + abs(ebit_slope_t_minus_2),
      
      
      operating_slope_t = (operating_earning_t-operating_earning_t_minus_1)/operating_earning_t_minus_1,
      operating_slope_t_minus_1 = (operating_earning_t_minus_1-operating_earning_t_minus_2)/operating_earning_t_minus_2,
      operating_slope_t_minus_2 = (operating_earning_t_minus_2-operating_earning_t_minus_3)/operating_earning_t_minus_3,
      opacity_operating_slope = abs(operating_slope_t) + abs(operating_slope_t_minus_1) + abs(operating_slope_t_minus_2),
      
      #sd_ebit_beta = apply(cbind(ebit_beta,ebit_beta_t_minus_1,ebit_beta_t_minus_2),1,sd),
      #sd_operating_beta = apply(cbind(operating_beta,operating_beta_t_minus_1,operating_beta_t_minus_2),1,sd)
      
      
      ## Earning YoY
      
      yoy_ebit_t = ifelse(earning_t_minus_1 > 0, earning_t/earning_t_minus_1, 1-(earning_t/earning_t_minus_1)),
      yoy_ebit_t_minus_1 = ifelse(earning_t_minus_2 > 0, earning_t_minus_1/earning_t_minus_2, 1-(earning_t_minus_1/earning_t_minus_2)),
      yoy_ebit_t_minus_2 = ifelse(earning_t_minus_3 > 0, earning_t_minus_2/earning_t_minus_3, 1-(earning_t_minus_2/earning_t_minus_3)),
      
      yoy_operating_t = ifelse(operating_earning_t_minus_1 > 0, operating_earning_t/operating_earning_t_minus_1, 1-(operating_earning_t/operating_earning_t_minus_1)),
      yoy_operating_t_minus_1 = ifelse(operating_earning_t_minus_2 > 0, operating_earning_t_minus_1/operating_earning_t_minus_2, 1-(operating_earning_t_minus_1/operating_earning_t_minus_2)),
      yoy_operating_t_minus_2 = ifelse(operating_earning_t_minus_3 > 0, operating_earning_t_minus_2/operating_earning_t_minus_3, 1-(operating_earning_t_minus_2/operating_earning_t_minus_3))
      
      
      
      
      
    ) %>%
    
    ungroup() %>%
    
    select(company,company_abbreviation,yyyymm,tse_industry_name,tse_industry_code,ebit_slope_t, ebit_slope_t_minus_1,ebit_slope_t_minus_2,
           operating_slope_t,operating_slope_t_minus_1,operating_slope_t_minus_2,
           yoy_ebit_t,yoy_ebit_t_minus_1,yoy_ebit_t_minus_2,
           yoy_operating_t,yoy_operating_t_minus_1,yoy_operating_t_minus_2
           
           ,opacity_ebit_slope,opacity_operating_slope)
  
  #select(company,company_abbreviation,yyyymm,tse_industry_name,tse_industry_code,ebit_beta, operating_beta,sd_ebit_beta,sd_operating_beta)
  
}


# TSE "company itself" level  (Earning Slopes  &  YoY)
earning_ratio_tse <- as.data.frame(earning_ratio(ifrs_data_tse_add_AQ_EM_columns))


# OTC "company itself" level  (Earning Slopes  &  YoY)
earning_ratio_otc <- as.data.frame(earning_ratio(ifrs_data_otc_add_AQ_EM_columns))





#### Join all AQ measures (Industry Level) ####


AQ_tse_1 <- left_join(SD_accruals_tse,DD_measure_tse[,c("DD_measure","company","yyyymm")], by = c("company","yyyymm"))
AQ_tse_2 <- left_join(AQ_tse_1,Persistence_resid_tse[,c("Persistence_resid","company","yyyymm")], by = c("company","yyyymm"))
AQ_tse_3 <- left_join(AQ_tse_2,Diff_persistence_resid_tse[,c("Diff_persistence_resid","company","yyyymm")], by = c("company","yyyymm"))
AQ_tse_final <- left_join(AQ_tse_3,earning_ratio_tse[,c("ebit_slope_t", "ebit_slope_t_minus_1","ebit_slope_t_minus_2",
                                                        "operating_slope_t","operating_slope_t_minus_1","operating_slope_t_minus_2",
                                                        "yoy_ebit_t","yoy_ebit_t_minus_1","yoy_ebit_t_minus_2",
                                                        "yoy_operating_t","yoy_operating_t_minus_1","yoy_operating_t_minus_2","company","yyyymm"
                                                        ,"opacity_operating_slope","opacity_ebit_slope")], by = c("company","yyyymm"))



AQ_otc_1 <- left_join(SD_accruals_otc,DD_measure_otc[,c("DD_measure","company","yyyymm")], by = c("company","yyyymm"))
AQ_otc_2 <- left_join(AQ_otc_1,Persistence_resid_otc[,c("Persistence_resid","company","yyyymm")], by = c("company","yyyymm"))
AQ_otc_3 <- left_join(AQ_otc_2,Diff_persistence_resid_otc[,c("Diff_persistence_resid","company","yyyymm")], by = c("company","yyyymm"))
AQ_otc_final <- left_join(AQ_otc_3,earning_ratio_otc[,c("ebit_slope_t", "ebit_slope_t_minus_1","ebit_slope_t_minus_2",
                                                        "operating_slope_t","operating_slope_t_minus_1","operating_slope_t_minus_2",
                                                        "yoy_ebit_t","yoy_ebit_t_minus_1","yoy_ebit_t_minus_2",
                                                        "yoy_operating_t","yoy_operating_t_minus_1","yoy_operating_t_minus_2","company","yyyymm"
                                                        ,"opacity_operating_slope","opacity_ebit_slope")], by = c("company","yyyymm"))



#### Join all AQ measures (All Sectors) ####

AQ_tse_1_all_sectors  <- left_join(SD_accruals_tse,DD_measure_tse[,c("DD_measure","company","yyyymm")], by = c("company","yyyymm"))
AQ_tse_2_all_sectors <- left_join(AQ_tse_1_all_sectors,Persistence_resid_tse_all_sectors[,c("Persistence_resid","company","yyyymm")], by = c("company","yyyymm"))
AQ_tse_3_all_sectors <- left_join(AQ_tse_2_all_sectors,Diff_persistence_resid_tse_all_sectors[,c("Diff_persistence_resid","company","yyyymm")], by = c("company","yyyymm"))
AQ_tse_final_all_sectors <- left_join(AQ_tse_3_all_sectors,earning_ratio_tse[,c("ebit_slope_t", "ebit_slope_t_minus_1","ebit_slope_t_minus_2",
                                                        "operating_slope_t","operating_slope_t_minus_1","operating_slope_t_minus_2",
                                                        "yoy_ebit_t","yoy_ebit_t_minus_1","yoy_ebit_t_minus_2",
                                                        "yoy_operating_t","yoy_operating_t_minus_1","yoy_operating_t_minus_2","company","yyyymm"
                                                        ,"opacity_operating_slope","opacity_ebit_slope")], by = c("company","yyyymm"))




AQ_otc_1_all_sectors  <- left_join(SD_accruals_otc,DD_measure_otc[,c("DD_measure","company","yyyymm")], by = c("company","yyyymm"))
AQ_otc_2_all_sectors <- left_join(AQ_otc_1_all_sectors,Persistence_resid_otc_all_sectors[,c("Persistence_resid","company","yyyymm")], by = c("company","yyyymm"))
AQ_otc_3_all_sectors <- left_join(AQ_otc_2_all_sectors,Diff_persistence_resid_otc_all_sectors[,c("Diff_persistence_resid","company","yyyymm")], by = c("company","yyyymm"))
AQ_otc_final_all_sectors <- left_join(AQ_otc_3_all_sectors,earning_ratio_otc[,c("ebit_slope_t", "ebit_slope_t_minus_1","ebit_slope_t_minus_2",
                                                                                "operating_slope_t","operating_slope_t_minus_1","operating_slope_t_minus_2",
                                                                                "yoy_ebit_t","yoy_ebit_t_minus_1","yoy_ebit_t_minus_2",
                                                                                "yoy_operating_t","yoy_operating_t_minus_1","yoy_operating_t_minus_2","company","yyyymm"
                                                                                ,"opacity_operating_slope","opacity_ebit_slope")], by = c("company","yyyymm"))



#### EM Measure ####

#### Jones & Modified Jones Model ####

Jones_modified_Jones_model <- function(data,is_all_sectors){
  
  grp <- quos(yyyymm, tse_industry_code)[[1:(is_all_sectors+1)]] 
  
  data %>% 
    drop_na(EM_total_accruals, TA_t_minus_1, delta_REV, PPE_t) %>% 
    
    group_by(UQ(grp)) %>%
    
    mutate(y = EM_total_accruals/TA_t_minus_1,
           x1 = 1/TA_t_minus_1,
           x2 = delta_REV/TA_t_minus_1,
           x3 = PPE_t/TA_t_minus_1,
           
           
           NDA_Jones_model =  predict(lm(y ~ x1+ x2+ x3 -1 , data = .)),
           Jones_model_measure = residuals(lm(y ~ x1+ x2+ x3 -1 , data = .)),
           
           
           
           beta_1 = rep(as.numeric(lm(y ~ x1+ x2+ x3 -1 , data = .)$coef[1]), length(company)),
           beta_2 = rep(as.numeric(lm(y ~ x1+ x2+ x3 -1 , data = .)$coef[2]), length(company)),
           beta_3 = rep(as.numeric(lm(y ~ x1+ x2+ x3 -1 , data = .)$coef[3]), length(company)),
           
           NDA_modified_Jones_model = beta_1*x1 + beta_2*(delta_REV_minus_delta_REC/TA_t_minus_1) + beta_3*x3,
           Modified_Jones_model_measure = y -NDA_modified_Jones_model
           
           
    )  %>%
    
    ungroup() %>%
    
    group_by(company, tag_of_section) %>%
    
    arrange(yyyymm) %>%
    
    mutate(opacity_Jones_model_measure = if(length(company)>=3){ c(rep(NA,2),rollapply(abs(Jones_model_measure),3,sum))}else{
      rep(NA,length(company))
    },
    
    opacity_modified_Jones_model_measure = if(length(company)>=3){ c(rep(NA,2),rollapply(abs(Modified_Jones_model_measure),3,sum))}else{
      rep(NA,length(company))
    }
    
    ) %>%
    
     ungroup() %>%
    
    
    select(company,company_abbreviation,yyyymm,tse_industry_name,tse_industry_code,Jones_model_measure, Modified_Jones_model_measure
           ,opacity_Jones_model_measure,opacity_modified_Jones_model_measure)
}


# TSE industry level (Jones & Modified Jones Model)
Jones_and_modified_Jones_model_tse <- as.data.frame(Jones_modified_Jones_model(ifrs_data_tse_add_AQ_EM_columns,0))

# TSE all sectors (Jones & Modified Jones Model)
Jones_and_modified_Jones_model_tse_all_sectors <- as.data.frame(Jones_modified_Jones_model(ifrs_data_tse_add_AQ_EM_columns,1))


# OTC industry level (Jones & Modified Jones Model)
Jones_and_modified_Jones_model_otc <- as.data.frame(Jones_modified_Jones_model(ifrs_data_otc_add_AQ_EM_columns,0))

# OTC all sectors (Jones & Modified Jones Model)
Jones_and_modified_Jones_model_otc_all_sectors <- as.data.frame(Jones_modified_Jones_model(ifrs_data_otc_add_AQ_EM_columns,1))



#### Performance Matching Model ####

Performance_matching_model <- function(data,is_all_sectors){
  
  grp <- quos(yyyymm, tse_industry_code)[[1:(is_all_sectors+1)]] 
  
  data %>% 
    drop_na(EM_total_accruals, TA_t_minus_1, delta_REV_minus_delta_REC, PPE_t, ROA_t_minus_1) %>% 
    
    group_by(tse_industry_code, yyyymm) %>%
    
    mutate(y = EM_total_accruals/TA_t_minus_1,
           x1 = 1/TA_t_minus_1,
           x2 = delta_REV_minus_delta_REC/TA_t_minus_1,
           x3 = PPE_t/TA_t_minus_1,
           x4 = ROA_t_minus_1,
           Performance_matching_measure = residuals(lm(y ~ x1+ x2+ x3 , data = .))
           
    )  %>%
    
    ungroup() %>%
    
    group_by(company, tag_of_section) %>%
    
    arrange(yyyymm) %>%
    
    mutate(opacity_performance_matching = if(length(company)>=3){ c(rep(NA,2),rollapply(abs(Performance_matching_measure),3,sum))}else{
        rep(NA,length(company))
        }
    ) %>%
    
    ungroup() %>%
  
    select(company,company_abbreviation,yyyymm,tse_industry_name,tse_industry_code,Performance_matching_measure,opacity_performance_matching,tag_of_section)
  
}


# TSE industry level (Performance Matching Model)
Performance_matching_measure_tse <- as.data.frame(Performance_matching_model(ifrs_data_tse_add_AQ_EM_columns,0))

# TSE all sectors (Performance Matching Model)
Performance_matching_measure_tse_all_sectors <- as.data.frame(Performance_matching_model(ifrs_data_tse_add_AQ_EM_columns,1))


# OTC industry level (Performance Matching Model)
Performance_matching_measure_otc <- as.data.frame(Performance_matching_model(ifrs_data_otc_add_AQ_EM_columns,0))

# OTC all sectors (Performance Matching Model)
Performance_matching_measure_otc_all_sectors <- as.data.frame(Performance_matching_model(ifrs_data_otc_add_AQ_EM_columns,1))


#### Join all AQ & EM Measures (Industry Level) ####

AQ_EM_tse_1 <- left_join(AQ_tse_final, 
                         Jones_and_modified_Jones_model_tse[,c("company", "yyyymm", "Jones_model_measure","Modified_Jones_model_measure"
                                                               ,"opacity_Jones_model_measure","opacity_modified_Jones_model_measure")],
                         by = c("company", "yyyymm"))

AQ_EM_tse_final <- left_join(AQ_EM_tse_1, 
                             Performance_matching_measure_tse[,c("company", "yyyymm", "Performance_matching_measure","opacity_performance_matching")],
                             by = c("company", "yyyymm"))

AQ_EM_otc_1 <- left_join(AQ_otc_final, 
                         Jones_and_modified_Jones_model_otc[,c("company", "yyyymm", "Jones_model_measure","Modified_Jones_model_measure"
                                                               ,"opacity_Jones_model_measure","opacity_modified_Jones_model_measure")],
                         by = c("company", "yyyymm"))

AQ_EM_otc_final <- left_join(AQ_EM_otc_1, 
                             Performance_matching_measure_otc[,c("company", "yyyymm", "Performance_matching_measure","opacity_performance_matching")],
                             by = c("company", "yyyymm"))



#### Join all AQ & EM Measures (All sectors) ####


AQ_EM_tse_1_all_sectors <- left_join(AQ_tse_final_all_sectors, 
                         Jones_and_modified_Jones_model_tse_all_sectors[,c("company", "yyyymm", "Jones_model_measure","Modified_Jones_model_measure"
                                                                            ,"opacity_Jones_model_measure","opacity_modified_Jones_model_measure")],
                         by = c("company", "yyyymm"))

AQ_EM_tse_final_all_sectors <- left_join(AQ_EM_tse_1_all_sectors, 
                             Performance_matching_measure_tse_all_sectors[,c("company", "yyyymm", "Performance_matching_measure","opacity_performance_matching")],
                             by = c("company", "yyyymm"))

AQ_EM_otc_1_all_sectors <- left_join(AQ_otc_final_all_sectors, 
                         Jones_and_modified_Jones_model_otc_all_sectors[,c("company", "yyyymm", "Jones_model_measure","Modified_Jones_model_measure"
                                                                           ,"opacity_Jones_model_measure","opacity_modified_Jones_model_measure")],
                         by = c("company", "yyyymm"))

AQ_EM_otc_final_all_sectors <- left_join(AQ_EM_otc_1_all_sectors, 
                             Performance_matching_measure_otc_all_sectors[,c("company", "yyyymm", "Performance_matching_measure","opacity_performance_matching")],
                             by = c("company", "yyyymm"))


