#### load the data ####

setwd("C:\\Users\\user\\Google 雲端硬碟\\石百達\\盧佳琪\\data")

library(dplyr)
library(tidyr)
library(broom)
library(tidyverse)

ifrs_data <- read.csv("IFRS上市上櫃財報資料.csv")
#index_factor <- read.csv("上市上櫃指數因子.csv")



#### Data Cleaning ####

#Remove the space
ifrs_data <- as.data.frame(apply(ifrs_data, 2 , function(input){gsub(" ","",input)} )) 
index_data <- as.data.frame(apply(index_factor, 2 , function(input){gsub(" ","",input)} )) 

colnames_to_string_ifrs <- c("公司","簡稱","上市別","TSE.產業別","TSE新產業_名稱","TSE新產業_代碼",
                        "TSE新產業名")


# Convert factor to string/number
index_data_cleaned <- index_data %>% mutate_if(is.factor, as.character)

ifrs_data_tag <- ifrs_data %>% select(colnames_to_string_ifrs) %>% mutate_if(is.factor, as.character)
ifrs_data_numeric <- ifrs_data %>% select(-colnames_to_string_ifrs) %>% mutate_if(is.factor, 
                                                             function(input){as.numeric(as.character(input))})

ifrs_data_cleaned <- cbind(ifrs_data_tag,ifrs_data_numeric)



#### Add AQ & EM Variables ####

ifrs_data_tse  <- ifrs_data_cleaned %>% filter(上市別 == "TSE")
ifrs_data_otc <- ifrs_data_cleaned %>% filter(上市別 == "OTC")



ifrs_data_tse$STDEBT <- ifelse(is.na(ifrs_data_tse$短期借款),0,ifrs_data_tse$短期借款) + 
                        ifelse(is.na(ifrs_data_tse$應付帳款及票據),0,ifrs_data_tse$應付帳款及票據) + 
                        ifelse(is.na(ifrs_data_tse$一年內到期長期負債),0,ifrs_data_tse$一年內到期長期負債)

ifrs_data_otc$STDEBT <- ifelse(is.na(ifrs_data_otc$短期借款),0,ifrs_data_otc$短期借款) + 
                        ifelse(is.na(ifrs_data_otc$應付帳款及票據),0,ifrs_data_otc$應付帳款及票據) + 
                        ifelse(is.na(ifrs_data_otc$一年內到期長期負債),0,ifrs_data_otc$一年內到期長期負債)



ifrs_data_tse$ROA.C.稅前息前折舊前 <- ifrs_data_tse$ROA.C.稅前息前折舊前/100
ifrs_data_tse$ROA.B.稅後息前折舊前 <- ifrs_data_tse$ROA.B.稅後息前折舊前/100
ifrs_data_tse$ROA.A.稅後息前 <- ifrs_data_tse$ROA.A.稅後息前/100

ifrs_data_otc$ROA.C.稅前息前折舊前 <- ifrs_data_otc$ROA.C.稅前息前折舊前/100
ifrs_data_otc$ROA.B.稅後息前折舊前 <- ifrs_data_otc$ROA.B.稅後息前折舊前/100
ifrs_data_otc$ROA.A.稅後息前 <- ifrs_data_otc$ROA.A.稅後息前/100


add_AQ_EM_columns <- function(data){
  data %>% group_by(公司) %>% arrange(年.月) %>% mutate(   ## AQ Variables
                                                           delta_CA = c(NA,diff(流動資產)),
                                                           delta_CL = c(NA,diff(流動負債)),
                                                           delta_cash = c(NA,diff(現金及約當現金)),
                                                           delta_STDEBT = c(NA,diff(STDEBT)),
                                                           
                                                           accruals_t = delta_CA-delta_CL-delta_cash+delta_STDEBT,
                                                           accruals_t_minus_1 = c(NA,accruals_t[-length(公司)]),
                                                           accruals_t_plus_1 = c(accruals_t[-1], NA),
                                                           
                                                           CFO_t = 來自營運之現金流量,
                                                           CFO_t_minus_1 = c(NA,來自營運之現金流量[-length(公司)]),
                                                           CFO_t_plus_1 = c(來自營運之現金流量[-1],NA),
                                                           
                                                           TA_t = 資產總額,
                                                           TA_t_minus_1 = c(NA,資產總額[-length(公司)]),
                                                           
                                                           earning_t = 稅前息前淨利,
                                                           earning_t_minus_1 = c(NA,稅前息前淨利[-length(公司)]),
                                                          
                                                    
                                                           ## EM Vairables
                                                           EM_total_accruals = 合併總損益-來自營運之現金流量,
                                                           delta_REV = c(NA, diff(營業收入毛額)),
                                                           delta_REC = c(NA, diff(應收帳款及票據)),
                                                           delta_REV_minus_delta_REC = delta_REV-delta_REC,
                                                           
                                                           PPE_t = 不動產廠房及設備,
                                                           ROA_t_minus_1 = c(NA, ROA.A.稅後息前[-length(公司)])
                                                    
                                                    )

}

ifrs_data_tse_add_AQ_EM_columns <- as.data.frame(add_AQ_EM_columns(ifrs_data_tse))
ifrs_data_otc_add_AQ_EM_columns <- as.data.frame(add_AQ_EM_columns(ifrs_data_otc))





#### AQ Measure ####

  #### DD Measure (Standard Deviation of Accruals Residuals) ####

DD_measure_resid <- function(data){

  data %>% 
  drop_na(accruals_t, CFO_t, CFO_t_minus_1, CFO_t_plus_1, TA_t_minus_1) %>% 
  
  group_by(TSE新產業_代碼, 年.月) %>%
  
    mutate(y = accruals_t/TA_t_minus_1,
         x1 = CFO_t_minus_1/TA_t_minus_1,
         x2 = CFO_t/TA_t_minus_1,
         x3 = CFO_t_plus_1/TA_t_minus_1,
         DD_resid  = residuals(lm(y ~ x1 + x2 + x3, data = .))
  ) %>% select(公司,簡稱,年.月,TSE新產業_名稱,TSE新產業_代碼,DD_resid)
  #%>% nest() %>%  
  #mutate(model = map(data , ~lm(y ~ x1 + x2 + x3, data = .)),
  #       model_data = map2(model, data ,broom::augment)) %>% unnest(model_data)
  
  #%>% select(.resid, .se.fit, .sigma)

}

DD_measure_resid_tse <- as.data.frame(DD_measure_resid(ifrs_data_tse_add_AQ_EM_columns))
DD_measure_resid_otc <- as.data.frame(DD_measure_resid(ifrs_data_otc_add_AQ_EM_columns))


DD_measure <- function(data){

data %>% group_by(公司) %>% arrange(年.月) %>% 
  mutate(DD_resid_t_minus_1 = c(NA, DD_resid[-length(公司)]),
         DD_resid_t_plus_1 = c(DD_resid[-1],NA),
         DD_measure = apply(cbind(DD_resid_t_minus_1,DD_resid,DD_resid_t_plus_1),1,sd)) %>%
    select(-c(DD_resid_t_minus_1,DD_resid_t_plus_1,DD_resid))

}

DD_measure_tse <- as.data.frame(DD_measure(DD_measure_resid_tse))
DD_measure_otc <- as.data.frame(DD_measure(DD_measure_resid_otc))


  #### Standard Deviation of Accruals ####

SD_accruals <- function(data){
data %>% group_by(公司) %>% arrange(年.月) %>% 
  mutate(accrual_ta_t_minus_1 = accruals_t_minus_1/TA_t,
         accrual_ta_t = accruals_t/TA_t,
         accrual_ta_t_plus_1 = accruals_t_plus_1/TA_t,
         
         SD_accruals = apply(cbind(accrual_ta_t_minus_1,accrual_ta_t,accrual_ta_t_plus_1),1,sd)
         
         ) %>%
  
  select(c(公司,簡稱,年.月,TSE新產業_名稱,TSE新產業_代碼,SD_accruals))

}

SD_accruals_tse <- as.data.frame(SD_accruals(ifrs_data_tse_add_AQ_EM_columns))
SD_accruals_otc<- as.data.frame(SD_accruals(ifrs_data_otc_add_AQ_EM_columns))


  #### Persistence ####


Persistence_resid <- function(data){
  
  data %>% 
    drop_na(earning_t, earning_t_minus_1, TA_t_minus_1) %>% 
    
    group_by(TSE新產業_代碼, 年.月) %>%
    
    mutate(y = earning_t/TA_t_minus_1,
           x1 = earning_t_minus_1/TA_t_minus_1,
           Persistence_resid = residuals(lm(y ~ x1 , data = .))
    ) %>% select(公司,簡稱,年.月,TSE新產業_名稱,TSE新產業_代碼,Persistence_resid)
  #%>% nest() %>%  
  #mutate(model = map(data , ~lm(y ~ x1 + x2 + x3, data = .)),
  #       model_data = map2(model, data ,broom::augment)) %>% unnest(model_data)
  
  #%>% select(.resid, .se.fit, .sigma)
  
}

Persistence_resid_tse <- as.data.frame(Persistence_resid(ifrs_data_tse_add_AQ_EM_columns))
Persistence_resid_otc <- as.data.frame(Persistence_resid(ifrs_data_otc_add_AQ_EM_columns))


  #### Differential Persistence  ####


Diff_persistence_resid <- function(data){
  
  data %>% 
    drop_na(earning_t, accruals_t_minus_1 , TA_t_minus_1, CFO_t_minus_1) %>% 
    
    group_by(TSE新產業_代碼, 年.月) %>%
    
    mutate(y = earning_t/TA_t_minus_1,
           x1 = accruals_t_minus_1/TA_t_minus_1,
           x2 = CFO_t_minus_1/TA_t_minus_1,
           Diff_persistence_resid  = residuals(lm(y ~ x1 + x2 , data = .))
    ) %>% select(公司,簡稱,年.月,TSE新產業_名稱,TSE新產業_代碼,Diff_persistence_resid)
  #%>% nest() %>%  
  #mutate(model = map(data , ~lm(y ~ x1 + x2 + x3, data = .)),
  #       model_data = map2(model, data ,broom::augment)) %>% unnest(model_data)
  
  #%>% select(.resid, .se.fit, .sigma)
  
}

Diff_persistence_resid_tse <- as.data.frame(Diff_persistence_resid(ifrs_data_tse_add_AQ_EM_columns))
Diff_persistence_resid_otc <- as.data.frame(Diff_persistence_resid(ifrs_data_otc_add_AQ_EM_columns))

  #### Join all AQ measures ####


AQ_tse_1 <- left_join(SD_accruals_tse,DD_measure_tse[,c("DD_measure","公司","年.月")], by = c("公司","年.月"))
AQ_tse_2 <- left_join(AQ_tse_1,Persistence_resid_tse[,c("Persistence_resid","公司","年.月")], by = c("公司","年.月"))
AQ_tse_final <- left_join(AQ_tse_2,Diff_persistence_resid_tse[,c("Diff_persistence_resid","公司","年.月")], by = c("公司","年.月"))


AQ_otc_1 <- left_join(SD_accruals_otc,DD_measure_otc[,c("DD_measure","公司","年.月")], by = c("公司","年.月"))
AQ_otc_2 <- left_join(AQ_otc_1,Persistence_resid_otc[,c("Persistence_resid","公司","年.月")], by = c("公司","年.月"))
AQ_otc_final <- left_join(AQ_otc_2,Diff_persistence_resid_otc[,c("Diff_persistence_resid","公司","年.月")], by = c("公司","年.月"))





#### EM Measure ####

  #### Jones & Modified Jones Model ####

Jones_modified_Jones_model <- function(data){
 
  
  data %>% 
    drop_na(EM_total_accruals, TA_t_minus_1, delta_REV, PPE_t) %>% 
    
    group_by(TSE新產業_代碼, 年.月) %>%
  
    mutate(y = EM_total_accruals/TA_t_minus_1,
           x1 = 1/TA_t_minus_1,
           x2 = delta_REV/TA_t_minus_1,
           x3 = PPE_t/TA_t_minus_1,
           
           
           NDA_Jones_model =  predict(lm(y ~ x1+ x2+ x3 -1 , data = .)),
           Jones_model_measure = residuals(lm(y ~ x1+ x2+ x3 -1 , data = .)),
           
           
           
           beta_1 = rep(as.numeric(lm(y ~ x1+ x2+ x3 -1 , data = .)$coef[1]), length(公司)),
           beta_2 = rep(as.numeric(lm(y ~ x1+ x2+ x3 -1 , data = .)$coef[2]), length(公司)),
           beta_3 = rep(as.numeric(lm(y ~ x1+ x2+ x3 -1 , data = .)$coef[3]), length(公司)),
           
           NDA_modified_Jones_model = beta_1*x1 + beta_2*(delta_REV_minus_delta_REC/TA_t_minus_1) + beta_3*x3,
           Modified_Jones_model_measure = y -NDA_modified_Jones_model
             
           
           )  %>%
  
  select(公司,簡稱,年.月,TSE新產業_名稱,TSE新產業_代碼,Jones_model_measure, Modified_Jones_model_measure)
}

Jones_and_modified_Jones_model_tse <- as.data.frame(Jones_modified_Jones_model(ifrs_data_tse_add_AQ_EM_columns))
Jones_and_modified_Jones_model_otc <- as.data.frame(Jones_modified_Jones_model(ifrs_data_otc_add_AQ_EM_columns))


  #### Performance Matching Model ####
 
Performance_matching_model <- function(data){
data %>% 
  drop_na(EM_total_accruals, TA_t_minus_1, delta_REV_minus_delta_REC, PPE_t, ROA_t_minus_1) %>% 
  
  group_by(TSE新產業_代碼, 年.月) %>%
  
  mutate(y = EM_total_accruals/TA_t_minus_1,
         x1 = 1/TA_t_minus_1,
         x2 = delta_REV_minus_delta_REC/TA_t_minus_1,
         x3 = PPE_t/TA_t_minus_1,
         x4 = ROA_t_minus_1,
         Performance_matching_measure = residuals(lm(y ~ x1+ x2+ x3 , data = .))
         
  )  %>%
  
  select(公司,簡稱,年.月,TSE新產業_名稱,TSE新產業_代碼,Performance_matching_measure)

}


Performance_matching_measure_tse <- as.data.frame(Performance_matching_model(ifrs_data_tse_add_AQ_EM_columns))
Performance_matching_measure_otc <- as.data.frame(Performance_matching_model(ifrs_data_otc_add_AQ_EM_columns))



  #### Join all AQ & EM Measures ####

AQ_EM_tse_1 <- left_join(AQ_tse_final, 
          Jones_and_modified_Jones_model_tse[,c("公司", "年.月", "Jones_model_measure","Modified_Jones_model_measure")],
          by = c("公司", "年.月"))

AQ_EM_tse_final <- left_join(AQ_EM_tse_1, 
                         Performance_matching_measure_tse[,c("公司", "年.月", "Performance_matching_measure")],
                         by = c("公司", "年.月"))

AQ_EM_otc_1 <- left_join(AQ_otc_final, 
                         Jones_and_modified_Jones_model_otc[,c("公司", "年.月", "Jones_model_measure","Modified_Jones_model_measure")],
                         by = c("公司", "年.月"))

AQ_EM_otc_final <- left_join(AQ_EM_otc_1, 
                             Performance_matching_measure_otc[,c("公司", "年.月", "Performance_matching_measure")],
                             by = c("公司", "年.月"))





#### Plots ####



test <- AQ_EM_tse_final %>% filter(TSE新產業_代碼 == "M2324" & 年.月 == "201612")

plot(test$DD_measure , test$Performance_matching_measure
     , xlab = "DD Measure", ylab = "Performance Matching", pch = "")

text(x= test$DD_measure , y = test$Performance_matching_measure , labels=test$簡稱)



plot(test$DD_measure , test$Modified_Jones_model_measure
     , xlab = "DD Measure", ylab = "Modified Jones model", pch = "")

text(x= test$DD_measure , y = test$Modified_Jones_model_measure , labels=test$簡稱)
