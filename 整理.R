setwd("C:\\Users\\user\\Google 雲端硬碟\\石百達\\盧佳琪\\data")

library(dplyr)

##### 成分股數量與去年年報數目比較 #####

stock_exchange_market <- read.csv("上市.csv" , header = T)
otc <- read.csv("上櫃.csv" , header = T)


stock_exchange_count <- stock_exchange_market %>% group_by(TSE新產業名) %>% summarise( count_of_industries = length(unique(公司)))
otc_count <- otc %>% group_by(TSE新產業名) %>% summarise( count_of_industries = length(unique(公司)))

stock_exchange_count_table <- as.data.frame(stock_exchange_count)
otc_count <- as.data.frame(otc_count)

industry_code_list <- strsplit(as.character(stock_exchange_count_table[,1]), split = " ")

industry_code <- sapply(industry_code_list , function(x) x[1]) #unlist the industry code
stock_exchange_count_table_final  <- cbind(stock_exchange_count_table, industry_code)
stock_exchange_count_table_final$industry_code <- as.character(stock_exchange_count_table_final$industry_code)

# write.csv(stock_exchange_count_table, file = "count.csv" )
# write.csv(otc_count, file = "count_otc.csv" )

stock_factors <- read.csv("成分股日資料.csv" )

head(stock_factors)
str(stock_factors)


latest_date <- max(as.numeric(stock_factors$年月日))
temp <- which(stock_factors$年月日 == latest_date) # Lastest date of data
unique(stock_factors[temp,2]) #Check all the industries are included.

count_of_stock_factors<- stock_factors %>% filter(年月日 == latest_date) %>% group_by(公司代碼) %>% summarise(count_of_factors = length(unique(成份股)))
count_of_stock_factors <- as.data.frame(count_of_stock_factors)
colnames(count_of_stock_factors)[1] <- "industry_code"

count_of_stock_factors$industry_code <- gsub(" ", "",as.character(count_of_stock_factors$industry_code))

comparison_stock_and_factors <- left_join(stock_exchange_count_table_final , count_of_stock_factors, by = "industry_code")
write.csv(comparison_stock_and_factors , file = "成分股比較.csv")


##### 找出期間內不同公司 ####


# 財報資料處理
fs_20181231 <- stock_exchange_market[1:3]

industry_code_fs <- sapply(strsplit(as.character(fs_20181231$TSE新產業名), split = " "), function(x) x[1])

fs_20181231_with_code  <- cbind(fs_20181231,industry_code_fs)
fs_20181231_with_code$industry_code_fs <- as.character(fs_20181231_with_code$industry_code_fs)
fs_20181231_with_code$公司 <- as.character(fs_20181231_with_code$公司)

colnames(fs_20181231_with_code)[1] <- "company_code"

# 成分股資料處理

head(stock_factors)

company_code <- sapply(strsplit(as.character(stock_factors$成份股), split = " "), function(x) x[1])
stock_factors_with_code <- cbind(stock_factors , company_code)

stock_factors_with_code$company_code <- gsub(" ", "",as.character(stock_factors_with_code$company_code))
stock_factors_with_code$公司代碼 <- gsub(" ", "",as.character(stock_factors_with_code$公司代碼))

colnames(stock_factors_with_code)[1] <- "industry_code"


all_index_code <- unique(fs_20181231_with_code$industry_code_fs)
all_index_code <- all_index_code[-length(all_index_code)]
change_company <- NULL

for(i in all_index_code){
  
  fs_company <- fs_20181231_with_code %>% filter(industry_code_fs == i )
  index_company <- stock_factors_with_code %>% filter(年月日 == latest_date ) %>% filter(industry_code == i )
  
  
  change_company <- rbind(change_company, anti_join(fs_company,index_company, by = "company_code"))
  
}

write.csv(change_company, file = "成分股變化公司.csv")

