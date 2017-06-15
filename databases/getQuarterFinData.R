library(finreportr)
library(curl)
library(XBRL)
library(lubridate)
library(data.table)
library(dplyr)
# Get a list of supported tickets
stockTickers <- as.vector(fread('data/tickers.csv')[[1]])

# Setup options for downloading files with XBRL package
options(stringsAsFactors = FALSE,download.file.method = 'curl')

# # Loop through all tickers and get 2014-2016 EOY Data 
# # Create Cache directories
# Sys.chmod(list.dirs("."), "777")
# f <- list.files(".", all.files = TRUE, full.names = TRUE, recursive=TRUE)
# Sys.chmod(f, (file.info(f)$mode | "664"))
# #dir.create('/home/jmo/Desktop/AOTC-B/databases/data/xbrl.cache/') #Create xbrl.cache directory
# 
# for(i in 1:length(stockTickers)){
#         dir.create(paste('/home/jmo/Desktop/AOTC-B/databases/data/xbrl.cache/',stockTickers[i],sep=''))
#         print(i)
# }


getQuarterly <- function(ticker){
        IS1 <- try(GetIncome3(ticker, 1)[,c(1,3,5)])
        IS2 <- try(GetIncome3(ticker, 2)[,c(1,3,5)])
        IS <- try(rbind(IS1,IS2))
        try(fwrite(IS,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Quarterly/income_statements/',ticker,'_is.csv',sep = '')))
        
        BS1 <- try(GetBalanceSheet3(ticker, 1)[,c(1,3,5)])
        BS2 <- try(GetBalanceSheet3(ticker, 2)[,c(1,3,5)])
        BS <- try(rbind(BS1,BS2))
        try(fwrite(BS,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Quarterly/balance_sheets/',ticker,'_bs.csv',sep = '')))
        
        CF1 <- try(GetCashFlow3(ticker, 1)[,c(1,3,5)])
        CF2 <- try(GetCashFlow3(ticker, 2)[,c(1,3,5)])
        CF <- try(rbind(CF1,CF2))
        try(fwrite(CF,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Yearly/cash_flow/',ticker,'_cf.csv',sep = '')))
}


library(doParallel) 

start.time <- Sys.time() # Time code 

cl <- makeCluster(16)  
registerDoParallel(cl)  

foreach(i=1:3187,.packages=c('XBRL','finreportr','data.table','dplyr','curl')) %dopar% {
        options(stringsAsFactors = FALSE,download.file.method = 'curl')
        try(getQuarterly(stockTickers[[i]]))
        
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
stopCluster(cl) 




