library(finreportr)
library(curl)
library(XBRL)
library(lubridate)
library(data.table)
library(dplyr)
# Get a list of supported tickets
stockTickers <- as.vector(fread('/home/jmo/Desktop/AOTC-B/shinyApp/data/tickers.csv')[[1]])
setwd(dir = 'AOTC-B/databases/')
# #Update Tickers
# allTickers <- fread('https://www.quandl.com/api/v3/datatables/WIKI/PRICES.csv?date=20170530&api_key=Xa-XyezxZxsEZpmhKYkt')
# tickers <- allTickers[,1]
# fwrite(x = tickers,'../shinyApp/data/tickers.csv')

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
        IS1 <- try(GetIncome3(ticker, 1)[,c(1,3,4,5)])
        IS2 <- try(GetIncome3(ticker, 2)[,c(1,3,4,5)])
        IS3 <- try(GetIncome3(ticker, 3)[,c(1,3,4,5)])
        IS <- try(rbind(IS1,IS2,IS3))
        if(class(IS) == "try-error"){
                IS <- try(IS1)
                if(class(IS) == 'try-error'){
                        return(IS)
                }
        }
        (fwrite(IS,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Quarterly/income_statements/',ticker,'_is.csv',sep = '')))
        rm(IS1,IS2,IS3,IS)
        
        BS1 <- try(GetBalanceSheet3(ticker, 1)[,c(1,3,4,5)])
        BS2 <- try(GetBalanceSheet3(ticker, 2)[,c(1,3,4,5)])
        BS3 <- try(GetBalanceSheet3(ticker, 3)[,c(1,3,4,5)])
        BS <- try(rbind(BS1,BS2,BS3))
        if(class(BS) == "try-error"){
                BS <- try(BS1)
                if(class(BS) == 'try-error'){
                        return(BS)
                }
        }
        (fwrite(BS,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Quarterly/balance_sheets/',ticker,'_bs.csv',sep = '')))
        rm(BS1,BS2,BS3,BS)

        CF1 <- try(GetCashFlow3(ticker, 1)[,c(1,3,4,5)])
        CF2 <- try(GetCashFlow3(ticker, 2)[,c(1,3,4,5)])
        CF3 <- try(GetCashFlow3(ticker, 3)[,c(1,3,4,5)])
        CF <- try(rbind(CF1,CF2,CF3))
        if(class(CF) == "try-error"){
                CF <- try(CF1)
                if(class(CF) == 'try-error'){
                        return(CF)
                }
        }
        (fwrite(CF,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Quarterly/cash_flow/',ticker,'_cf.csv',sep = '')))
}

# Setup options for downloading files with XBRL package
options(stringsAsFactors = FALSE,download.file.method = 'curl')

 for(i in 62) {
         out <- try(getQuarterly(stockTickers[i]))
         print(i)
}

library(doParallel) 
start.time <- Sys.time() # Time code 
cl <- makeCluster(10) #Max 14 clusters  
registerDoParallel(cl)  

foreach(i=1:150,.packages=c('XBRL','finreportr','data.table','dplyr','curl')) %dopar% {
        options(stringsAsFactors = FALSE,download.file.method = 'curl')
        try(getQuarterly(stockTickers[[i]]))
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
stopCluster(cl) 




