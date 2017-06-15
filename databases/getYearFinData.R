library(finreportr)
library(curl)
library(XBRL)
library(lubridate)
library(data.table)
library(dplyr)
# Setup variables
ticker <- 'AA'

# Get a list of supported tickets
stockTickers <- as.vector(fread('data/tickers.csv')[[1]])

# Setup options for downloading files with XBRL package
options(stringsAsFactors = FALSE,download.file.method = 'curl')

# Loop through all tickers and get 2014-2016 EOY Data 
# Create Cache directories
Sys.chmod(list.dirs("."), "777")
f <- list.files(".", all.files = TRUE, full.names = TRUE, recursive=TRUE)
Sys.chmod(f, (file.info(f)$mode | "664"))
dir.create('/home/jmo/Desktop/AOTC-B/databases/data/xbrl.cache/') #Create xbrl.cache directory
for(i in 1:length(stockTickers)){
        dir.create(paste('/home/jmo/Desktop/AOTC-B/databases/data/xbrl.cache/',stockTickers[i],sep=''))
        print(i)
}

getInfo <- function(ticker){
        IS <- try(GetIncome2(ticker, 2017)[,c(1,3,5)])
        if(class(IS) == "try-error"){
                IS <- try(GetIncome2(ticker, 2016)[,c(1,3,5)])
                if(class(IS) == 'try-error'){
                        return(IS)
                }
        }
        try(fwrite(IS,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Yearly/income_statements/',ticker,'_is.csv',sep = '')))
        BS <- try(GetBalanceSheet2(ticker, 2017)[,c(1,3,5)])
        if(class(BS) == "try-error"){
                BS <- try(GetBalanceSheet2(ticker, 2016)[,c(1,3,5)])
                if(class(BS) == 'try-error'){
                        return(BS)
                }
        }
        try(fwrite(BS,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Yearly/balance_sheets/',ticker,'_bs.csv',sep = '')))
        CF <- try(GetCashFlow2(ticker,2017)[,c(1,3,5)])
        if(class(CF) == 'try-error'){
                CF <- try(GetCashFlow2(ticker, 2016)[,c(1,3,5)])
                if(class(CF) == 'try-error'){
                        return(IS)
                }
        }
        try(fwrite(CF,paste('/home/jmo/Desktop/AOTC-B/shinyApp/data/Yearly/cash_flow/',ticker,'_cf.csv',sep = '')))
}


library(doParallel) 

start.time <- Sys.time() # Time code 

cl <- makeCluster(16)  
registerDoParallel(cl)  

foreach(i=2001:3187,.packages=c('XBRL','finreportr','data.table','dplyr','curl')) %dopar% {
        options(stringsAsFactors = FALSE,download.file.method = 'curl')
        try(getInfo(stockTickers[[i]]))
        
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
stopCluster(cl) 



#getInfo(ticker) #test function
# 
# 
# for(i in 1:length(stockTickers)) {
#         try(getInfo(stockTickers[i]))
#         print(i)
# }

        
