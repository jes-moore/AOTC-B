library(finreportr)
library(curl)
library(XBRL)
library(lubridate)
library(data.table)
# Setup variables
ticker <- 'fb'

# Get a list of supported tickets
stockTickers <- as.vector(fread('data/tickers.csv')[[1]])

# Setup options for downloading files with XBRL package
options(stringsAsFactors = FALSE,download.file.method = 'curl')

# Loop through all tickers and get 2014-2016 EOY Data 

getInfo <- function(ticker){
        IS <- GetIncome(ticker, 2017)[,c(1,3,5)]
        fwrite(IS,paste('C:/Users/User/Desktop/AOTC-B/shinyApp/data/Yearly/income_statements/',ticker,'_is.csv',sep = ''))
        BS <- GetBalanceSheet(ticker, 2017)[,c(1,3,5)]
        fwrite(BS,paste('C:/Users/User/Desktop/AOTC-B/shinyApp/data/Yearly/balance_sheets/',ticker,'_bs.csv',sep = ''))
        CF <- GetCashFlow(ticker,2017)[,c(1,3,5)]
        fwrite(CF,paste('C:/Users/User/Desktop/AOTC-B/shinyApp/data/Yearly/cash_flow/',ticker,'_cf.csv',sep = ''))
}

        
library(doParallel)  
cl <- makeCluster(4)  
registerDoParallel(cl)  

foreach(i=1:4,.packages=c('XBRL','finreportr','data.table')) %dopar% {
        try(getInfo(stockTickers[[i]]))
        
}

stopCluster(cl)  

        
