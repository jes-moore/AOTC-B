library(finreportr)
library(curl)
library(XBRL)
library(lubridate)
library(data.table)
# Setup variables
ticker <- 'fb'

# Get a list of supported tickets
stockTickers <- as.vector(read.csv('data/tickers.csv'))

# Setup options for downloading files with XBRL package
options(stringsAsFactors = FALSE,download.file.method = 'curl')

# Loop through all tickers and get 2014-2016 EOY Data 

getInfo <- function(ticker){
        IS <- GetIncome(ticker, 2017)[,c(1,3,5)]
        fwrite(IS,paste('../shinyApp/data/income_statements/',ticker,'_is.csv',sep = ''))
        BS <- GetBalanceSheet(ticker, 2017)[,c(1,3,5)]
        fwrite(BS,paste('../shinyApp/data/balance_sheets/',ticker,'_bs.csv',sep = ''))
        CF <- GetCashFlow(ticker,2017)[,c(1,3,5)]
        fwrite(CF,paste('../shinyApp/data/cash_flow/',ticker,'_cf.csv',sep = ''))
}

        

