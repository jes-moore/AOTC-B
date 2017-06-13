library(tidyquant)
library(lubridate)
library(DT)
library(data.table)
library(scales)
library(finreportr)
library(XBRL)
library(curl)
library(RCurl)

#################################################################################
##################### Download Required files Example ############################
#################################################################################


getRatios <- function(ticker){
        tq_get(ticker,get='key.ratios')
}
#df.key.ratios <- getRatios(ticker)
#profitability <- df.key.ratios$data[[2]]

#Statistics (EPS,P/E,etc)
getStats <- function(ticker){
        tq_get(ticker,get='key.stats',from='2014-01-01')
}
#df.stats <- getStats(ticker)

#Financials
getFinancials <- function(ticker){
        tq_get(ticker,get='financials')
}

getFinancials2 <- function(ticker){
        GetIncome(ticker,2017)
}
#df.financials <- getFinancials(ticker)

#Function for merging many datasets
merge.all <- function(by, ...) {
        frames <- list(...)
        return (Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames))
}  

QkeyFinancials <- function(df.financials,df.key.ratios,x) {
        #Financials Yearly
        df.Qfinancials <- df.financials$quarter
        QincomeStatement <- na.exclude(df.Qfinancials[[3]])
        
        QRevenue <- QincomeStatement[QincomeStatement$category == 'Revenue',3:4]
        colnames(QRevenue) <- c('Date','Revenue')
        
        QOpIncome <- QincomeStatement[QincomeStatement$category == 'Operating Income',3:4]
        colnames(QOpIncome) <- c('Date','Operating Income')
        
        QNetProfit <- QincomeStatement[QincomeStatement$category == 'Net Income',3:4]
        colnames(QNetProfit) <- c('Date','Net Profit')
        
        Qeps <- QincomeStatement[QincomeStatement$category == 'Diluted Normalized EPS',3:4]
        colnames(Qeps) <- c('Date','Annualized EPS')
        Qeps$`Annualized EPS` <- Qeps$`Annualized EPS` * 4
        Qeps$`EPS QOQ (%)` <- 0
        for(i in 1:nrow(Qeps)-1){
                Qeps$`EPS QOQ (%)`[i] <- percent(round((Qeps$`Annualized EPS`[i] - Qeps$`Annualized EPS`[i+1])/Qeps$`Annualized EPS`[i+1],digits = 4))
        }
        
        #Create P/E Based on Prices
        QfinIndicators <-data.table(tail(merge.all(by = 1, QRevenue, QOpIncome, QNetProfit,Qeps),4))
        setkey(QfinIndicators,'Date')
        setkey(x,'date')
        QfinIndicators <- x[,c(1,5)][QfinIndicators, roll=Inf][,c(1,3:7,2)] ## perform rolling join
        colnames(QfinIndicators)[1] <- 'Date'
        colnames(QfinIndicators)[7] <- 'Price'
        QfinIndicators$`P/E` <- QfinIndicators$Price / QfinIndicators$`Annualized EPS`
        
        #Format For Display
        QfinIndicators$`Net Profit` <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>$",format(QfinIndicators$`Net Profit` ,big.mark = ','),"M</span>")
        QfinIndicators$`Annualized EPS` <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>$",format(QfinIndicators$`Annualized EPS` ,big.mark = ','),"</span>")
        QfinIndicators$`EPS QOQ (%)` <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",QfinIndicators$`EPS QOQ (%)`,"</span>")
        QfinIndicators$`Operating Income` <- paste0("<span style='font-family: sans-serif;text-align: center;font-size: 11px; text-decoration: none; color:#2F2F2F'>$",format(QfinIndicators$`Operating Income`,big.mark = ','),"M</span>")
        QfinIndicators$Revenue <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>$",format(QfinIndicators$Revenue,big.mark = ','),"M</span>")
        QfinIndicators$`P/E` <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",round(QfinIndicators$`P/E`,1),"</span>")
        QfinIndicators$Date <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",QfinIndicators$Date,"</span>")
        QfinIndicators$Price <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",QfinIndicators$Price,"</span>")
        colnames(QfinIndicators) <- paste0('<span style="font-family: sans-serif;text-align: center; color:',c("#697068;","#697068;"),"font-size:11px",'">',colnames(QfinIndicators),'</span>')
        
        datatable(QfinIndicators,escape = FALSE,
                  options = list(autowidth=T,
                                 order = list(list(0, 'des')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:7)),
                                 sDom  = '<"top">lrt<"bottom">ip',  
                                 dom='ptl',
                                 "bLengthChange" = FALSE,
                                 "bInfo"=FALSE,
                                 "DataTables_Table_0_paginate"=FALSE,
                                 bPaginate = FALSE),
                  rownames= FALSE
                  
                  
        )
}


YkeyFinancials <- function(df.financials,df.key.ratios,x) {
        #Financials Yearly
        df.Yfinancials <- df.financials$annual
        YincomeStatement <- na.exclude(df.Yfinancials[[3]])
        
        YRevenue <- YincomeStatement[YincomeStatement$category == 'Revenue',3:4]
        colnames(YRevenue) <- c('Date','Revenue')
        YRevenue$Date <- year(YRevenue$Date)
        YRevenue$Revenue <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>$",format(YRevenue$Revenue,big.mark = ','),"M</span>")
        
        YOpIncome <- YincomeStatement[YincomeStatement$category == 'Operating Income',3:4]
        colnames(YOpIncome) <- c('Date','Operating Income')
        YOpIncome$Date <- year(YOpIncome$Date)
        YOpIncome$`Operating Income` <- paste0("<span style='font-family: sans-serif;text-align: center;font-size: 11px; text-decoration: none; color:#2F2F2F'>$",format(YOpIncome$`Operating Income`,big.mark = ','),"M</span>")
        
        YNetProfit <- YincomeStatement[YincomeStatement$category == 'Net Income',3:4]
        colnames(YNetProfit) <- c('Date','Net Profit')
        YNetProfit$Date <- year(YNetProfit$Date)
        YNetProfit$`Net Profit` <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>$",format(YNetProfit$`Net Profit` ,big.mark = ','),"M</span>")
        
        Yeps <- YincomeStatement[YincomeStatement$category == 'Diluted Normalized EPS',3:4]
        colnames(Yeps) <- c('Date','EPS')
        Yeps$`EPS YOY (%)` <- 0
        for(i in 1:nrow(Yeps)-1){
                Yeps$`EPS YOY (%)`[i] <- percent(round((Yeps$EPS[i] - Yeps$EPS[i+1])/Yeps$EPS[i+1],digits = 4))
        }
        Yeps$Date <- year(Yeps$Date)
        Yeps$EPS <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>$",format(Yeps$EPS ,big.mark = ','),"</span>")
        Yeps$`EPS YOY (%)` <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",Yeps$`EPS YOY (%)`,"</span>")
        
        Ype <- df.key.ratios$data[[7]]
        Ype <- na.exclude(Ype[Ype$category == 'Price to Earnings',c(4,5)])
        colnames(Ype) <- c('Date','P/E')
        Ype$Date <- as.Date(gsub(pattern = '30',x = Ype$Date,replacement = '31'))
        Ype$Date <- year(Ype$Date)
        Ype$`P/E` <- round(Ype$`P/E`,digits = 1)
        Ype$`P/E` <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",Ype$`P/E`,"</span>")
        
        YfinIndicators <-data.table(tail(merge.all(by = 1, YRevenue, YOpIncome, YNetProfit,Yeps,Ype),3))
        
        
        setkey(YfinIndicators,'Date')
        dateMerge <- data.table('Date' = as.Date(paste(YfinIndicators$Date,'-12-31',sep='')))
        setkey(dateMerge,'Date')
        setkey(x,'date')
        price <- x[,c(1,5)][dateMerge, roll=Inf] ## perform rolling join
        colnames(price)[1] <- 'Date'
        colnames(price)[2] <- 'Price'
        price$Date <- year(price$Date)
        YfinIndicators <- merge.all(by = 'Date',YfinIndicators,price)
        YfinIndicators$Date <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",YfinIndicators$Date,"</span>")
        YfinIndicators$Price <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",YfinIndicators$Price,"</span>")
        colnames(YfinIndicators) <- paste0('<span style="font-family: sans-serif;text-align: center; color:',c("#697068;","#697068;"),"font-size:11px",'">',colnames(YfinIndicators),'</span>')
        YfinIndicators <- YfinIndicators[,c(1,2,3,4,5,6,8,7)]
        datatable(YfinIndicators,escape = FALSE,
                  options = list(autowidth=T,
                                 order = list(list(0, 'des')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:7)),
                                 sDom  = '<"top">lrt<"bottom">ip',  
                                 dom='ptl',
                                 "bLengthChange" = FALSE,
                                 "bInfo"=FALSE,
                                 "DataTables_Table_0_paginate"=FALSE,
                                 bPaginate = FALSE),
                  rownames= FALSE
                  
                  
        )
}




