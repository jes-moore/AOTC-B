library(zoo)
library(data.table)
library(quantmod)
library(tidyquant)
library(dplyr)
library(lubridate)
library(shiny)
library(DT)
library(reshape2)
library(xts)
library(curl)
library(dygraphs)
library(RMySQL)
library(scales)
shinyServer(function(input, output,session){
        #selectable tickers
        stockTickers <- as.vector(read.csv('data/tickers.csv'))
        names(stockTickers) <- 'Tickers'
        
        output$thisTicker <- renderText(paste(input$ticker,'(Update Ticker On First Tab)'))
        output$Yearly_Financials <- renderText('Yearly Financials')
        output$Quarterly_Financials <- renderText('Quarterly Financials')
        
        #Loads the data based on selected share price and date range
        input_data <- reactive({
                req(input$ticker %in% stockTickers[[1]])

                withProgress(message = 'Downloading Stock Data', value = 0, {
                        for (i in 1:15) {
                                incProgress(1/15)
                                Sys.sleep(0.01)
                        }
                })
                
                #
                if(input$split == TRUE){
                        apiCall <- paste("https://www.quandl.com/api/v3/datatables/WIKI/PRICES.csv?date.gte=20000101&ticker=", input$ticker,
                                  "&qopts.columns=date,adj_open,adj_high,adj_low,adj_close,volume,ex-dividend&api_key=Xa-XyezxZxsEZpmhKYkt",sep="")
                }
                if(input$split == FALSE){
                        apiCall <- paste("https://www.quandl.com/api/v3/datatables/WIKI/PRICES.csv?date.gte=20000101&ticker=", input$ticker,
                                         "&qopts.columns=date,open,high,low,close,volume,ex-dividend&api_key=Xa-XyezxZxsEZpmhKYkt",sep="")
                }

                x <- fread(apiCall)
                if(nrow(x) < 1){
                        x <- tq_get(input$ticker,'stock.prices')
                }
                #x <- tq_get(input$ticker,get='stock.prices')
                
                x$date <- as.Date(x$date)
                return(x)
                })
        #Takes input data, runs through the creation of ma's and other trading indicators, outputs df
        cutdata <- reactive({
                x <- input_data()
                withProgress(message = 'Computing Indicators', value = 0, {
                        for (i in 1:15) {
                                incProgress(1/15)
                                Sys.sleep(0.01)
                        }
                })
                colnames(x) <- c("Date","Open","High","Low","Close","Volume",'Dividend')
                x <- x[,1:6]
                x$MA <- SMA(x = x$Close,n=input$smaval)
                x$MA <- round(x$MA,2)
                x$EMA <- EMA(x = x$Close,n=input$emaval)
                x$EMA <- round(x$EMA,2)
                #boll <- BBands(HLC = x[,c(3,4,5)],n = input$bollval)
                #x <- cbind(x,boll)
                x$RSI <- RSI(x$Close)
                x$EMA12 <- EMA(x = x$Close,n = 12)
                x$EMA26 <- EMA(x = x$Close,n = 26)
                x$MACD <- x$EMA12 - x$EMA26
                x$SIGNAL <- EMA(x = x$MACD,n = 9)
                mfi <- MFI(HLC = x[,c("High","Low","Close")],x[,"Volume"],n=14)
                x <- cbind(x,mfi)
                #x <- x[(x$Date >= as.Date(input$dates[1],format="%Y-%m-%d")) & (x$Date <= as.Date(input$dates[2],format="%Y-%m-%d")),]
                x$Date <- as.Date(x$Date,format="%Y-%m-%d")
                x <- na.exclude(x)
                x <- xts(x[,-1],x$Date)
                return(x)
                })
        
        # Fundamental Analysis
        output$stockNews <- DT::renderDataTable({
                source('stockNews.R')
                withProgress(message = 'Downloading News', value = 0, {
                        for (i in 1:15) {
                                incProgress(1/15)
                                Sys.sleep(0.01)
                        }
                })
                newsDF <- stockNewsDF(input$ticker)
                return(newsDF)
        })
        source('finreports.R')
        df.key.ratios <- reactive({
                return(getRatios(input$ticker))
                })
        profitability <- reactive({
                df.key.ratios <- df.key.ratios()
                return(df.key.ratios$data[[2]])
                })
        df.stats <- reactive({
                return(getStats(input$ticker))
        })
        df.financials <- reactive({
                return(getFinancials(input$ticker))
        })
        output$YFinancials <- DT::renderDataTable({
                data <- input_data()
                withProgress(message = 'Gathering Financial Data', value = 0, {
                        for (i in 1:15) {
                                incProgress(1/15)
                                Sys.sleep(0.01)
                        }
                })
                return(YkeyFinancials(df.financials(),df.key.ratios(),data))
        })
        output$QFinancials <- DT::renderDataTable({
                data <- input_data()
                withProgress(message = 'Gathering Financial Data', value = 0, {
                        for (i in 1:15) {
                                incProgress(1/15)
                                Sys.sleep(0.01)
                        }
                })
                return(QkeyFinancials(df.financials(),df.key.ratios(),data))
        })
        #End Income reports

        
        
        # Stock Graphs
        output$candlestick<- renderDygraph({
                source('sharePricePlot.R')
                data <- cutdata()
                chart <- sharePricePlot(data)
                return(chart)
        })
        
        output$sharePrice2<- renderDygraph({
                source('sharePricePlot2.R')
                data <- cutdata()
                chart <- sharePricePlot2(data)
                return(chart)
        })
        output$technicals<- renderDygraph({
                source('technicalChart.R')
                data <- cutdata()
                chart <- technicalChart(data)
                return(chart)
        })

        # Copper Graphs
        output$COTGraph<- renderDygraph({
                source('copperCOTC.R')
                chart <- COTGraph()
                return(chart)
        })
        output$LMEInvGraph<- renderDygraph({
                source('copperCOTC.R')
                chart <- LMEInvGraph()
                return(chart)
        })
        output$BackwardationGraph<- renderDygraph({
                source('copperCOTC.R')
                chart <- BackwardationGraph()
                return(chart)
        })
        #Copper News Analysis
        output$newsDF <- DT::renderDataTable({
                source('copperNews.R')
                newsDF <- copperNewsDT()
                return(newsDF)
        })
        
        # Crude Oil GRaphs
        output$importsGraph<- renderDygraph({
                source('USCrudeGraphs.R')
                chart <- importsGraph()
                return(chart)
        })
        output$oilStocks<- renderDygraph({
                source('USCrudeGraphs.R')
                chart <- oilStocks()
                return(chart)
                })
        output$supplyGraph<- renderDygraph({
                source('USCrudeGraphs.R')
                chart <- supplyGraph()
                return(chart)
                })
        output$importsAvgGraph<- renderDygraph({
                source('USCrudeGraphs.R')
                chart <- importsAvgGraph()
                return(chart)
                })
        
})








