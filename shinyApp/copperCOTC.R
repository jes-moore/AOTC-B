library(quantmod)
library(plyr)
library(lubridate)
library(Quandl)
library(ggplot2)
library(scales)
library(dygraphs)
library(htmlwidgets)

#Graphing Functions
COTGraph <- function(){
        Quandl.api_key('Xa-XyezxZxsEZpmhKYkt')
        crudeCOT <- Quandl("CFTC/HG_FO_ALL",type = "raw")
        crudeCOT$MMNet <- crudeCOT$`Money Manager Longs` - crudeCOT$`Money Manager Shorts`
        
        prices <- Quandl("LME/PR_CU",type = "raw")
        prices <- prices[,c("Date","Cash Buyer")]
        
        prices$`Cash Buyer` <- signif(as.integer(prices$`Cash Buyer`)/2204.62,digits = 3)
        
        merged <- join(x=crudeCOT,y=prices,by = "Date")
        merged <- merged[!is.na(merged$`Cash Buyer`),]
        
        crudeCOT <- merged
        crudeCOT <- xts(crudeCOT[,-1],order.by=crudeCOT[,1])
        
        rm(merged)
        rm(prices)
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        COT <- dygraph(crudeCOT[,c("Money Manager Longs","Money Manager Shorts","MMNet","Cash Buyer")],
                       main = "", ylab = "Lots",group = 'lme') %>%
                dySeries("Money Manager Longs",strokeWidth = 2, label = "Longs",color = "#57B8FF") %>%
                dySeries("Money Manager Shorts",strokeWidth = 2, label = "Shorts",color = "#FE5D26") %>%
                dySeries("MMNet",label = "Net Position",color = "#2b2b2b",fillGraph = TRUE,strokeWidth = 0) %>%
                dySeries("Cash Buyer",label = "CU Price",strokeWidth = 2,strokePattern = "dashed",color = "#C67336",axis = 'y2') %>%
                dyRangeSelector(height = 30,dateWindow = c(range(index(crudeCOT))[2] - 180,range(index(crudeCOT))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                dyAxis("y2", label = "Copper Price", independentTicks = FALSE) %>%
                dyAxis("y",drawGrid = FALSE,
                       valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                       axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
                dyCrosshair(direction = "vertical") %>%
                dyCSS("css/COTC.css") %>%
                dyLegend(show = "always",width = 600) %>%
                dyEvent("2016-11-10", "Trump Elected", labelLoc = "bottom") %>%
                dyEvent("2017-01-20", "Trump Inauguration", labelLoc = "bottom") %>%
                dyShading(from = range(index(crudeCOT))[1], to = range(index(crudeCOT))[2], color = "rgba(190,167,123,0.1)")
                return(COT)
        }
LMEInvGraph <- function(){
        inventories <- Quandl("LME/ST_CU_ALL", type ="raw")
        
        prices <- Quandl("LME/PR_CU",type = "raw")
        prices <- prices[,c("Date","Cash Buyer")]
        prices$`Cash Buyer` <- signif(as.integer(prices$`Cash Buyer`)/2204.62,digits = 3)
        
        merged <- join(x=inventories,y=prices,by = "Date")
        merged <- merged[!is.na(merged$`Cash Buyer`),]
        
        inventories <- merged
        inventories <- xts(inventories[,-1],order.by=inventories[,1])
        
        rm(merged)
        rm(prices)
        
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        LME <- dygraph(inventories[,c("Closing Stock","Cash Buyer","Cancelled Tonnage")],
                       ylab = "Tonnes",group = 'lme') %>%
                dySeries("Closing Stock",strokeWidth = 2, label = "LME Inventory",color = "#FE5D26") %>%
                dySeries("Cash Buyer",label = "CU Price",strokeWidth = 2,strokePattern = "dashed",color = "#C67336",axis = 'y2') %>%
                dySeries("Cancelled Tonnage",label = "Cancelletions",color = "#2b2b2b",fillGraph = TRUE,strokeWidth = 0) %>%
                dyRangeSelector(height = 30,dateWindow = c(range(index(inventories))[2] - 180,range(index(inventories))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                dyAxis("y2", label = "Copper Price", independentTicks = FALSE) %>%
                dyAxis("y",drawGrid = FALSE,
                       valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                       axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
                dyCrosshair(direction = "vertical") %>%
                dyCSS("css/LME.css") %>%
                dyLegend(show = "always",width = 600) %>%
                dyEvent("2016-11-10", "Trump Elected", labelLoc = "bottom") %>%
                dyEvent("2017-01-20", "Trump Inauguration", labelLoc = "bottom") %>%
                dyShading(from = range(index(inventories))[1], to = range(index(inventories))[2], color = "rgba(190,167,123,0.1)")
                return(LME)
        }
BackwardationGraph <- function(){
        prices <- Quandl("LME/PR_CU",type = "raw")
        prices$spread <- prices$`Cash Buyer` - prices$`3-months Buyer`
        prices <- xts(prices[,-1],order.by=prices[,1])
        
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        contango <- dygraph(prices[,"spread"],
                            ylab = "$US/Tonne") %>%
                dySeries("spread",strokeWidth = 0, label = "LME 3 Month Spread",color = "#FE5D26",fillGraph = TRUE) %>%
                dyRangeSelector(height = 30,dateWindow = c(range(index(prices))[2] - 365,range(index(prices))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                dyAxis("y",drawGrid = FALSE,
                       valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                       axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
                dyCrosshair(direction = "vertical") %>%
                dyCSS("css/LME.css") %>%
                dyLegend(show = "always",width = 600) %>%
                dyEvent("2016-11-10", "Trump Elected", labelLoc = "bottom") %>%
                dyEvent("2017-01-20", "Trump Inauguration", labelLoc = "bottom") %>%
                dyShading(from = range(index(prices))[1], to = range(index(prices))[2], color = "rgba(190,167,123,0.1)") %>%
                dyLimit(0, label = "Backwardation Above ---- Contango Below", labelLoc = "left",
                        color = "black", strokePattern = "solid")
}

