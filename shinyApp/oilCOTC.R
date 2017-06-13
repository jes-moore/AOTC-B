library(rCharts)
library(quantmod)
library(plyr)
library(lubridate)
library(Quandl)
library(dygraphs)
library(htmlwidgets)

#Graphing Functions
COTGraph <- function(){
        crudeCOT <- Quandl("CFTC/CL_F_ALL",type = "raw")
        crudeCOT$MMNet <- crudeCOT$`Money Manager Longs` - crudeCOT$`Money Manager Shorts`
        
        #prices <- Quandl("CHRIS/CME_CL1",type = "raw")
        #prices <- prices[,c("Date","Last")]
        
        #merged <- join(x=crudeCOT,y=prices,by = "Date",type = 'right')
        #merged <- merged[!is.na(merged$`Last`),]
        
        #crudeCOT <- merged
        crudeCOT <- xts(crudeCOT[,-1],order.by=crudeCOT[,1])
        
        #rm(merged)
        #rm(prices)
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        COT <- dygraph(crudeCOT[,c("Money Manager Longs","Money Manager Shorts","MMNet")],
                       main = "", ylab = "Lots") %>%
                dySeries("Money Manager Longs",strokeWidth = 2, label = "Longs",color = "#57B8FF") %>%
                dySeries("Money Manager Shorts",strokeWidth = 2, label = "Shorts",color = "#FE5D26") %>%
                dySeries("MMNet",label = "Net Position",color = "#2b2b2b",fillGraph = TRUE,strokeWidth = 0) %>%
                #dySeries("Last",label = "Front Month WTI",strokeWidth = 2,strokePattern = "dashed",color = "#C67336",axis = 'y2') %>%
                dyRangeSelector(height = 30,dateWindow = c(range(index(crudeCOT))[2] - 180,range(index(crudeCOT))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                #dyAxis("y2", label = "Crude Price", independentTicks = FALSE) %>%
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




