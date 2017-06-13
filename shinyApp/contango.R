library(Quandl)
library(lubridate)
library(rCharts)
library(quantmod)
library(plyr)
library(lubridate)
library(Quandl)
library(dygraphs)
library(htmlwidgets)

#Graphing Functions
COTGraph <- function(data){
        futures = data.frame("Front Month" = character(),"Price" = integer())
        cl1 = data.frame("Front_Month" = 1,"Price" = Quandl("CHRIS/CME_CL1", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl2 = data.frame("Front_Month" = 2,"Price" = Quandl("CHRIS/CME_CL2", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl3 = data.frame("Front_Month" = 3,"Price" = Quandl("CHRIS/CME_CL3", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl4 = data.frame("Front_Month" = 4,"Price" = Quandl("CHRIS/CME_CL4", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl5 = data.frame("Front_Month" = 5,"Price" = Quandl("CHRIS/CME_CL5", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl6 = data.frame("Front_Month" = 6,"Price" = Quandl("CHRIS/CME_CL6", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl7 = data.frame("Front_Month" = 7,"Price" = Quandl("CHRIS/CME_CL7", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl8 = data.frame("Front_Month" = 8,"Price" = Quandl("CHRIS/CME_CL8", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl9 = data.frame("Front_Month" = 9,"Price" = Quandl("CHRIS/CME_CL9", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl10 = data.frame("Front_Month" = 10,"Price" = Quandl("CHRIS/CME_CL10", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl11 = data.frame("Front_Month" = 11,"Price" = Quandl("CHRIS/CME_CL11", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl12 = data.frame("Front_Month" = 12,"Price" = Quandl("CHRIS/CME_CL12", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl13 = data.frame("Front_Month" = 13,"Price" = Quandl("CHRIS/CME_CL13", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        cl14 = data.frame("Front_Month" = 14,"Price" = Quandl("CHRIS/CME_CL14", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(1))[,5])
        
        futures = rbind(futures,cl1,cl2,cl3,cl4,cl5,cl6,cl7,cl8,cl9,cl10,cl11,cl12,cl13,cl14)
        
        futures1 = data.frame("Front Month" = character(),"Price" = integer())
        cl1 = data.frame("Front_Month" = 1,"Price" = Quandl("CHRIS/CME_CL1", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl2 = data.frame("Front_Month" = 2,"Price" = Quandl("CHRIS/CME_CL2", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl3 = data.frame("Front_Month" = 3,"Price" = Quandl("CHRIS/CME_CL3", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl4 = data.frame("Front_Month" = 4,"Price" = Quandl("CHRIS/CME_CL4", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl5 = data.frame("Front_Month" = 5,"Price" = Quandl("CHRIS/CME_CL5", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl6 = data.frame("Front_Month" = 6,"Price" = Quandl("CHRIS/CME_CL6", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl7 = data.frame("Front_Month" = 7,"Price" = Quandl("CHRIS/CME_CL7", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl8 = data.frame("Front_Month" = 8,"Price" = Quandl("CHRIS/CME_CL8", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl9 = data.frame("Front_Month" = 9,"Price" = Quandl("CHRIS/CME_CL9", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl10 = data.frame("Front_Month" = 10,"Price" = Quandl("CHRIS/CME_CL10", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl11 = data.frame("Front_Month" = 11,"Price" = Quandl("CHRIS/CME_CL11", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl12 = data.frame("Front_Month" = 12,"Price" = Quandl("CHRIS/CME_CL12", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl13 = data.frame("Front_Month" = 13,"Price" = Quandl("CHRIS/CME_CL13", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        cl14 = data.frame("Front_Month" = 14,"Price" = Quandl("CHRIS/CME_CL14", api_key="Xa-XyezxZxsEZpmhKYkt", start_date=Sys.Date()-days(90))[62,5])
        
        futures1 = rbind(futures1,cl1,cl2,cl3,cl4,cl5,cl6,cl7,cl8,cl9,cl10,cl11,cl12,cl13,cl14)
        compare <- cbind(futures,futures1$Price)
        
        colnames(compare) <- c('Front_Month','Current','90_Days_Back')
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        COT <- dygraph(compare,
                       main = "", ylab = "Price ($US)",xlab = 'Front Month') %>%
                dySeries("Current",strokeWidth = 2, label = "Current Curve",color = "#57B8FF") %>%
                dySeries("90_Days_Back",strokeWidth = 2, label = "Curve 3 Months Back",color = "#FE5D26") %>%
                #dySeries("Last",label = "Front Month WTI",strokeWidth = 2,strokePattern = "dashed",color = "#C67336",axis = 'y2') %>%
                #dyRangeSelector(height = 30,dateWindow = c(1,14)) %>%
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
                dyShading(from = 1, to = 14, color = "rgba(190,167,123,0.1)")
        return(COT)
}
COTGraph()


















