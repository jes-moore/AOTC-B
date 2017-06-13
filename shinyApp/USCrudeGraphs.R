library(xts)
library(dygraphs)

importsGraph <- function(){
        #con <- dbConnect(MySQL(),host="mysql3.gear.host", dbname='oilimports',user="oilimports",pass="Ar3GEuhb_g~0")
        #oilImports <- dbReadTable(con,name='oilimports') #Need to store this in a datawarehouse
        oilImports <- read.csv('data/oilimports.csv',row.names = 1)
        oilImports[,1] <- as.Date(oilImports[,1])
        oilImports[,2] <- as.integer(oilImports[,2])
        oilImports[,3] <- as.integer(oilImports[,3])
        oilImports <- xts(oilImports[,-1],order.by=oilImports[,1])
        
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        importsGraph <- dygraph(oilImports,group = 'imports',
                       main = "", ylab = "000's Of Barrels") %>%
                dySeries("Weekly_US_Net_Imports_000s",strokeWidth = 2, label = "Net Imports",color = "#57B8FF") %>%
                dySeries("Weekly_US_Total_Imports_000s",strokeWidth = 2, label = "Total Imports",color = "#FE5D26") %>%
                dyRangeSelector(height = 15,dateWindow = c(range(index(oilImports))[2] - 180,range(index(oilImports))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                #dyAxis("y2", label = "Crude Price", independentTicks = FALSE) %>%
                dyAxis("y",drawGrid = FALSE,
                       valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                       axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
                dyCrosshair(direction = "vertical") %>%
                dyCSS("css/COTC.css") %>%
                dyLegend(show = "always",width = 600) %>%
                dyShading(from = range(index(oilImports))[1], to = range(index(oilImports))[2], color = "rgba(190,167,123,0.1)")
        
        return(importsGraph)
}

supplyGraph <- function(){
        #con <- dbConnect(MySQL(),host="mysql3.gear.host", dbname='oilimports',user="oilimports",pass="Ar3GEuhb_g~0")
        #oilSupply <- dbReadTable(con,name='oilsupply') #Need to store this in a datawarehouse
        oilSupply <-read.csv('data/oilsupply.csv',row.names = 1)
        oilSupply[,1] <- as.Date(oilSupply[,1])
        oilSupply[,2] <- as.integer(oilSupply[,2])
        oilSupply <- xts(oilSupply[,-1],order.by=oilSupply[,1])
        
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        supplyGraph <- dygraph(oilSupply,group = 'supply',
                               main = "", ylab = "000's Of Barrels") %>%
                dySeries("V1",strokeWidth = 2, label = "US Crude Production",color = "#57B8FF") %>%
                dyRangeSelector(height = 15,dateWindow = c(range(index(oilSupply))[2] - 180,range(index(oilSupply))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                #dyAxis("y2", label = "Crude Price", independentTicks = FALSE) %>%
                dyAxis("y",drawGrid = FALSE,
                       valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                       axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
                dyCrosshair(direction = "vertical") %>%
                dyCSS("css/COTC.css") %>%
                dyLegend(show = "always",width = 600) %>%
                dyShading(from = range(index(oilSupply))[1], to = range(index(oilSupply))[2], color = "rgba(190,167,123,0.1)")
        
        return(supplyGraph)
}

importsAvgGraph <- function(){
        #con <- dbConnect(MySQL(),host="mysql3.gear.host", dbname='oilimports',user="oilimports",pass="Ar3GEuhb_g~0")
        #oilImportsAvg <- dbReadTable(con,name='oilimportsavg') #Need to store this in a datawarehouse
        oilImportsAvg <- read.csv('data/oilimportsavg.csv',row.names = 1)
        oilImportsAvg[,1] <- as.Date(oilImportsAvg[,1])
        oilImportsAvg[,2] <- as.integer(oilImportsAvg[,2])
        oilImportsAvg[,3] <- as.integer(oilImportsAvg[,3])
        oilImportsAvg <- xts(oilImportsAvg[,-1],order.by=oilImportsAvg[,1])
        
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        importsAvgGraph <- dygraph(oilImportsAvg,group = 'imports',
                                main = "", ylab = "000's Of Barrels") %>%
                dySeries("X4WkAvg_US_Net_Imports_000s",strokeWidth = 2, label = "4Wk Avg Net Imports",color = "#57B8FF") %>%
                dySeries("X4WkAvg_US_Total_Imports_000s",strokeWidth = 2, label = "4Wk Avg Total Imports",color = "#FE5D26") %>%
                dyRangeSelector(height = 15,dateWindow = c(range(index(oilImportsAvg))[2] - 180,range(index(oilImportsAvg))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                #dyAxis("y2", label = "Crude Price", independentTicks = FALSE) %>%
                dyAxis("y",drawGrid = FALSE,
                       valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                       axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
                dyCrosshair(direction = "vertical") %>%
                dyCSS("css/COTC.css") %>%
                dyLegend(show = "always",width = 600) %>%
                dyShading(from = range(index(oilImportsAvg))[1], to = range(index(oilImportsAvg))[2], color = "rgba(190,167,123,0.1)")
       
        return(importsAvgGraph)
}

oilStocks <- function(){
        #con <- dbConnect(MySQL(),host="mysql3.gear.host", dbname='oilimports',user="oilimports",pass="Ar3GEuhb_g~0")
        #oilStocks <- dbReadTable(con,name='oilstocks') #Need to store this in a datawarehouse
        oilStocks <- read.csv('data/oilstocks.csv',row.names = 1)
        oilStocks[,1] <- as.Date(oilStocks[,1])
        oilStocks[,2] <- as.integer(oilStocks[,2])
        oilStocks <- xts(oilStocks[,-1],order.by=oilStocks[,1])
        
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        oilStocks <- dygraph(oilStocks,group = 'supply',
                                   main = "", ylab = "000's Of Barrels") %>%
                dySeries("V1",strokeWidth = 2, label = "Weekly Ending Crude Stocks 000s",color = "#57B8FF") %>%
                dyRangeSelector(height = 15,dateWindow = c(range(index(oilStocks))[2] - 180,range(index(oilStocks))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                #dyAxis("y2", label = "Crude Price", independentTicks = FALSE) %>%
                dyAxis("y",drawGrid = FALSE,
                       valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                       axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
                dyCrosshair(direction = "vertical") %>%
                dyCSS("css/COTC.css") %>%
                dyLegend(show = "always",width = 600) %>%
                dyShading(from = range(index(oilStocks))[1], to = range(index(oilStocks))[2], color = "rgba(190,167,123,0.1)")
        return(oilStocks)
}



