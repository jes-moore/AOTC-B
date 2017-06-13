sharePricePlot <- function(data){
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        ohlc <- dyCandlestick(dygraph(data[,1:4]) ,compress=TRUE) %>%
                dyRangeSelector(height = 30,dateWindow = c(range(index(data))[2] - 180,range(index(data))[2])) %>%
                dyCSS("css/stocks.css") %>%
                dyLegend(show = "always",width = 600) %>%
                dyAxis("y",drawGrid = FALSE) %>%
                dyCrosshair(direction = "vertical") 
        ohlc
        
        
        
}

