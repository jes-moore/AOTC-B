technicalChart <- function(data,boll){
        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        if(2 %in% boll){
                ohlc <- dygraph(data[,c(9,10,11)]) %>%
                        dySeries("up",strokeWidth = 2, label = NULL,color = "#57B8FF") %>%
                        dySeries("dn",strokeWidth = 2, label = NULL,color = "#57B8FF") %>%
                        dySeries("mavg",strokeWidth = 2, label = NULL,color = "#57B8FF") %>%
                        dyRangeSelector(height = 30,dateWindow = c(range(index(data))[2] - 180,range(index(data))[2])) %>%
                        dyCSS("css/stocks.css") %>%
                        dyShading(from = range(index(data))[1], to = range(index(data))[2], color = "rgba(61,71,69,0.1)") %>%
                        dyLegend(show = "follow",width = 600) %>%
                        dyCrosshair(direction = "vertical") 
                ohlc
        }     
        
        
        
}

