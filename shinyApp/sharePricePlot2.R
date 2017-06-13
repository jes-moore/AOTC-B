sharePricePlot2 <- function(data){

        dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
                dyPlugin(
                        dygraph = dygraph,
                        name = "Crosshair",
                        path = system.file("plugins/crosshair.js", package = "dygraphs"),
                        options = list(direction = match.arg(direction))
                )
        }
        
        plot <- dygraph(data[,c('Close','Volume')],
                       main = "", ylab = "Lots") %>%
                dySeries("Close",strokeWidth = 2, label = "Price",color = "#57B8FF") %>%
                dySeries("Volume",axis = 'y2' ,label = "Volume",color = "#2b2b2b",fillGraph = TRUE,strokeWidth = 0) %>%
                dyRangeSelector(height = 30,dateWindow = c(range(index(data))[2] - 180,range(index(data))[2])) %>%
                dyAxis("x",drawGrid = FALSE) %>%
                #dyAxis("y2", label = "Crude Price", independentTicks = FALSE) %>%
                dyAxis("y",drawGrid = FALSE,
                       valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                       axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
                dyCrosshair(direction = "vertical") %>%
                dyCSS("css/sp2.css") %>%
                dyLegend(show = "always",width = 600)
        return(plot)
}