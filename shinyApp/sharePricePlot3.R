sharePricePlot3 <- function(data){
        data <- na.exclude(data)
        data <- transform(data, date = as.numeric(as.Date(date)) * 86400000)
        a <- rCharts::Highcharts$new()
        a$yAxis(list(
                list(title = list(text = "Share Price"),max = max(data$close)*1.05,opposite = TRUE,height = 300,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "ADL"),opposite = FALSE, 
                     top = 325,offset = 0,height = 125,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "Volume"),opposite = TRUE, plotBands = list(color = "black",from = 100, to = 110), 
                     top = 475,offset = 0,height = 125,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                )
        )
        )
        
        a$xAxis(type = "datetime")
        
        a$series(type = "line",name = "Share Price",
                 data = toJSONArray2(data[,c("date","close")],json = F,names = F),
                 color = "orange",
                 yAxis = 0 
        )
        
        a$series(type = "line",name = "Parabolic SAR",
                 data = toJSONArray2(data[,c("date","sar1")],json = F,names = F) ,
                 lineWidth = 0,
                 marker = list(enabled = TRUE,radius = 3),
                 yAxis = 0
        )
        
        a$series(type = "line",name = "ADL",
                 data = toJSONArray2(data[,c("date","volume")],json = F,names = F),              
                 yAxis = 2
        )
        a$series(type = "line",name = "Volume",
                 data = toJSONArray2(data[,c("date","ADL")],json = F,names = F),
                 yAxis = 1              
        )
        
        
        a$tooltip(crosshairs = list(T,F),
                  shared=TRUE,
                  formatter = "#! function() { 
                   var d = new Date(this.x);
                   d = d.toLocaleDateString();
                   d = '<strong>' + d + '</strong>';
                   $.each(this.points,function(){
                   d += '<br>' + this.series.name + ': ' + '<strong>' + Highcharts.numberFormat(this.y,2,'.') + '</strong>';
                   })
                   return (d);
                   }!#"
        )       
        
        
        a$legend(align = "right",verticalAlign = "center", 
                 layout = "vertical",itemMarginTop = 10,x = 10,
                 floating = FALSE,backgroundColor = "white",
                 labelFormatter="#! function () {return this.name;}!#")
        
        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0.2,marker = list(enabled=FALSE)))
        a$chart(zoomType = 'x',width = 1000, height = 650,plotBorderWidth = 0.5,plotBorderColor="black")
        a
        
}