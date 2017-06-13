library(XML)
library(feedeR)
library(dplyr)
library(DT)
stockNewsDF <- function(ticker){
        url1 <- paste("https://news.google.com/news/feeds?q=",ticker,"+stock&output=rss&num=25",sep = '')

        newsDF <- feed.extract(url1)
        newsTitles <- newsDF$items$title
        newsDates <- newsDF$items$date
        newsLinks <- newsDF$items$link
        
        df <- data.frame(Headlines = newsTitles,
                         Dates = newsDates,
                         Links = newsLinks)
        
        unique <- !duplicated(df$Headlines) #Remove duplicate rows
        df <- df[unique,]
        df <- arrange(df,desc(Dates))
        df$Dates <- as.Date(df$Dates)
        rownames(df) <- NULL
        df$Headlines <- paste0("<a style='font-family: sans-serif;font-size: 11px; text-decoration: none; color:#2F2F2F' href='",df$Links,"'>",df$Headlines,"</a>")
        df$Dates <- paste0("<span style='font-family: sans-serif;font-size: 11px;text-align: center; text-decoration: none; color:#2F2F2F'>",df$Dates,"</span>")
        colnames(df) <- paste0('<span style="font-family: sans-serif;text-align: ',c("center;","left;"),' color:',c("#697068;","#697068;"),
                                     " font-size:11px",'">',c("Headlines","Dates"),'</span>')
        
        rows <- c(1:nrow(df))
        rows <- paste0('<span style="font-size:0px; color:rgba(5,5,5,0)"',rownames(df),'</span>')
        
        rownames(df) <- rows
        
        d <- datatable(df[,1:2],escape=FALSE,
                       extensions = "Scroller",
                       caption =  
                               htmltools::tags$caption(
                                       style = 'position:relative; bottom: -10px;  text-align: left; color:grey; font-size: 10px;',
                                       htmltools::p('Data Source: Google News'
                                       )
                               ),
                       options = list(
                               autowidth=T,
                               columnDefs = list(
                                       list(className ='td-left',width = '80%', targets = c(1)),
                                       list(className ='dt-center',width = '20%', targets = c(2))
                                       
                                       ),
                               sDom  = '<"top">lrt<"bottom">ip',  
                               dom='ptl',
                               # initComplete = JS(
                               #         "function(settings, json) {",
                               #         "$(this.api().table().header()).css({'background-color': 'rgba(47,47,46,0.2)', 'color': 'rgba(227,227,232,1)'});",
                               #         "}"),
                               "bLengthChange" = FALSE, "bInfo"=FALSE,
                               "DataTables_Table_0_paginate"=FALSE,
                               rownames= FALSE,
                               scrollY = 300,
                               scrollX = F,
                               bPaginate = FALSE
                       )
        )
        d
}










