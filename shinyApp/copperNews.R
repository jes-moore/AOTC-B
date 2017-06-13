library(XML)
library(feedeR)
library(dplyr)
library(DT)
copperNewsDF <- function() {
        url1 <-
                "https://news.google.com/news/feeds?q=copper_price&output=rss&num=50" #max 30
        url2 <-
                "https://news.google.com/news/feeds?q=copper_supply&output=rss&num=50"
        url3 <-
                "https://news.google.com/news/feeds?q=copper_demand&output=rss&num=50"
        
        copper1 <- feed.extract(url1)
        copper2 <- feed.extract(url2)
        copper3 <- feed.extract(url3)
        #copper4 <- feed.extract(url4)
        
        copperTitles <- c(copper1$items$title,
                          copper2$items$title,
                          copper3$items$title)
        copperDates <- c(copper1$items$date,
                         copper2$items$date,
                         copper3$items$date)
        copperLinks <- c(copper1$items$link,
                         copper2$items$link,
                         copper3$items$link)
        df <- data.frame(Headlines = copperTitles,
                         Dates = copperDates,
                         Links = copperLinks)
        unique <- !duplicated(df$Headlines) #Remove duplicate rows
        df <- df[unique, ]
        df <- arrange(df, desc(Dates))
        df$Dates <- as.Date(df$Dates)
        rownames(df) <- NULL
        df$Headlines <-
                paste0(
                        "<a style='font-family: sans-serif;font-size: 10px; text-decoration: none; color:#2F2F2F' href='",
                        df$Links,
                        "'>",
                        df$Headlines,
                        "</a>"
                )
        df$Dates <-
                paste0(
                        "<span style='font-family: sans-serif;font-size: 10px; text-decoration: none; color:#2F2F2F'>",
                        df$Dates,
                        "</span>"
                )
        return(df)
        
}

copperNewsDT <- function() {
        copperDF <- copperNewsDF()[, 1:2]
        colnames(copperDF) <-
                paste0(
                        '<span style="font-family: sans-serif; color:',
                        c("#bf4015;", "#bf4015;"),
                        "margin-right: ",
                        c("140px !important;", "90px !important;"),
                        " font-size:11px",
                        '">',
                        c("Headlines", "Dates"),
                        '</span>'
                )
        rows <- c(1:nrow(copperDF))
        rows <-
                paste0(
                        '<span style="font-size:0px; color:rgba(5,5,5,0)"',
                        rownames(copperDF),
                        '</span>'
                )
        
        rownames(copperDF) <- rows
        
        d <- datatable(
                copperDF[, 1:2],
                escape = FALSE,
                style = "jqueryui",
                extensions = "Scroller",
                width = 500,
                caption =
                        htmltools::tags$caption(style = 'position:relative; bottom: -10px;  text-align: left; color:grey; font-size: 10px;',
                                                htmltools::p('Data Source: Google News')),
                options = list(
                        autowidth = T,
                        columnDefs = list(
                                list(
                                        className = 'td-left',
                                        width = '70%',
                                        targets = list(1)
                                ),
                                list(
                                        className = 'td-left',
                                        width = '30%',
                                        targets = list(2)
                                )
                        ),
                        sDom  = '<"top">lrt<"bottom">ip',
                        dom = 'ptl',
                        initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': 'rgba(47,47,46,0.2)', 'color': 'rgba(47,47,46,0.5)'});",
                                "}"
                        ),
                        "bLengthChange" = FALSE,
                        "bInfo" = FALSE,
                        "DataTables_Table_0_paginate" = FALSE,
                        pageLength = 5,
                        rownames = FALSE,
                        scrollY = 300,
                        scrollX = F,
                        caption = "hello",
                        bPaginate = FALSE
                )
        )
        
        d
}

