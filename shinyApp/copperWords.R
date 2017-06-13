library(XML)
library(feedeR)
library(dplyr)
library(DT)
library(rvest)
library(tm)
library(SnowballC)
library(plotrix)
library(RWeka)
library(RColorBrewer)

copperNewsDF <- function(){
        url1 <- "https://news.google.com/news/feeds?q=copper_price&output=rss&num=50" #max 30
        url2 <- "https://news.google.com/news/feeds?q=copper_supply&output=rss&num=50"
        url3 <- "https://news.google.com/news/feeds?q=copper_demand&output=rss&num=50"
        
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
        df <- df[unique,]
        df <- arrange(df,desc(Dates))
        df$Dates <- as.Date(df$Dates)
        rownames(df) <- NULL
        df$daysBack <- as.integer(format(as.POSIXct(Sys.Date()),format="%j")) - 
                as.integer(strftime(df$Dates, format = "%j"))
        df$weeksBack <- 0
        df$weeksBack[df$daysBack<8] =1
        df$weeksBack[df$daysBack>7] =2
        df <- df[df$daysBack<15,]
        return(df)
        
}
linkContent <- function(df){
        df$Links <- as.character(df$Links)
        links <- df$Links
        df$text <- ""
        
        for(i in 1:length(links)){
                linkContent <- read_html(x = links[i])
                linkText <- linkContent %>% html_nodes("p") %>%html_text()
                linkParagraph <- paste0(linkText,collapse = '')
                df$text[i] <- linkParagraph
        }
        return(df)
}
clean_vector <- function(vector){
        vector <- removePunctuation(vector)
        vector <- removeNumbers(vector)
        vector <- removeWords(vector,c(stopwords("en"),stopwords("SMART"),
                                       "report","learn","click","bulletin","metal","dec","download","source","download",
                                       "free","freeget","get","freeclick","trial","address","website","visitor",
                                       "visit","update","user","reportclick",
                                       "information","success","investing","copper"))
        vector <- stripWhitespace(vector)
        return(vector)
}
createTDM <- function(df){
        df$text[1] <- clean_vector(df$text[1])
        df$text[2] <- clean_vector(df$text[2])
        
        dfSource <- VectorSource(df$text)
        dfCorpus <- VCorpus(dfSource)
        
        df$text[1] <- stemDocument(as.character(df$text[1]))
        df$text[2] <- stemDocument(as.character(df$text[2]))
        
        dfSource2 <- VectorSource(df$text)
        dfCorpus2 <- Corpus(dfSource2)
        
        options(mc.cores=1)
        tokenizer <- function(x){
                NGramTokenizer(x,Weka_control(min=2,max=2))
        }
        
        tdm <- TermDocumentMatrix(dfCorpus2, control = list(tokenize=tokenizer))
        
        completed = stemCompletion(rownames(tdm), dfCorpus)
        rownames(tdm) <- names(completed)
        return(tdm)   
        
}
thisWeekLastWeek <- function(){
        copperNews <- copperNewsDF()
        copperNews <- linkContent(copperNews)
        copperNews$text <- tolower(copperNews$text)
        
        copperNews1 <- copperNews[copperNews$weeksBack==1,]
        copperNews1 <- paste(copperNews1$text,collapse = " ")
        
        copperNews2 <-copperNews[copperNews$weeksBack==2,]
        copperNews2 <- paste(copperNews2$text,collapse = " ")
        
        copperCombined <- rbind(copperNews1,copperNews2)
        copperCombined <- as.data.frame(copperCombined)
        colnames(copperCombined) <- "text"
        copperCombined$text <- as.character(copperCombined$text)
        
        tdm <- createTDM(copperCombined)
        tdm <- as.matrix(tdm)
        return(tdm)
} 

tdm <- thisWeekLastWeek()
tdm <- tdm[tdm[,1]>0 & tdm[,2] >0,]
tdm <- as.data.frame(tdm)
tdm$difference <- abs(as.integer(tdm[,1]) - as.integer(tdm[,2]))
tdm <- tdm[order(tdm[,3],decreasing=TRUE),]
tdm <- tdm[(tdm$difference>1) & (tdm$difference<5),]

top25_df <- data.frame(x=tdm[1:20,1],y=tdm[1:20,2],labels=rownames(tdm[1:20,]))


pyramid.plot(top25_df$x,top25_df$y,labels=top25_df$labels,
             main=NULL,gap = 2.5,laxlab = c(0,5,10),raxlab = c(0,3,6),
             labelcex = 1,lxcol = "#E56900",rxcol = "#E56900",
             unit = NULL,top.labels = c("This Week","","Last Week"))


