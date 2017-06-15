library(finreportr)
library(XBRL)
library(edgar)
library(rvest)
library(htmltidy)
library(httr)
library(dplyr)
library(data.table)
library(edgar)
library(XML)

#Get CIK's to match with our tickers
cik <- read.csv('../shinyApp/data/cik_ticker.csv')

#These functions download the master list of all filings
#getDailyMaster(2015)
#getDailyMaster(2016)
#Needs to be updated as time passes
getMasterIndex(2016)
getMasterIndex(2017)

#################################################################################
### Grabs all individual year RDA files and combines them with necessary data ###
#################################################################################

# object1 <- load("Master Index/2013master.Rda")
# obj <- year.master
# rm(object1)
# object2 <- load("Master Index/2014master.Rda")
# obj <- rbind(obj,year.master)
# rm(object2)
# object3 <- load("Master Index/2015master.Rda")
# obj <- rbind(obj,year.master)
# rm(object3)
object4 <- load("Master Index/2016master.Rda")
obj <- year.master
rm(object4)
obj <- rbind(obj,year.master)
object5 <- load("Master Index/2017master.Rda")
rm(object5)
obj <- rbind(obj,year.master)
rm(year.master)

obj <- obj[obj$CIK %in% cik$CIK,]
obj <- obj[obj$FORM_TYPE %in% c('10-Q','10-K'),]

obj <- merge(obj,cik[,2:3],by='CIK')
obj$EDGAR_LINK <- paste('https://www.sec.gov/Archives/',obj$EDGAR_LINK,sep='')

obj$html <- paste('https://www.sec.gov/Archives/edgar/data/',obj$CIK,
                  '/',
                  gsub('.txt','',gsub('-','',tstrsplit(x = obj$EDGAR_LINK,'/')[[8]])),
                  '/',
                  gsub('.txt','',tstrsplit(x = obj$EDGAR_LINK,'/')[[8]]),
                  '-index.htm',sep='')


ReportPeriod <- function(url) {
        
        # url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
        #               accession.no, "/", accession.no.raw, "-index.htm")
        
        search.result <- xml2::read_html(url)
        
        ##   Generic function to extract info
        ExtractInfo <- function(html.node) {
                info <-
                        search.result %>%
                        rvest::html_nodes(html.node) %>%
                        rvest::html_text()
                return(info)
        }
        
        report.period <- ExtractInfo(".formGrouping+ .formGrouping .info:nth-child(2)")
        return(report.period)
}
obj$reportPeriod <- 0
library(doParallel)  
cl <- makeCluster(4)  
registerDoParallel(cl)  
reportPeriod <- data.frame(reportPeriod = character)
reportPeriod = foreach(i=1:nrow(obj),.export = c('ReportPeriod'),.packages='dplyr'  ,.combine='rbind') %dopar% {  
        
        data.frame(reportPeriod = ReportPeriod(obj$html[i]))
}
stopCluster(cl)  
for(i in 12438:nrow(obj)){
        obj$reportPeriod[i] <- ReportPeriod(obj$html[i])
        print(i)
        
}

obj$INST <- paste('https://www.sec.gov/Archives/edgar/data/',
                  obj$CIK,'/',
                  gsub('.txt','',gsub('-','',tstrsplit(x = obj$EDGAR_LINK,'/')[[8]])),'/',
                  tolower(obj$Ticker),'-',
                  gsub('-','',obj$reportPeriod),'.xml',sep='')

#Cache results so far
write.csv(obj,'data/tempEdgar.csv',row.names = FALSE)
sp

