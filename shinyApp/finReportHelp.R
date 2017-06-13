
library(data.table)
library(XBRL)
library(curl)

GetFinancialYearly <- function(ticker) {
        
        ##   This is here to please R CMD check
        description <- NULL
        roleId <- NULL
        labelRole <- NULL
        labelString <- NULL
        unitId <- NULL
        fact <- NULL
        contextId <- NULL
        startDate <- NULL
        endDate <- NULL
        
        

        ##   Function to download Instance Document
        GetInstFile <- function(url) {
                xbrlDoAll(url, cache.dir= 'xbrl.cache', prefix.out ="out", verbose=FALSE)
        }
        options(stringsAsFactors = FALSE,download.file.method = 'curl')
        this <- GetInstFile(url = 'https://www.sec.gov/Archives/edgar/data/1326801/000132680115000006/fb-20141231.xml')
        
        edgarFiles <- fread('data/tempEdgar.csv')
        edgarFiles$DATE_FILED <- as.Date(edgarFiles$DATE_FILED)
        urls <- edgarFiles$INST[edgarFiles$Ticker == toupper(ticker) &
                                        edgarFiles$FORM_TYPE == '10-K' &
                                        edgarFiles$reportPeriod > as.Date('2013-01-01')]
        
        ##   Download Instance Document
        instanceFiles <- lapply(urls,GetInstFile)
        
        
        GetInstFile(urls[1])
        
        ##   Clear Cache Dir
        file.remove("out_calculations.csv", "out_contexts.csv", "out_definitions.csv", 
                    "out_elements.csv", "out_facts.csv", "out_footnotes.csv", 
                    "out_labels.csv", "out_presentations.csv", "out_roles.csv", "out_units.csv")
        
        unlink("XBRLcache", recursive = TRUE)
        
        ##   Get Role ID from Instance Document
        role.df <- instFile$role %>%
                filter(toupper(description) %in% statement.type)
        
        role.id <- as.character(role.df$roleId)
        
        ##   Create statement template from Presentation Linkbase
        statement.skeleton <-
                instFile$presentation %>%
                filter(roleId == role.id)
        
        rowid <- c(1:nrow(statement.skeleton))
        statement.skeleton <- mutate(statement.skeleton, rowid = rowid)
        
        ##   Merge with Label Linkbase
        statement <-
                merge(statement.skeleton, instFile$label, by.x = "toElementId", 
                      by.y = "elementId") %>%
                filter(labelRole == "http://www.xbrl.org/2003/role/label")
        
        ##   Merge with Fact Linkbase
        statement <- merge(statement, instFile$fact, by.x = "toElementId", 
                           by.y = "elementId")
        
        ##   Merge with Context Linkbase
        statement <- merge(statement, instFile$context, by.x = "contextId", 
                           by.y = "contextId") %>%
                arrange(rowid)
        
        ##   Clean combined table
        statement <- subset(statement, is.na(statement$dimension1))
        
        clean.statement <- select(statement, labelString, unitId, fact, contextId, 
                                  startDate, endDate, rowid)
        clean.statement <- select(clean.statement, -contextId)
        
        colnames(clean.statement)[1] <- "Metric"
        colnames(clean.statement)[2] <- "Units"
        colnames(clean.statement)[3] <- "Amount"
        
        clean.statement <- arrange(clean.statement, rowid)
        clean.statement <- select(clean.statement, -rowid)
        return(clean.statement)
}