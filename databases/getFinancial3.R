library(curl)
library(dplyr)
library(XBRL)
library(finreportr)
library(data.table)
GetFinancial3 <- function(statement.type,symbol,quarter) {
        
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
        
        ##   Function to acquire Instance Document URL
        GetURL3 <- function(symbol,quarter) {
                instances <- fread('data/tempEdgar.csv')
                thisInst <- arrange(instances[instances$Ticker == toupper(symbol)],desc(reportPeriod))
                thisInst <- thisInst$INST[[quarter]]
                return(thisInst)
        }
        

        
        #   Function to download Instance Document
        GetInstFile <- function(url) {
                XBRL::xbrlDoAll(url,delete.cached.inst = TRUE, cache.dir=as.character(paste('/home/jmo/Desktop/AOTC-B/databases/data/xbrl.cache/',toupper(symbol),sep='')), prefix.out ="out", verbose=FALSE)
        }
        
        # GetInstFile <- function(url) {
        #         XBRL::xbrlDoAll(url,delete.cached.inst = TRUE, cache.dir=as.character(paste('data/xbrl.cache/',toupper(symbol),sep='')), prefix.out ="out", verbose=FALSE)
        # }
        
        inst.url <- GetURL3(symbol,quarter)
        
        ##   Check if url exits
        
        check <- tryCatch(is.list(httr::GET(inst.url)), error = function(e) {return(FALSE)})
        if(check == FALSE) {
                stop("no XBRL-format filings detected")
        }
        
        ##   Download Instance Document
        instFile <- GetInstFile(inst.url)
        
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

GetCashFlow3 <- function(symbol, quarter) {
        
        cash.flow.descriptions <- c("CONSOLIDATED STATEMENT OF CASH FLOWS",
                                    "CONDENSED CONSOLIDATED STATEMENT OF CASH FLOWS",
                                    "CONSOLIDATED STATEMENTS OF CASH FLOWS",
                                    "CONDENSED CONSOLIDATED STATEMENTS OF CASH FLOWS",
                                    "CASH FLOWS STATEMENTS",
                                    "CONDENSED CASH FLOWS STATEMENTS")
        
        GetFinancial3(cash.flow.descriptions, symbol, quarter)
        
}

GetBalanceSheet3 <- function(symbol, quarter) {
        
        balance.sheet.descriptions <- c("CONSOLIDATED BALANCE SHEET", 
                                        "CONDENSED CONSOLIDATED BALANCE SHEET", 
                                        "CONSOLIDATED BALANCE SHEETS",
                                        "CONDENSED CONSOLIDATED BALANCE SHEETS", 
                                        "CONSOLIDATED STATEMENT OF FINANCIAL POSITION",
                                        "CONDENSED CONSOLIDATED STATEMENT OF FINANCIAL POSITION", 
                                        "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION",
                                        "CONDENSED CONSOLIDATED STATEMENTS OF FINANCIAL POSITION",
                                        "BALANCE SHEETS",
                                        "CONDENSED BALANCE SHEETS")
        
        GetFinancial3(balance.sheet.descriptions, symbol, quarter)
}

GetIncome3 <- function(symbol, quarter) {
        
        income.descriptions <- c("CONSOLIDATED STATEMENTS OF INCOME",
                                 "CONDENSED CONSOLIDATED STATEMENTS OF INCOME",
                                 "CONSOLIDATED STATEMENT OF INCOME", 
                                 "CONDENSED CONSOLIDATED STATEMENT OF INCOME", 
                                 "CONSOLIDATED STATEMENTS OF OPERATIONS", 
                                 "CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS", 
                                 "CONSOLIDATED STATEMENT OF OPERATIONS", 
                                 "CONDENSED CONSOLIDATED STATEMENT OF OPERATIONS", 
                                 "CONSOLIDATED STATEMENT OF EARNINGS", 
                                 "CONDENSED CONSOLIDATED STATEMENT OF EARNINGS", 
                                 "CONSOLIDATED STATEMENTS OF EARNINGS",
                                 "CONDENSED CONSOLIDATED STATEMENTS OF EARNINGS",
                                 "INCOME STATEMENTS",
                                 "CONDENSED INCOME STATEMENTS")
        
        GetFinancial3(income.descriptions, symbol, quarter)
}


