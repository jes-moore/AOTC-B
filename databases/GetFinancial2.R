library(curl)
library(dplyr)
GetFinancial2 <- function(statement.type, symbol, year) {
        
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
        GetURL <- function(symbol, year) {
                
                lower.symbol <- tolower(symbol)
                
                accession.no.raw <- GetAccessionNo(symbol, year, foreign = FALSE)
                accession.no <- gsub("-", "" , accession.no.raw)
                
                CIK <- CompanyInfo(symbol)
                CIK <- as.numeric(CIK$CIK)
                
                report.period <- ReportPeriod(symbol, CIK, accession.no, accession.no.raw)
                report.period <- gsub("-", "" , report.period)
                
                inst.url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                                   accession.no, "/", lower.symbol, "-", report.period, ".xml")
                return(inst.url)
        }
        
        
        ##   Function to download Instance Document
        GetInstFile <- function(url) {
                XBRL::xbrlDoAll(url,delete.cached.inst = TRUE, cache.dir=as.character(paste('/home/jmo/Desktop/AOTC-B/databases/data/xbrl.cache/',toupper(symbol),sep='')), prefix.out ="out", verbose=FALSE)
        }
        
        inst.url <- GetURL(symbol, year)
        
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





GetCashFlow2 <- function(symbol, year) {
        
        cash.flow.descriptions <- c("CONSOLIDATED STATEMENT OF CASH FLOWS", 
                                    "CONSOLIDATED STATEMENTS OF CASH FLOWS",
                                    "CASH FLOWS STATEMENTS")
        
        GetFinancial2(cash.flow.descriptions, symbol, year)
        
}

GetBalanceSheet2 <- function(symbol, year) {
        
        balance.sheet.descriptions <- c("CONSOLIDATED BALANCE SHEET", 
                                        "CONSOLIDATED BALANCE SHEETS", 
                                        "CONSOLIDATED STATEMENT OF FINANCIAL POSITION", 
                                        "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION",
                                        "BALANCE SHEETS")
        
        GetFinancial2(balance.sheet.descriptions, symbol, year)
}

GetIncome2 <- function(symbol, year) {
        
        income.descriptions <- c("CONSOLIDATED STATEMENTS OF INCOME", 
                                 "CONSOLIDATED STATEMENT OF INCOME", 
                                 "CONSOLIDATED STATEMENTS OF OPERATIONS", 
                                 "CONSOLIDATED STATEMENT OF OPERATIONS", 
                                 "CONSOLIDATED STATEMENT OF EARNINGS", 
                                 "CONSOLIDATED STATEMENTS OF EARNINGS",
                                 "INCOME STATEMENTS")
        
        GetFinancial2(income.descriptions, symbol, year)
}

GetAccessionNo <- function(symbol, year, foreign = FALSE) {
        
        ##   This is here to please R CMD check
        filing.year <- NULL
        filing.name <- NULL
        accession.no <- NULL
        
        year.char <- as.character(year)
        
        reports.df <- AnnualReports(symbol, foreign)
        reports.df <-
                mutate(reports.df, filing.year = substr(reports.df$filing.date,1,4) ) %>%
                filter(filing.year == year.char) %>%
                filter(filing.name == "10-K" | filing.name == "20-F")
        
        accession.no.raw <-
                select(reports.df, accession.no) %>%
                as.character()
        
        ##   Error message for function
        if(accession.no.raw == "character(0)") {
                stop("no filings available for given year")
        }
        
        return(accession.no.raw)
}

################################################################################
#####     Backend Utility: Extract Period of Report                        #####
################################################################################


ReportPeriod <- function(symbol, CIK, accession.no, accession.no.raw) {
        
        url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                      accession.no, "/", accession.no.raw, "-index.htm")
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
