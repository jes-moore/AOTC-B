library(curl)
library(dplyr)
library(XBRL)
library(finreportr)
library(data.table)
GetFinancial3 <- function(statement.type,symbol,quarter) {
        
        if(symbol == 'AFL'){
                stop()
        }
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
        
        # Function to download Instance Document
        GetInstFile <- function(url) {
                XBRL::xbrlDoAll(url,delete.cached.inst = TRUE, cache.dir=as.character(paste('/home/jmo/Desktop/AOTC-B/databases/data/xbrl.cache/',toupper(symbol),sep='')), prefix.out ="out", verbose=FALSE)

        }
        
        
        
        inst.url <- try(GetURL3(symbol,quarter))
        if(class(inst.url) == 'try-error'){
                error <- read.csv('/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',stringsAsFactors = FALSE)
                newError <- data.frame(Ticker = symbol,Quarter = quarter,Error = as.character(inst.url))
                error <- rbind(error,newError)
                write.csv(file = '/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',error,row.names = FALSE)
                rm(error,newError)
        }
        
        ##   Check if url exits
        
        check <- tryCatch(is.list(httr::GET(inst.url)), error = function(e) {return(FALSE)})
        if(check == FALSE) {
                error <- read.csv('/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',stringsAsFactors = FALSE)
                newError <- data.frame(Ticker = symbol,Error = "no XBRL-format filings detected")
                error <- rbind(error,newError)
                write.csv(file = '/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',error,row.names = FALSE)
                rm(error,newError)
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
        role.df <- try(instFile$role %>%
                filter(toupper(description) %in% toupper(statement.type)))
        if(class(role.df) == 'try-error'){
                error <- read.csv('/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',stringsAsFactors = FALSE)
                newError <- data.frame(Ticker = symbol,Quarter = quarter,Error = 'Missing Role Description')
                error <- rbind(error,newError)
                write.csv(file = '/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',error,row.names = FALSE)
                rm(error,newError)
        }
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
                                    "CONSOLIDATED CASH FLOWS STATEMENTS",
                                    "CONSOLIDATED STATEMENT OF CASH FLOWS (UNAUDITED)",
                                    "CONDENSED CONSOLIDATED STATEMENT OF CASH FLOWS",
                                    "CONDENSED CONSOLIDATED STATEMENT OF CASH FLOWS",
                                    "CONDENSED CONSOLIDATED STATEMENT OF CASH FLOWS (UNAUDITED)",
                                    "CONSOLIDATED STATEMENTS OF CASH FLOWS", 
                                    "CONSOLIDATED STATEMENTS OF CASH FLOWS (UNAUDITED)",
                                    "CONDENSED CONSOLIDATED STATEMENTS OF CASH FLOWS",
                                    "CONDENSED CONSOLIDATED STATEMENTS OF CASH FLOWS (UNAUDITED)",
                                    "CASH FLOWS STATEMENTS",
                                    "CASH FLOWS STATEMENTS (UNAUDITED)",
                                    "CONDENSED CASH FLOWS STATEMENTS",
                                    "CONDENSED CASH FLOWS STATEMENTS (UNAUDITED)",
                                    "Statements of Cash Flows",
                                    "CONSOLIDATED STATEMENT OF CASH FLOWS",
                                    "CONSOLIDATED STATEMENT OF CASH FLOWS (UNAUDITED)",
                                    "CONSOLIDATED CONDENSED STATEMENT OF CASH FLOWS",
                                    "CONSOLIDATED CONDENSED STATEMENT OF CASH FLOWS",
                                    "CONSOLIDATED CONDENSED STATEMENT OF CASH FLOWS (UNAUDITED)",
                                    "CONSOLIDATED CONDENSED STATEMENTS OF CASH FLOWS",
                                    "CONSOLIDATED CONDENSED STATEMENTS OF CASH FLOWS (UNAUDITED)",
                                    "Unaudited Consolidated Statements of Cash Flows")
        out <- try(GetFinancial3(cash.flow.descriptions, symbol, quarter))
        if(class(out) == 'try-error'){
                error <- read.csv('/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',stringsAsFactors = FALSE)
                newError <- data.frame(Ticker = symbol,Quarter = quarter,Error = 'Failed To Get Cash Flow')
                error <- rbind(error,newError)
                write.csv(file = '/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',error,row.names = FALSE)
                rm(error,newError)
        } else return(out)
        
}

GetBalanceSheet3 <- function(symbol, quarter) {
        
        balance.sheet.descriptions <- c("CONSOLIDATED BALANCE SHEET (UNAUDITED)", 
                                        "CONSOLIDATED BALANCE SHEET", 
                                        "CONDENSED CONSOLIDATED BALANCE SHEET (UNAUDITED)", 
                                        "CONDENSED CONSOLIDATED BALANCE SHEET", 
                                        "CONSOLIDATED BALANCE SHEETS (UNAUDITED)",
                                        "CONSOLIDATED BALANCE SHEETS",
                                        "CONDENSED CONSOLIDATED BALANCE SHEETS (UNAUDITED)", 
                                        "CONDENSED CONSOLIDATED BALANCE SHEETS", 
                                        "CONSOLIDATED STATEMENT OF FINANCIAL POSITION (UNAUDITED)",
                                        "CONSOLIDATED STATEMENT OF FINANCIAL POSITION",
                                        "CONDENSED CONSOLIDATED STATEMENT OF FINANCIAL POSITION (UNAUDITED)", 
                                        "CONDENSED CONSOLIDATED STATEMENT OF FINANCIAL POSITION (LOSS) (UNAUDITED)", 
                                        "CONDENSED CONSOLIDATED STATEMENT OF FINANCIAL POSITION", 
                                        "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION (UNAUDITED)",
                                        "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION",
                                        "CONDENSED CONSOLIDATED STATEMENTS OF FINANCIAL POSITION (UNAUDITED)",
                                        "CONDENSED CONSOLIDATED STATEMENTS OF FINANCIAL POSITION",
                                        "BALANCE SHEETS (UNAUDITED)",                                        
                                        "BALANCE SHEETS",                                        
                                        "CONDENSED BALANCE SHEETS (UNAUDITED)",
                                        "CONDENSED BALANCE SHEETS",
                                        "CONSOLIDATED CONDENSED BALANCE SHEET (UNAUDITED)", 
                                        "CONSOLIDATED CONDENSED BALANCE SHEET", 
                                        "CONSOLIDATED CONDENSED BALANCE SHEETS (UNAUDITED)", 
                                        "CONSOLIDATED CONDENSED BALANCE SHEETS", 
                                        "CONSOLIDATED CONDENSED STATEMENT OF FINANCIAL POSITION (UNAUDITED)", 
                                        "CONSOLIDATED CONDENSED STATEMENT OF FINANCIAL POSITION (LOSS) (UNAUDITED)", 
                                        "CONSOLIDATED CONDENSED STATEMENT OF FINANCIAL POSITION", 
                                        "CONSOLIDATED CONDENSED STATEMENTS OF FINANCIAL POSITION (UNAUDITED)",
                                        "CONSOLIDATED CONDENSED STATEMENTS OF FINANCIAL POSITION",
                                        "Unaudited Consolidated Balance Sheets",
                                        "Unaudited Consolidated Balance Sheet",
                                        "Condensed Consolidated Balance Sheets (Current Period Unaudited)"
        )
        
        out = try(GetFinancial3(balance.sheet.descriptions, symbol, quarter))
        if(class(out) == 'try-error'){
                error <- read.csv('/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',stringsAsFactors = FALSE)
                newError <- data.frame(Ticker = symbol,Quarter = quarter,Error = 'Failed To Get Balance Sheet')
                error <- rbind(error,newError)
                write.csv(file = '/home/jmo/Desktop/AOTC-B/databases/errors/error.csv',error,row.names = FALSE)
                rm(error,newError)
        } else return(out)
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
                                 "CONDENSED INCOME STATEMENTS",
                                 "Consolidated Condensed Statements of Income (Loss) (Unaudited)",
                                 "Consolidated Condensed Statements of Income (Loss)",
                                 "Condensed Consolidated Statements of Income (Loss) (Unaudited)",
                                 "CondensedConsolidated Statements of Income (Loss)",
                                 #"CONDENSED CONSOLIDATED STATEMENT OF COMPREHENSIVE INCOME (LOSS) (Unaudited)",
                                 #"CONDENSED CONSOLIDATED STATEMENTS OF COMPREHENSIVE INCOME (LOSS) (Unaudited)",
                                 "CONSOLIDATED STATEMENTS OF INCOME (UNAUDITED)",
                                 "CONDENSED CONSOLIDATED STATEMENTS OF INCOME (UNAUDITED)",
                                 "CONSOLIDATED STATEMENT OF INCOME (UNAUDITED)", 
                                 "CONDENSED CONSOLIDATED STATEMENT OF INCOME (UNAUDITED)", 
                                 "CONSOLIDATED STATEMENTS OF OPERATIONS (UNAUDITED)", 
                                 "CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS (UNAUDITED)", 
                                 "CONSOLIDATED STATEMENT OF OPERATIONS (UNAUDITED)", 
                                 "CONDENSED CONSOLIDATED STATEMENT OF OPERATIONS (UNAUDITED)", 
                                 "CONSOLIDATED STATEMENT OF EARNINGS (UNAUDITED)", 
                                 "CONDENSED CONSOLIDATED STATEMENT OF EARNINGS (UNAUDITED)", 
                                 "CONSOLIDATED STATEMENTS OF EARNINGS (UNAUDITED)",
                                 "CONDENSED CONSOLIDATED STATEMENTS OF EARNINGS (UNAUDITED)",
                                 "INCOME STATEMENTS (UNAUDITED)",
                                 "CONDENSED INCOME STATEMENTS (UNAUDITED)",
                                 "CONSOLIDATED CONDENSED STATEMENTS OF INCOME",
                                 "CONSOLIDATED CONDENSED STATEMENT OF INCOME", 
                                 "CONSOLIDATED CONDENSED STATEMENTS OF OPERATIONS", 
                                 "CONSOLIDATED CONDENSED STATEMENT OF OPERATIONS", 
                                 "CONSOLIDATED CONDENSED STATEMENT OF EARNINGS", 
                                 "CONSOLIDATED CONDENSED STATEMENTS OF EARNINGS",
                                 #"CONSOLIDATED CONDENSED STATEMENT OF COMPREHENSIVE INCOME (LOSS) (UNAUDITED)",
                                 #"CONSOLIDATED CONDENSED STATEMENTS OF COMPREHENSIVE INCOME (LOSS) (UNAUDITED)",
                                 "CONSOLIDATED CONDENSED STATEMENTS OF INCOME (UNAUDITED)",
                                 "CONSOLIDATED CONDENSED STATEMENT OF INCOME (UNAUDITED)", 
                                 "CONSOLIDATED CONDENSED STATEMENTS OF OPERATIONS (UNAUDITED)", 
                                 "CONSOLIDATED CONDENSED STATEMENT OF OPERATIONS (UNAUDITED)", 
                                 "CONSOLIDATED CONDENSED STATEMENT OF EARNINGS (UNAUDITED)", 
                                 "CONSOLIDATED CONDENSED STATEMENTS OF EARNINGS (UNAUDITED)",
                                 "Unaudited Consolidated Statements of Operations",
                                 "Consolidated Income Statements",
                                 "Unaudited Consolidated Statements of Income",
                                 "Unaudited Consolidated Statement of Income",
                                 "Unaudited Consolidated Statement of Operations",
                                 "Unaudited Consolidated Statements of Earnings",
                                 "Unaudited Consolidated Statement of Earnings")
        
        output <- try(GetFinancial3(income.descriptions, symbol, quarter),silent = TRUE)
        # If income statement files, get comprehensive income
        if(class(output) == 'try-error'){
                income.descriptions <- c("Statements of Comprehensive Loss",
                                         'Consolidated Statements of Comprehensive Income',
                                         'Consolidated Statements of Comprehensive Income (Loss)',
                                         'Consolidated Statements of Comprehensive Income (Unaudited)',
                                         "Consolidated Statements of Comprehensive Income (Loss) (Unaudited)",
                                         "Consolidated Statements of Comprehensive Income (Loss) (Unaudited)", 
                                         
                                         
                                         'Condensed Consolidated Statements of Comprehensive Income',
                                         'Condensed Consolidated Statements of Comprehensive Income (Loss)',
                                         'Condensed Consolidated Statements of Comprehensive Income (Unaudited)',
                                         'Condensed Consolidated Statements of Comprehensive Income (Loss) (Unaudited)',
                                         
                                         "Consolidated Statements of Income and Comprehensive Income",
                                         "Consolidated Statements of Income and Comprehensive Income (Loss)",
                                         "Consolidated Statements of Income and Comprehensive Income (Unaudited)",
                                         "Consolidated Statements of Income and Comprehensive Income (Loss) (Unaudited)"
                )
                output <- try(GetFinancial3(income.descriptions, symbol, quarter))
                if(class(output) != 'try-error') print('Second Round Success')
                return(output)
        } else {
                return(output)
        }
}

## Helper Functions

##   Function to acquire Instance Document URL For Quarterly reports
GetURL3 <- function(symbol,quarter) {
        instances <- fread('/home/jmo/Desktop/AOTC-B/databases/data/tempEdgar.csv')
        
        instances <- instances[!duplicated(instances),]
        
        #Edge Cases
        if(toupper(symbol) == 'AFI'){
                stop('AFI Symbol Needs To Be Fixed')
        }
        if(symbol == 'AAWW'){
                thisInst <- arrange(
                        instances[instances$Ticker == toupper('air') & 
                                          instances$FORM_TYPE == '10-Q' &
                                          instances$COMPANY_NAME == 'ATLAS AIR WORLDWIDE HOLDINGS INC'],
                        desc(reportPeriod)
                )
                thisInst <- thisInst$INST[[quarter]]  
                thisInst <- sub('air','aaww',thisInst)
                return(thisInst)
        } else if(symbol == 'ACM'){
                thisInst <- arrange(instances[instances$Ticker == toupper(symbol) & instances$FORM_TYPE == '10-Q'],desc(reportPeriod))
                thisInst <- thisInst$INST[[quarter]]
                thisInst <- gsub('30.','31.',thisInst)
                return(thisInst)
        }
        else {
                thisInst <- try(arrange(instances[instances$Ticker == toupper(symbol) & instances$FORM_TYPE == '10-Q'],desc(reportPeriod)))
                if(class(thisInst) == 'try-error' | nrow(thisInst) == 0){
                        thisInst <- GetURLFailed(symbol)[quarter]
                        print(paste("Got CIK From Edgar For ",symbol,' Q-',quarter,sep=''))
                        return(thisInst)
                }
                thisInst <- thisInst$INST[[quarter]]
                return(thisInst)
        }
        
        
}

GetURLFailed <- function(symbol) {
        
        GetQAccessionNo <- function(symbol, foreign = FALSE) {
                
                QuarterlyReports <- function(symbol, foreign = FALSE) {
                        
                        options(stringsAsFactors = FALSE)
                        
                        if(foreign == FALSE) {
                                url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                                              symbol, "&type=10-q&dateb=&owner=exclude&count=10")
                        } else {
                                url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                                              symbol, "&type=20-f&dateb=&owner=exclude&count=10")
                        }
                        
                        filings <- xml2::read_html(url)
                        
                        ##   Generic function to extract info
                        ExtractInfo <- function(html.node) {
                                info <-
                                        filings %>%
                                        rvest::html_nodes(html.node) %>%
                                        rvest::html_text()
                                return(info)
                        }
                        
                        ##   Acquire filing name
                        filing.name <- ExtractInfo("#seriesDiv td:nth-child(1)")
                        
                        ##   Error message for function
                        if(length(filing.name) == 0) {
                                stop("invalid company symbol or foreign logical")
                        }
                        
                        ##   Acquire filing date
                        filing.date <- ExtractInfo(".small+ td")
                        
                        ##   Acquire accession number
                        accession.no.raw <- ExtractInfo(".small")
                        
                        accession.no <-
                                gsub("^.*Acc-no: ", "", accession.no.raw) %>%
                                substr(1, 20)
                        
                        ##   Create dataframe
                        info.df <- data.frame(filing.name = filing.name, filing.date = filing.date, 
                                              accession.no = accession.no)
                        return(info.df[1:3,])
                }
                ##   This is here to please R CMD check
                filing.name <- NULL
                accession.no <- NULL
                
                
                reports.df <- QuarterlyReports(symbol, foreign)
                reports.df <- try(mutate(reports.df, filing.year = substr(reports.df$filing.date,1,4) ) %>%
                                          filter(filing.name == "10-Q" | filing.name == "20-F"))
                
                ##   Error message for function
                if(class(reports.df) == "try-error") {
                        stop(reports.df)
                }
                
                return(reports.df)
        }
        
        lower.symbol <- tolower(symbol)
        
        accession.no.raw <- GetQAccessionNo(symbol, foreign = FALSE)
        accession.no <- gsub("-", "" , accession.no.raw$accession.no)
        accession.no.raw <- accession.no.raw$accession.no
        
        CIK <- CompanyInfo(symbol)
        CIK <- as.numeric(CIK$CIK)
        
        ReportPeriodQ <- function(symbol,CIK,accession.no,accession.no.raw) {
                
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
        
        report.period <- data.frame(report.period = ReportPeriodQ(symbol, CIK, accession.no[1], accession.no.raw[1]))
        try(report.period[2,1] <- ReportPeriodQ(symbol, CIK, accession.no[2], accession.no.raw[2]))
        try(report.period[3,1] <- ReportPeriodQ(symbol, CIK, accession.no[3], accession.no.raw[3]))
        report.period$report.period <- gsub("-", "" , report.period$report.period)
        report.period$accession.no = accession.no
        report.period$inst.url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                                         report.period$accession.no, "/", lower.symbol, "-", report.period$report.period, ".xml")
        return(report.period$inst.url)
}


# XBRL3 <- function() {
#         self <- list(element=NULL, role=NULL,
#                      label=NULL, presentation=NULL, definition=NULL, calculation=NULL,
#                      context=NULL, unit=NULL, fact=NULL, footnote=NULL)
#         
#         cache.dir <- NULL
#         discovered.files <- NULL
#         doc.inst <- NULL
#         dname.inst <- NULL
#         verbose <- FALSE
#         inst.lnkb <- NULL
#         
#         fixFileName <- function(dname, file.name) {
#                 if (substr(file.name, 1, 5) != "http:") {
#                         if (substr(file.name, 1, 5) == "../..") { ## A better solution is preferred, but it works for now
#                                 file.name <- paste0(dirname(dirname(dname)), "/",  substr(file.name, 7, nchar(file.name)))
#                         } else if (substr(file.name, 1, 2) == "..") {
#                                 file.name <- paste0(dirname(dname), "/", substr(file.name, 4, nchar(file.name)))
#                         } else {
#                                 file.name <- paste0(dname,"/", file.name)
#                         }
#                 }
#                 file.name
#         }
#         
#         setVerbose <- function(newVerbose) {
#                 oldVerbose <- verbose
#                 verbose <<- newVerbose
#                 oldVerbose
#         }
#         
#         setCacheDir <- function(new.cache.dir) {
#                 if (!file.exists(new.cache.dir)) {
#                         dir.create(new.cache.dir)
#                 }
#                 cache.dir <<- new.cache.dir
#         }
#         
#         fileFromCache <- function(file) {
#                 if (!(gsub("^(http|https|ftp)://.*$", "\\1", file) %in% c("http", "https", "ftp"))) {
#                         return (file)
#                 }
#                 bname <- basename(file)
#                 cached.file <- paste0(cache.dir, "/", bname)
#                 if (!file.exists(cached.file)) {
#                         if (verbose) {
#                                 cat("Downloading to cache dir...")
#                         }
#                         
#                         status <- try(download.file(file, cached.file, quiet = !verbose),
#                                       silent=TRUE)
#                         
#                         if (class(status)[1] == "try-error" || status == 1) {
#                                 unlink(cached.file)
#                                 stop(status, "\n")
#                         }
#                         
#                 } else {
#                         if (verbose) {
#                                 cat("Using file from cache dir...\n")
#                         }
#                 }
#                 cached.file
#         }
#         
#         # openInstance <- function(file.inst) {
#         #         dname.inst <<- dirname(file.inst)
#         #         if (!is.null(cache.dir)) {
#         #                 file.inst <- fileFromCache(file.inst)
#         #                 inst.lnkb <<- file.inst
#         #         }
#         #         doc.inst <<- XBRL::xbrlParse(file.inst)
#         # }
#         
#         openInstance <- function(file.inst) {
#                 dname.inst <<- dirname(file.inst)
#                 if (!is.null(cache.dir)) {
#                         file.inst <- fileFromCache(file.inst)
#                         inst.lnkb <<- file.inst
#                 }
#                 doc.inst <<- xbrlParse2(file.inst)
#         }
#         
#         deleteCachedInstance <- function() {
#                 if (verbose) {
#                         cat("Deleting the following downloaded and/or cached files...\n")
#                         print(inst.lnkb)
#                 }
#                 unlink(inst.lnkb)
#                 if (verbose) {
#                         cat("Done...\n")
#                 }
#         }
#         
#         getSchemaName <- function() {
#                 fixFileName(dname.inst, .Call("xbrlGetSchemaName", doc.inst, PACKAGE="XBRL"))
#         }
#         
#         processSchema <- function(file, level=1) {
#                 if (verbose) {
#                         cat("Schema: ", file, "\n")
#                 }
#                 if (length(which(discovered.files == file)) > 0) {
#                         if (verbose) {
#                                 cat("Already discovered. Skipping\n")
#                         }
#                         return (NULL)
#                 }
#                 discovered.files <<- c(discovered.files, file)
#                 dname <- dirname(file)
#                 if (level >= 1 && !is.null(cache.dir)) {
#                         if (verbose) {
#                                 cat("Level:", level,  "==>", file, "\n")
#                         }
#                         file <- fileFromCache(file)
#                         if (level == 1) {
#                                 inst.lnkb <<- c(inst.lnkb, file)
#                         }
#                 }
#                 
#                 doc <- XBRL::xbrlParse(file)
#                 
#                 if (level == 1) {
#                         processRoles(doc)
#                 }
#                 processElements(doc)
#                 linkbaseNames <- .Call("xbrlGetLinkbaseNames", doc, PACKAGE="XBRL")
#                 importNames <- .Call("xbrlGetImportNames", doc, PACKAGE="XBRL")
#                 .Call("xbrlFree", doc, PACKAGE="XBRL")
#                 
#                 for (linkbaseName in linkbaseNames) {
#                         linkbaseName <- fixFileName(dname, linkbaseName)
#                         if (verbose) {
#                                 cat(file," ==> Linkbase: ", linkbaseName,"\n")
#                         }
#                         processLinkbase(linkbaseName, level+1)
#                 }
#                 
#                 for (importName in importNames) {
#                         importName <- fixFileName(dname, importName)
#                         if (verbose) {
#                                 cat(file," ==> Schema: ", importName,"\n")
#                         }
#                         processSchema(importName, level+1)
#                 }
#         }
#         
#         processRoles <- function(doc) {
#                 if (verbose) {
#                         cat("Roles\n")
#                 }
#                 self$role <<- rbind(self$role,
#                                     .Call("xbrlProcessRoles", doc, PACKAGE="XBRL"))
#         }
#         
#         processElements <- function(doc) {
#                 if (verbose) {
#                         cat("Elements\n")
#                 }
#                 self$element <<- rbind(self$element,
#                                        .Call("xbrlProcessElements", doc, PACKAGE="XBRL"))
#         }
#         
#         processLinkbase <- function(file, level) {
#                 if (verbose) {
#                         cat("Linkbase: ", file, "\n")
#                 }
#                 if (length(which(discovered.files == file)) > 0) {
#                         if (verbose) {
#                                 cat("Already discovered. Skipping\n")
#                         }
#                         return (NULL)
#                 }
#                 discovered.files <<- c(discovered.files, file)
#                 if (level >= 2 && !is.null(cache.dir)) {
#                         if (verbose) {
#                                 cat("Level:", level,  "==>", file, "\n")
#                         }
#                         file <- fileFromCache(file)
#                         inst.lnkb <<- c(inst.lnkb, file)
#                 }
#                 doc <- XBRL::xbrlParse(file)
#                 
#                 ## We assume there can be only one type per linkbase file
#                 if (!processLabels(doc)) {
#                         if (!processPresentations(doc)) {
#                                 if (!processDefinitions(doc)) {
#                                         processCalculations(doc)
#                                 }
#                         }
#                 }
#                 .Call("xbrlFree", doc, PACKAGE="XBRL")
#         }
#         
#         processLabels <- function(doc) {
#                 pre.length <- length(self$label)
#                 self$label <<- rbind(self$label,
#                                      ans <- .Call("xbrlProcessLabels", doc, PACKAGE="XBRL"))
#                 if (!is.null(ans)) {
#                         if (verbose) {
#                                 cat("Labels.\n")
#                         }
#                         return (TRUE)
#                 }
#                 FALSE
#         }
#         
#         processPresentations <- function(doc) {
#                 pre.length <- length(self$presentation)
#                 self$presentation <<- rbind(self$presentation,
#                                             ans <- .Call("xbrlProcessArcs", doc, "presentation", PACKAGE="XBRL"))
#                 if (!is.null(ans)) {
#                         if (verbose) {
#                                 cat("Presentations.\n")
#                         }
#                         return (TRUE)
#                 }
#                 FALSE
#         }
#         
#         processDefinitions <- function(doc) {
#                 pre.length <- length(self$definition)
#                 self$definition <<- rbind(self$definition,
#                                           ans <- .Call("xbrlProcessArcs", doc, "definition", PACKAGE="XBRL"))
#                 if (!is.null(ans)) {
#                         if (verbose) {
#                                 cat("Definitions.\n")
#                         }
#                         return (TRUE)
#                 }
#                 FALSE
#         }
#         
#         processCalculations <- function(doc) {
#                 pre.length <- length(self$calculation)
#                 self$calculation <<- rbind(self$calculation,
#                                            ans <- .Call("xbrlProcessArcs", doc, "calculation", PACKAGE="XBRL"))
#                 if (!is.null(ans)) {
#                         if (verbose) {
#                                 cat("Calculations.\n")
#                         }
#                         return (TRUE)
#                 }
#                 FALSE
#         }
#         
#         processContexts <- function() {
#                 if (verbose) {
#                         cat("Contexts\n")
#                 }
#                 self$context <<- .Call("xbrlProcessContexts", doc.inst, PACKAGE="XBRL")
#         }
#         
#         processFacts <- function() {
#                 if (verbose) {
#                         cat("Facts\n")
#                 }
#                 self$fact <<- .Call("xbrlProcessFacts", doc.inst, PACKAGE="XBRL")
#         }
#         
#         processUnits <- function() {
#                 if (verbose) {
#                         cat("Units\n")
#                 }
#                 self$unit <<- .Call("xbrlProcessUnits", doc.inst, PACKAGE="XBRL")
#         }
#         
#         processFootnotes <- function() {
#                 if (verbose) {
#                         cat("Footnotes\n")
#                 }
#                 self$footnote <<- .Call("xbrlProcessFootnotes", doc.inst, PACKAGE="XBRL")
#         }
#         
#         closeInstance <- function() {
#                 .Call("xbrlFree", doc.inst, PACKAGE="XBRL")
#                 doc.inst <<- NULL
#         }
#         
#         getResults <- function() {
#                 self
#         }
#         
#         list(setVerbose=setVerbose,
#              setCacheDir=setCacheDir,
#              openInstance=openInstance,
#              deleteCachedInstance=deleteCachedInstance,
#              getSchemaName=getSchemaName,
#              processSchema=processSchema,
#              processContexts=processContexts,
#              processFacts=processFacts,
#              processUnits=processUnits,
#              processFootnotes=processFootnotes,
#              closeInstance=closeInstance,
#              getResults=getResults)
# }
# 
# xbrlDoAll3 <- function(file.inst, cache.dir="xbrl.Cache",
#                        prefix.out=NULL, verbose=FALSE,
#                        delete.cached.inst=TRUE) {
#         xbrl <- XBRL3()
#         xbrl$setVerbose(verbose)
#         if (!is.null(cache.dir)) { 
#                 xbrl$setCacheDir(cache.dir)
#         }
#         xbrl$openInstance(file.inst)
#         xbrl$processSchema(xbrl$getSchemaName())
#         xbrl$processContexts()
#         xbrl$processFacts()
#         xbrl$processUnits()
#         xbrl$processFootnotes()
#         xbrl$closeInstance()
#         if (delete.cached.inst &&
#             gsub("^(http|https|ftp)://.*$", "\\1", file.inst) %in% c("http", "https", "ftp")) {
#                 xbrl$deleteCachedInstance()
#         }
#         
#         xbrl.vars <- xbrl$getResults()
#         if (!is.null(prefix.out)) {
#                 if (verbose) {
#                         cat("Saving data\n")
#                 }
#                 write.csv(xbrl.vars$role, file=paste0(prefix.out, "_roles.csv"))
#                 write.csv(xbrl.vars$element, file=paste0(prefix.out, "_elements.csv"))
#                 write.csv(xbrl.vars$label, file=paste0(prefix.out, "_labels.csv"))
#                 write.csv(xbrl.vars$presentation, file=paste0(prefix.out, "_presentations.csv"))
#                 write.csv(xbrl.vars$definition, file=paste0(prefix.out, "_definitions.csv"))
#                 write.csv(xbrl.vars$calculation, file=paste0(prefix.out, "_calculations.csv"))
#                 write.csv(xbrl.vars$context, file=paste0(prefix.out, "_contexts.csv"))
#                 write.csv(xbrl.vars$fact, file=paste0(prefix.out, "_facts.csv"))
#                 write.csv(xbrl.vars$unit, file=paste0(prefix.out, "_units.csv"))
#                 write.csv(xbrl.vars$footnote, file=paste0(prefix.out, "_footnotes.csv"))
#         }
#         invisible(xbrl.vars)
# }
# 
# xbrlParse2 <- function(file) {
#         # if(!file.exists(file)) {
#         #         stop(file, " does not exists. Aborting.\n")
#         # }
#         .Call("xbrlParse", file, PACKAGE="XBRL")
# }
