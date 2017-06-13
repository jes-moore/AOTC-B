library(finreportr)
library(XBRL)
library(edgar)
library(data.table)
cik <- read.csv('data/cik_ticker.csv')
ticker = 'fb'
ticker <- toupper(ticker)
thisCIK <- cik$CIK[cik$Ticker == ticker]
filings <- fread('data/master_index.csv',stringsAsFactors = FALSE)
filings <- filings[filings$CIK == thisCIK,]

filings$INST <- paste('https://www.sec.gov/Archives/',
                          gsub('.txt','',gsub('-','',thisFilings$EDGAR_LINK)),
                          '/',tolower(ticker),'-',
                          gsub('-','',thisFilings$DATE_FILED),
                          '.xml',sep = '')

