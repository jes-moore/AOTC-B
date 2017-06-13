library(xlsx)
library(lubridate)
oilStocks <- read.xlsx('oilDT.xls',sheetIndex = 2,colIndex = c(1,2),stringsAsFactors=FALSE)
colnames(oilStocks) <- c('Date','Weekly_Ending_Crude_Stocks_000s')
oilStocks <- oilStocks[3:nrow(oilStocks),]
oilStocks$Date <- as.Date('1900-01-01') + days(oilStocks$Date)
oilStocks <- na.exclude(oilStocks)

oilSupply <- read.xlsx('oilDT.xls',sheetIndex = 3,colIndex = c(1,2),stringsAsFactors=FALSE)
colnames(oilSupply) <- c('Date','Weekly_US_Crude_Production_000s')
oilSupply <- oilSupply[3:nrow(oilSupply),]
oilSupply$Date <- as.Date('1900-01-01') + days(oilSupply$Date)
oilSupply <- na.exclude(oilSupply)

oilImports <- read.xlsx('oilDT.xls',sheetIndex = 3,colIndex = c(1,5,6),stringsAsFactors=FALSE)
colnames(oilImports) <- c('Date','Weekly_US_Net_Imports_000s','Weekly_US_Total_Imports_000s')
oilImports <- oilImports[3:nrow(oilImports),]
oilImports$Date <- as.Date('1900-01-01') + days(oilImports$Date)
oilImports <- na.exclude(oilImports)

oilImportsAvg <- read.xlsx('oilDT.xls',sheetIndex = 4,colIndex = c(1,5,6),stringsAsFactors=FALSE)
colnames(oilImportsAvg) <- c('Date','4WkAvg_US_Net_Imports_000s','4WkAvg_US_Total_Imports_000s')
oilImportsAvg <- oilImportsAvg[3:nrow(oilImportsAvg),]
oilImportsAvg$Date <- as.Date('1900-01-01') + days(oilImportsAvg$Date)
oilImportsAvg <- na.exclude(oilImportsAvg)

write.csv(oilImports,'../shinyApp/data/oilimports.csv')
write.csv(oilImportsAvg,'../shinyApp/data/oilimportsavg.csv')
write.csv(oilStocks,'../shinyApp/data/oilstocks.csv')
write.csv(oilSupply,'../shinyApp/data/oilsupply.csv')



# library(RMySQL)
# con <- dbConnect(MySQL(),host="mysql3.gear.host", dbname='oilimports',user="oilimports",pass="Ar3GEuhb_g~0")
# dbWriteTable(con, value = oilImports, name = "oilimports",overwrite = TRUE,append=FALSE) 
# dbWriteTable(con, value = oilImportsAvg, name = "oilimportsavg", overwrite = TRUE,append=FALSE) 
# dbWriteTable(con, value = oilStocks, name = "oilstocks", overwrite = TRUE,append=FALSE) 
# dbWriteTable(con, value = oilSupply, name = "oilsupply", overwrite = TRUE,append=FALSE) 
# 



