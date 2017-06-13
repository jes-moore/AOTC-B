library(OECD)
dataset_list <- get_datasets()
search_dataset("composite", data = dataset_list)
dataset <- 'MEI_CLI'
filterList = "CSCICP03.USA+EA19+CHN.M/all?startTime=2005-05&endTime=2017-12&dimensionAtObservation=allDimensions&pid=f02522ad-a645-4ac6-9a76-3f70fc17bf5c"
df <- get_dataset(dataset = dataset, filter= filterList)[,c(7,2,8)]
df$obsTime <- as.Date(paste(df$obsTime,"-01",sep = ""),format="%Y-%m-%d")

