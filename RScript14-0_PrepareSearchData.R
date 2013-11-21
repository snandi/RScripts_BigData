################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-11-12                                      ##
################################################################## 

################################################################## 
## This script will use the list of Search websites created     ##
## previously and identify whether a user has conducted a       ##
## "Search" or not. This data includes both types of users,     ##
## those have made a transaction and those that haven't. This   ##
## dataset will be used to try to model different search chara- ##
## -cteristics to the final purchase behavior.                  ##
################################################################## 

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

################################################################## 
## Initialize Header file and function library                  ##
################################################################## 
FilePath <- '~/peterq/PA_BigData/RScripts_BigData/'
DataPath <- '~/peterq/PA_BigData/Comscore data/'
DataPath.Scratch <- '/scratch/Nandi/'
#DataPath.Scratch <- '~/peterq/scratch/'
OutputDataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
#Filename.Header <- paste(FilePath, 'HeaderFile_BigMem.R', sep='')
Filename.Header <- paste('~/RScripts/HeaderFile_Nandi.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
################################################################## 

################################################################## 
## Data Input                                                   ##
################################################################## 
FilePrefix <- 'AllMonths'
Filename.In <- paste(DataPath.Scratch, FilePrefix, '_', 'Data_Travel.RData', sep='')
load(Filename.In)

Colnames.Drop <- c('hoh_most_education', 'census_region', 'household_size',
                   'hoh_oldest_age', 'household_income', 'children',
                   'racial_background', 'connection_speed', 'country_of_origin',
                   'zip_code', 'site_session_id', 'domain_id', 'ref_domain_name')

Data1 <- Data1[,colnames(Data1) %w/o% Colnames.Drop]

Sample <- subset(Data1, machine_id %in% c("17640180", "17644491", "49645796", "55111552", "56559711", "58622574", "61053494", "61284419", "61433282", "61538672"))
dim(Sample)

################################################################## 
## Top websites of travel transactions                          ##
################################################################## 
Transactions_Hotel <- fn_getTransactionSites(BigData=Data1, category_id=44,
                                             TransColname='Transaction_Hotel',
                                             dropWebsites=c('aol.com'))
Transactions_Air <- fn_getTransactionSites(BigData=Data1, category_id=43,
                                             TransColname='Transaction_Air',
                                             dropWebsites=c('aol.com'))
Transactions_Car <- fn_getTransactionSites(BigData=Data1, category_id=45,
                                             TransColname='Transaction_Car',
                                             dropWebsites=c('aol.com'))
################################################################## 

################################################################## 
## Try to categorize browsing behavior as a "Search"            ##
##################################################################
Transactions_Travel <- fn_getTransactionSites(BigData=Data1, category_id=c(43, 44, 45),
                                              TransColname='Transaction_Travel',
                                              dropWebsites=c('aol.com', 'vistaprint.com'))
Sample.Search <- subset(Sample, domain_name %in% c(as.vector(Transactions_Travel$Website)))
Sample.Search$DateTime <- as.POSIXct(paste(Sample.Search$event_date, Sample.Search$event_time))
Sample.Search <- Sample.Search[order(Sample.Search$machine_id, Sample.Search$DateTime),]
Sample.Search <- do.call(rbind, lapply(X=split(Sample.Search, f=Sample.Search$machine_id), FUN=fn_groupSearchActivity, BrowseWindow=4))  ## This complicated statement just calculates the time difference between consecutive events for each machine_id

DatabyMC <- subset(Sample.Search, machine_id=='49645796')

Colnames.View <- c('machine_id', 'pages_viewed', 'duration', 'event_date', 'domain_name',
                   'DateTime', 'DateTime2', 'EventTimeGap_Min', 'tran_flg', 'prod_category',
                   'SearchNum')
View(Sample.Search[,Colnames.View])
View(DatabyMC[,Colnames.View])

dim(Sample.Search)

################################################################## 
## Try to "Search" characteristics from the data created above  ##
##################################################################
DatabyMC <- subset(Sample.Search, machine_id=='49645796')

## Number of pages viewed
split(DatabyMC, f=DatabyMC[,c('machine_id', 'SearchNum')])
fn_returnSearchDetails <- function(Data){
  machine_id <- Data$machine_id[1]
  SearchNum <- as.numeric(Data$SearchNum[1])
  duration_Total <- as.numeric(sum(Data$duration))
  pages_viewed_Total <- as.numeric(sum(Data$pages_viewed))
  Search_Time_Total.Min <- as.numeric(min(1, 60*(max(Data$DateTime) - min(Data$DateTime))))
  domain_name_Total <- as.numeric(length(unique(Data$domain_name)))
  DateTime.Start <- min(Data$DateTime)
  Trans.ThisPeriod <- sum(na.is.zero(as.numeric(Data$tran_flg)))
  if(Trans.ThisPeriod == 1){
    Trans.DateTime <- min(subset(Data, tran_flg==1)[,'DateTime'])
  } else{
    Trans.DateTime <- NA
  }
  SearchDetails <- str(as.data.frame(cbind(machine_id=as.numeric(machine_id),
                                           SearchNum=as.numeric(SearchNum),
                                           duration_Total=as.numeric(duration_Total)), stringsAsFactors=FALSE)), 
                                       pages_viewed_Total, Search_Time_Total.Min,
                                       domain_name_Total, DateTime.Start, Trans.ThisPeriod,
                                       Trans.DateTime), stringsAsFactors=FALSE)
  return(SearchDetails)
}

Data <- subset(DatabyMC, SearchNum==8)

SearchDetails <- do.call(rbind, lapply(X=split(DatabyMC, f=DatabyMC[,c('machine_id', 'SearchNum')]), FUN=fn_returnSearchDetails))  
str(SearchDetails)
View(SearchDetails)

## Number of domains
## Duration
## TimeGap Total

View(DatabyMC[,Colnames.View])

##################################################################
## Create a unique Transaction ID                               ##
##################################################################
## Transaction_ID <- (Machine_ID, Date, prod_category_id, SerialNum)
## TransData <- subset(Sample, tran_flg==1)
## TransUniq <- aggregate(TransData$prod_qty, by=list(machine_id=TransData$machine_id, event_date=TransData$event_date, category_id=TransData$prod_category_id), FUN=length)
## TransUniq

## TransData2 <- transform(TransData, unique_id = as.integer(interaction(machine_id, event_date, prod_category_id)))
##################################################################

## Next Steps (2013-11-13)
## 1. Continue identifying "Search" behavior
## 2. Associate a particular "Search" behavior with transactions
## 3. Create unique transaction identifiers
## 4. For each Search, if there is a transaction within one week, associate that search with
## that transaction
