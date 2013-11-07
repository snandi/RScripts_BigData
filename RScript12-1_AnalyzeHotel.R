################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-07-14                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript10-1, which  ##
## is saved as AllMonths_Data_Hotel, and                        ##
## 1. It categorizes hotels as luxury and economy               ##
## 2. It quantifies a search window of 24 hrs before the trans. ##
## 3. It quantifies search volume by websites, pages, duration  ##
## 4. It merges this data with demographics, and prepares the   ##
## first dataset for a logistic regression between luxury and   ##
## economy hotel purchasers                                     ##
################################################################## 

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

################################################################## 
## Initialize Header file and function library                  ##
################################################################## 
FilePath <- '~/peterq/PA_BigData/RScripts_BigData/'
DataPath <- '~/peterq/PA_BigData/Comscore data/'
#DataPath.Scratch <- '/scratch/Nandi/'
DataPath.Scratch <- '~/peterq/scratch/'
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
Filename.In <- paste(DataPath.Scratch, FilePrefix, '_', 'Data_Hotel.RData', sep='')
load(Filename.In)

Subset.Hotel <- subset(Data.Hotel, (tran_flg=='1' & prod_category=='Hotel Reservations'))
## Filename.Subset <- '../Meetings/2013-07-18/HotelDetails.txt'
## write.table(x = Subset.Hotel, file = Filename.Subset, col.names = T, row.names = F,
##            quote = F, sep = '|') 

ID <- '70841669'
Data <- subset(Data.Hotel, machine_id == ID)
Data$DateTime <- as.POSIXct(paste(as.character(Data$event_date), as.character(Data$event_time)))
 
## 1. Establish search sites
PurchaseSites_Hotel.Table <- fn_getPurchaseSites(Data=Data.Hotel, category='Hotel Reservations')
PurchaseSites_Hotel <- c(names(PurchaseSites_Hotel.Table), 'kayak.com')

## 2. Separate out users of hilton, marriot
Subset.Hotel <- subset(Data.Hotel, (tran_flg=='1' & prod_category=='Hotel Reservations'))
Subset.Hotel$Star1 <- 0
## Subscript L is for luxury
Hotels_L <- c('marriott', 'hilton', 'radisson', 'hyatt')
Websites_L <- c('marriott.com', 'hilton.com', 'radisson.com', 'hyatt.com', 
                        'starwoodhotels.com') 
Subset.Hotel[Subset.Hotel$domain_name %in% Websites_L, 'Star1'] <- 4
Subset.Hotel$Star2 <- 4*na.is.zero(as.integer(sapply(X=Subset.Hotel$prod_name, FUN=grep, 
                                                     pattern=paste(Hotels_L, collapse='|'), 
                                                     ignore.case=T)))
## Subscript E is for economy
Hotels_E <- c('motel', 'super', 'extended', 'best western', 'days inn', 'best value')
Websites_E <- c('bestwestern.com', 'daysinn.com', 'super8.com', 'countryinns.com', 
                'ramada.com')

Subset.Hotel[Subset.Hotel$domain_name %in% Websites_E, 'Star1'] <- 1
Subset.Hotel$Star3 <- na.is.zero(as.integer(sapply(X=Subset.Hotel$prod_name, FUN=grep, 
                                                   pattern=paste(Hotels_E, collapse='|'), 
                                                   ignore.case=T)))

fn_StarRating <- function(Row) {
  if (max(Row[1], Row[2]) == 4){
    Star <- 4
  } else if (max(Row[1], Row[3]) == 1){
    Star <- 1
  } else{
    Star <- 0
  }
  return(Star)
}

Subset.Hotel$Star <- apply(X=Subset.Hotel[,c('Star1', 'Star2', 'Star3')], MARGIN=1, 
                           FUN=fn_StarRating)
Subset.Hotel$Star1 <- NULL
Subset.Hotel$Star2 <- NULL
Subset.Hotel$Star3 <- NULL

RegData.Hotel <- subset(Subset.Hotel, Star > 0)
RegData.Hotel$Search_Duration <- 0
RegData.Hotel$Search_NumWebsites <- 0
RegData.Hotel$Search_NumPages <- 0

load('~/peterq/PA_BigData/RScripts_BigData/Data/AllMonths_Demographics.RData')
RegData.Hotel <- merge(x=RegData.Hotel, y=Data.Demographics, by='machine_id', all.x=T, 
                       all.y=F)
Index <- 14
for(Index in 1:nrow(RegData.Hotel)){
  ID <- RegData.Hotel[Index, 'machine_id']
  TransactionDateTime <- as.POSIXct(paste(as.character(RegData.Hotel[Index, 'event_date']), 
                                          as.character(RegData.Hotel[Index, 'event_time'])))
  Data <- subset(Data.Hotel, machine_id == ID)
  Data$Search_EndTime <- as.POSIXct(paste(as.character(Data$event_date), as.character(Data$event_time)))
  
  Data <- subset(Data, Search_EndTime <= TransactionDateTime)
  Time2 <- TransactionDateTime - 86400
  Data <- subset(Data, Search_EndTime >= Time2)
  
  Data$Search_Ind <- Data$domain_name %in% PurchaseSites_Hotel
  Data$Search_Duration <- Data$Search_Ind * Data$duration
  Data$Search_NumPages <- Data$Search_Ind * Data$pages_viewed
  
  RegData.Hotel[Index, 'Search_Duration'] <- sum(Data$Search_Duration)
  RegData.Hotel[Index, 'Search_NumPages'] <- sum(Data$Search_NumPages)
  RegData.Hotel[Index, 'Search_NumWebsites'] <- sum(Data$Search_Ind)
  print(paste(Index, ID, TransactionDateTime))
        
  rm(Data)
}

Filename <- paste(OutputDataPath, 'RegData_Hotel.RData', sep='')
save(RegData.Hotel, file=Filename)
