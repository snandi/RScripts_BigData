################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-08-07                                      ##
################################################################## 

################################################################## 
## This script prepares the hotel data for modeling, jus like   ##
## the one created by RScript12-1, which is saved as RegData_H- ##
## -otel.RData, and fits the first logistic regression model.   ##
## This script includes the Ratings data prepared by Yu Chen    ## 
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
load(Filename.In) ## This step loads the object Data.Hotel

### Do not open these csv files on a linux computer. This read-in program 
### is written specifically for files edited and saved on a windows m/c
HotelwRatings <- fn_returnHotelRatings(filename='HotelDetails_v6_Nandi.csv')

## Subset.Hotel <- subset(Data.Hotel, (tran_flg=='1' & prod_category=='Hotel Reservations'))

## 1. Establish search sites
PurchaseSites_Hotel.Table <- fn_getPurchaseSites(Data=Data.Hotel, category='Hotel Reservations')
PurchaseSites_Hotel <- c(names(PurchaseSites_Hotel.Table), 'kayak.com')

RegData.Hotel <- HotelwRatings
#RegData.Hotel.Rating <- subset(HotelwRatings, StarRating > 0)
#RegData.Hotel.Price <- subset(HotelwRatings, prod_totprice > 0)

RegData.Hotel$Search_Duration <- 0
RegData.Hotel$Search_NumWebsites <- 0
RegData.Hotel$Search_NumPages <- 0

load('~/peterq/PA_BigData/RScripts_BigData/Data/AllMonths_Demographics.RData')
RegData.Hotel <- merge(x=RegData.Hotel, y=Data.Demographics, by='machine_id', all.x=T, 
                       all.y=F)
Time1 <- Sys.time()

NumCores <- 12   ## Number of cores to be used
cl <- makeSOCKcluster(as.numeric(NumCores))
registerDoSNOW(cl)

RegData1.Hotel <- foreach(Indices=1:nrow(RegData.Hotel), .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_estHotelSearch(RowIndex=Indices, RegData=RegData.Hotel, BrowseData=Data.Hotel, NDaysBack=1)

stopCluster(cl)
rm(cl)
print(Sys.time() - Time1)

#RegData1.Hotel$StayDuration <- RegData1.Hotel$EndDate - RegData1.Hotel$StartDate
RegData1.Hotel$Tran_DayOfWeek <- dayOfWeek(as.timeDate(RegData1.Hotel$event_date))
RegData1.Hotel$PricePerNight <- RegData1.Hotel$prod_totprice / RegData1.Hotel$prod_qty
RegData1.Hotel$IsWeekend[is.na(RegData1.Hotel$StartDate)] <- NA

RegData1.Hotel$census_region <- fn_formatCensus(Vector.In=RegData1.Hotel$census_region)
RegData1.Hotel$household_size <- fn_formatHHSize(Vector.In=RegData1.Hotel$household_size)
RegData1.Hotel$hoh_oldest_age <- fn_formatHHOldestAge(Vector.In=RegData1.Hotel$hoh_oldest_age)
RegData1.Hotel$household_income <- fn_formatHHIncome(Vector.In=RegData1.Hotel$household_income)
RegData1.Hotel$children <- fn_formatChildren(Vector.In=RegData1.Hotel$children)
RegData1.Hotel$racial_background <- fn_formatRacialBackground(Vector.In=RegData1.Hotel$racial_background)

RegData.Hotel.Rating <- subset(RegData1.Hotel, StarRating > 0)
RegData.Hotel.Price <- subset(RegData1.Hotel, prod_totprice > 0)

Filename <- paste(OutputDataPath, 'RegData_Hotel_Rating.RData', sep='')
save(RegData.Hotel.Rating, file=Filename)

Filename <- paste(OutputDataPath, 'RegData_Hotel_Price.RData', sep='')
save(RegData.Hotel.Price, file=Filename)

RegData.Hotel <- RegData1.Hotel
Filename <- paste(OutputDataPath, 'RegData_Hotel_All.RData', sep='')
save(RegData.Hotel, file=Filename)
