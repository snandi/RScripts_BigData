################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-07-10                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript10-1, which  ##
## is saved as AllMonths_Data_Hotel, and conducts preliminary   ##
## analysis. It then conducts some summary stats for 2013-07-11 ##
## meeting.                                                     ##
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

## Search_Hotel <- fn_getSearchSites(BigData=Data.Hotel, category_id=44)
## Search_Hotel.df <- as.data.frame(cbind(names(Search_Hotel), Search_Hotel), stringsAsFactors=F)
## names(Search_Hotel.df) <- c('Website', 'Freq')
## Search_Hotel.df$Freq <- as.numeric(Search_Hotel.df$Freq)
## Filename.Out <- '../Meetings/2013-07-11/Table5_HotelWebsites.csv'
## write.table(x = Search_Hotel.df, file = Filename.Out,
##             row.names = T, quote = F, sep = ',') 

Data3 <- subset(x=Data.Hotel, prod_category != 'No Purchase')
Data3 <- Data3[,c('machine_id', 'prod_category', 'domain_name')]

Websites_HighPrice <- c('marriott.com', 'hilton.com', 'ichotelsgroup.com', 'hyatt.com')
Websites_Multi <- c('expedia.com', 'hotels.com', 'priceline.com', 'choicehotels.com', 'hotwire.com', 'travelocity.com', 'orbitz.com')
Websites_LowPrice <- c('bestwestern.com', 'daysinn.com', 'super8.com', 'countryinns.com', 'ramada.com')
Data_HighPrice <- subset(Data3, domain_name %in% Websites_HighPrice)
Data_LowPrice <- subset(Data3, domain_name %in% Websites_LowPrice)
Data_Multi <- subset(Data3, domain_name %in% Websites_Multi)

intersect(unique(Data_HighPrice$machine_id), unique(Data_Multi$machine_id))

Ids_HighPrice <- unique(Data_HighPrice$machine_id)
Ids_LowPrice <- unique(Data_LowPrice$machine_id)
Ids_Multi <- unique(Data_Multi$machine_id)

AllIds <- unique(c(Ids_HighPrice, Ids_LowPrice, Ids_Multi))

CommonIds <- cbind((AllIds %in% Ids_HighPrice),
                   (AllIds %in% Ids_Multi),
                   (AllIds %in% Ids_LowPrice))

names(CommonIds) <- c('High Price', 'Search Websites', 'Low Price')

CommonIds.Venn <- vennCounts(CommonIds)
jpeg('../Meetings/2013-07-11/VennDiagram_Hotel.jpg')
vennDiagram(CommonIds.Venn, names=names(CommonIds), col='blue')
dev.off()

Websites_Search <- c(Websites_HighPrice, Websites_Multi, Websites_LowPrice, 'kayak.com')

Data_65122486 <- subset(Data.Hotel, machine_id == '65122486')

ID <- '94800448'

for(ID in Ids_HighPrice){
  Data <- subset(Data.Hotel, machine_id == ID)
  Data$DateTime <- as.POSIXct(paste(as.character(Data$event_date), as.character(Data$event_time)))
  Data$Search_Ind <- Data$domain_name %in% Websites_Search
  Data$Search_Time <- Data$Search_Ind * Data$duration
  Data$tran_flg <- na.is.zero(as.numeric(Data$tran_flg))
  Data$tran_flg[Data$prod_category != 'Hotel Reservations'] <- 0
  
  Data.xts <- as.xts(Data[,c('Search_Ind', 'duration', 'Search_Time', 'tran_flg')],
                     order.by=Data$DateTime)
  
  jpeg(paste('../Meetings/2013-07-11/Plot2_', ID, '_HP.jpg', sep=''))
  plot(Data.xts$Search_Time, type='h', main=Data$machine_id[1], ylab='Search Time')
  points(Data.xts$tran_flg[Data$tran_flg>0], col='red', pch='H', cex=2)
  dev.off()
}

load('~/peterq/PA_BigData/RScripts_BigData/Data/AllMonths_Demographics.RData')

Demo.HighPrice <- subset(Data.Demographics, machine_id %in% Ids_HighPrice)
Demo.LowPrice <- subset(Data.Demographics, machine_id %in% Ids_LowPrice)

Demo.HighPrice$household_income <- fn_formatHHIncome(Vector.In=Demo.HighPrice$household_income)
Demo.LowPrice$household_income <- fn_formatHHIncome(Vector.In=Demo.LowPrice$household_income)

Table1 <- table(Demo.HighPrice$household_income)
Table1.Pct <- Table1/sum(Table1)
Table2 <- table(Demo.LowPrice$household_income)
Table2.Pct <- Table2/sum(Table2)
par(las=2) # make label text perpendicular to axis
barplot(Table1.Pct, col='darkolivegreen', border='darkolivegreen')
barplot(Table2.Pct, col='darkolivegreen', border='darkolivegreen')

jpeg('../Meetings/2013-07-11/Plot3_byIncome.jpg')
par(las=2) # make label text perpendicular to axis
barplot(rbind(Table1.Pct, Table2.Pct), beside=TRUE, col=c('black', 'gray'),
        xlab='Income Range', ylab='Percentage')
legend("topleft", c('High Price Hotels', 'Medium/Low Price Hotels'), cex=1, 
   bty="n", fill=c('black', 'gray'))
par(las=1) 
dev.off()

