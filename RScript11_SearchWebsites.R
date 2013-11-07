################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-07-03                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript08, which is ## 
## saved as janfeb_Data_Travel, and quantifies the "search" pe- ##
## -riod before a travel related transaction has occured.       ##
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
FilePrefix <- 'janfeb'
Filename.In <- paste(DataPath.Scratch, FilePrefix, '_', 'Data_Travel.RData', sep='')
load(Filename.In)

################################################################## 
## Day of Transaction                                           ##
##################################################################
Search_Air <- fn_getSearchSites(BigData=Data1, category_id=43)
Search_Hotel <- fn_getSearchSites(BigData=Data1, category_id=44)
Search_Car <- fn_getSearchSites(BigData=Data1, category_id=45)

Search_Air.df <- as.data.frame(cbind(names(Search_Air), Search_Air), stringsAsFactors=F)
names(Search_Air.df) <- c('Website', 'Freq')
Search_Air.df$Freq <- as.numeric(Search_Air.df$Freq)
jpeg(filename='../Meetings/2013-07-03/Cloud1_Air.jpg')
wordcloud(Search_Air.df[-c(1:7, 9, 11),'Website'], Search_Air.df[-c(1:7, 9, 11),'Freq'], min.freq=15, random.color=TRUE, colors='purple', scale=c(4, 0.2))
dev.off()
 
Search_Car.df <- as.data.frame(cbind(names(Search_Car), Search_Car), stringsAsFactors=F)
names(Search_Car.df) <- c('Website', 'Freq')
Search_Car.df$Freq <- as.numeric(Search_Car.df$Freq)
jpeg(filename='../Meetings/2013-07-03/Cloud3_Car.jpg')
wordcloud(Search_Car.df[-(1:11),'Website'], Search_Car.df[-(1:11),'Freq'], min.freq=15, random.color=TRUE, colors='red', scale=c(3, 0.2))
dev.off()

Search_Hotel.df <- as.data.frame(cbind(names(Search_Hotel), Search_Hotel), stringsAsFactors=F)
names(Search_Hotel.df) <- c('Website', 'Freq')
Search_Hotel.df$Freq <- as.numeric(Search_Hotel.df$Freq)
Filename.Out <- '../Meetings/2013-07-11/Table5_HotelWebsites.csv'
write.table(x = Search_Hotel.df, file = Filename.Out,
            row.names = T, quote = F, sep = ',') 
jpeg(filename='../Meetings/2013-07-11/Cloud2_Hotel.jpg')
Pal2 <- brewer.pal(8, 'Dark2')
wordcloud(Search_Hotel.df[-c(1:9, 12:13),'Website'], Search_Hotel.df[-c(1:9),'Freq'], min.freq=15, colors=Pal2, scale=c(4, 0.18), max.words=100, rot.per=0.15)
dev.off()
