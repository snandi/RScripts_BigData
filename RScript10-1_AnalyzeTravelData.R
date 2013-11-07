################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-07-07                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript09-1, which  ##
## is saved as AllMonths_Data_Travel, and conducts preliminary  ##
## analysis. It separates out the data for people who have had  ##
## at least one hotel reservation and saves as AllMonths_Data-  ##
## -Hotel.RData.                                                ##
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

################################################################## 
## Top Purchase Categories                                      ##
################################################################## 
Table.Category <- table(Data1$prod_category)
Table.Category <- Table.Category[names(Table.Category) %w/o% "No Purchase"]
Table.MachineId <- table(Data1$machine_id)

ProdCat <- sort(Table.Category, decreasing=T)

################################################################## 
## Top websites of travel transactions                          ##
################################################################## 
Data3 <- subset(x=Data1, prod_category != 'No Purchase')
Data3 <- Data3[,c('prod_category', 'domain_name')]

Table_Air <- table(subset(x=Data3, prod_category=='Air Travel')$domain_name)
Table_Air <- sort(Table_Air, decreasing=T)
Filename.Out <- '../Meetings/2013-07-11/Table1_Website_Air.csv'
write.table(x = as.data.frame(Table_Air), file = Filename.Out,
            row.names = T, quote = F, sep = ',') 

Table_Hotel <- table(subset(x=Data3, prod_category=='Hotel Reservations')$domain_name)
Table_Hotel <- sort(Table_Hotel, decreasing=T)
Filename.Out <- '../Meetings/2013-07-11/Table2_Website_Hotel.csv'
write.table(x = as.data.frame(Table_Hotel), file = Filename.Out,
            row.names = T, quote = F, sep = ',') 

Table_Car <- table(subset(x=Data3, prod_category=='Car Rental')$domain_name)
Table_Car <- sort(Table_Car, decreasing=T)
Filename.Out <- '../Meetings/2013-07-11/Table3_Website_Car.csv'
write.table(x = as.data.frame(Table_Car), file = Filename.Out,
            row.names = T, quote = F, sep = ',') 

Table_Packages <- table(subset(x=Data3, prod_category=='Travel Packages')$domain_name)
Table_Packages <- sort(Table_Packages, decreasing=T)
Filename.Out <- '../Meetings/2013-07-11/Table4_Website_Packages.csv'
write.table(x = as.data.frame(Table_Packages), file = Filename.Out,
            row.names = T, quote = F, sep = ',') 
################################################################## 

Search_Packages <- fn_getSearchSites(BigData=Data1, category_id=46)
Search_Packages.df <- as.data.frame(cbind(names(Search_Packages), Search_Packages), stringsAsFactors=F)
names(Search_Packages.df) <- c('Website', 'Freq')
Search_Packages.df$Freq <- as.numeric(Search_Packages.df$Freq)
jpeg(filename='../Meetings/2013-07-11/Cloud1_Air.jpg')
wordcloud(Search_Packages.df[-c(1:3, 5, 8:9),'Website'], Search_Packages.df[-c(1:3, 5, 8:9),'Freq'], min.freq=15, random.color=TRUE, colors='blue', scale=c(3, 0.2))
dev.off()


##1 Quantify a browsing session
##2 Consider only Hotel buyers: Marriot vs 
FilePrefix <- 'AllMonths'
Filename.In <- paste(OutputDataPath, FilePrefix, '_Purchasers_wCategory.RData', sep='')
load(Filename.In)

Purchasers.Hotel <- subset(x = Purchasers.All, subset = prod_category_id %in% 
  c('Hotel Reservations'))

Data.Hotel <- subset(Data1, machine_id %in% Purchasers.Hotel$machine_id)
attributes(Data.Hotel)$comment <- 'These machine_ids have made at least one hotel reservation'
Filename.Out <- paste(DataPath.Scratch, FilePrefix, '_', 'Data_Hotel.RData', sep='')
save(Data.Hotel, file=Filename.Out)

View(subset(Data.Hotel, machine_id=='95057969'))
