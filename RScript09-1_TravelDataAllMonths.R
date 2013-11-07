################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-07-07                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript08-1, which  ##
## has Purchasers, segregates the ones that made at least one   ##
## travel related transaction. This script then goes to the raw ##
## text files and accumulates the search and transaction infor- ##
## -mation of those purchasers. The output of this program is   ##
## saved as AllMonths_Data_Travel. This file is an extension of ##
## RScript09. This includes all six months                      ##
## This should run on BigMems                                   ##
## This is a parallelized script                                ##
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
Filename.In <- paste(OutputDataPath, FilePrefix, '_Purchasers_wCategory.RData', sep='')
load(Filename.In)

Purchasers.Travel <- subset(x = Purchasers.All, subset = prod_category_id %in% 
  c('Air Travel', 'Hotel Reservations', 'Car Rental', 'Travel Packages', 'Other Travel'))

Colnames <- fn_getColumnNames(FilePrefix='janfeb', DataPath=DataPath)
Colnames.Keep <- Colnames
length(unique(Purchasers.Travel$machine_id))

################################################################## 
## This part is parallelized for bigmem                         ##
################################################################## 
NumCores <- 9   ## Number of cores to be used
cl <- makeSOCKcluster(as.numeric(NumCores))
registerDoSNOW(cl)

Data1 <- foreach(Indices=1:32, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_getTravelPurchasers(FilePrefix='janfeb', FileIndex=Indices, DataPath=DataPath.Scratch, 
                         Colnames=Colnames, Colnames.Keep=Colnames, 
                         Purchasers.Travel=Purchasers.Travel)
Data2 <- foreach(Indices=1:30, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_getTravelPurchasers(FilePrefix='marapril', FileIndex=Indices, DataPath=DataPath.Scratch, 
                         Colnames=Colnames, Colnames.Keep=Colnames, 
                         Purchasers.Travel=Purchasers.Travel)
Data3 <- foreach(Indices=1:28, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_getTravelPurchasers(FilePrefix='mayjune', FileIndex=Indices, DataPath=DataPath.Scratch, 
                         Colnames=Colnames, Colnames.Keep=Colnames, 
                         Purchasers.Travel=Purchasers.Travel)
stopCluster(cl)
rm(cl)
# 
################################################################## 
## This part is just an example for one dataset                 ##
################################################################## 
# Data1 <- fn_getTravelPurchasers(FilePrefix='janfeb', FileIndex=2, DataPath=DataPath.Scratch, 
#                                  Colnames=Colnames, Colnames.Keep=Colnames, 
#                                  Purchasers.Travel=Purchasers.Travel)

Data1 <- rbind(Data1, Data2, Data3)
rm(Data2, Data3)
Data1$prod_category <- fn_formatProdCategory.F(Vector.In = Data1$prod_category_id)
Data1 <- Data1[order(Data1$machine_id, Data1$event_date, Data1$event_time),]

attributes(Data1)$comment <- 'These machine_ids have made at least one travel related purchase'
Filename.Out <- paste(DataPath.Scratch, FilePrefix, '_', 'Data_Travel.RData', sep='')
save(Data1, file=Filename.Out)
