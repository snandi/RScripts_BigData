################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-07-09                                      ##
################################################################## 

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

################################################################## 
## This script is to read in the big data, one small file at a  ##
## time, extract only the demographics related information and  ##
## save it for later use.                                       ##
## This should run on BigMems                                   ##
## This is a parallelized script                                ##
################################################################## 

################################################################## 
## Initialize Header file and function library                  ##
################################################################## 
FilePath <- '~/peterq/PA_BigData/RScripts_BigData/'
DataPath <- '~/peterq/PA_BigData/Comscore data/'
DataPath.Scratch <- '/scratch/Nandi/'
OutputDataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
#Filename.Header <- paste(FilePath, 'HeaderFile_BigMem.R', sep='')
Filename.Header <- paste('~/RScripts/HeaderFile_Nandi.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
################################################################## 

################################################################## 
## Data Input                                                   ##
################################################################## 
#setwd(DataPath)
Colnames <- fn_getColumnNames(FilePrefix='janfeb', DataPath=DataPath)
Colnames.Keep <- c('machine_id', 'hoh_most_education', 'census_region', 'household_size', 
                   'hoh_oldest_age', 'household_income', 'children', 'racial_background', 
                   'connection_speed', 'country_of_origin', 'zip_code')
FilePrefix <- 'janfeb'

Time1 <- Sys.time()
################################################################## 
## This part is parallelized for bigmem                         ##
################################################################## 
NumCores <- 12   ## Number of cores to be used
cl <- makeSOCKcluster(as.numeric(NumCores))
registerDoSNOW(cl)

Data1 <- foreach(Indices=1:32, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_readBigData(FilePrefix='janfeb', FileIndex=Indices, DataPath=DataPath.Scratch, 
                 Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=TRUE)

Data2 <- foreach(Indices=1:30, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_readBigData(FilePrefix='marapril', FileIndex=Indices, DataPath=DataPath.Scratch, 
                 Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=TRUE)

Data3 <- foreach(Indices=1:28, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_readBigData(FilePrefix='mayjune', FileIndex=Indices, DataPath=DataPath.Scratch, 
                 Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=TRUE)

stopCluster(cl)
rm(cl)

################################################################## 
## This part is just an example for one dataset                 ##
################################################################## 
# Data <- fn_readBigData(FilePrefix='janfeb', FileIndex=1, DataPath=DataPath.Scratch, 
#                         Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=FALSE)

Data1 <- unique(rbind(Data1, Data2, Data3))
Data.Demographics <- Data1

## Drop the only duplicate machine_id
Data.Demographics <- subset(Data.Demographics, !(machine_id == '80355787' & zip_code==''))

FilePrefix <- 'AllMonths'
Filename.Out <- paste(OutputDataPath, FilePrefix, '_', 'Demographics.RData', sep='')
save(Data.Demographics, file=Filename.Out)
