################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-06-28                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript08, which is ## 
## saved as janfeb_Data_Travel, and conducts preliminary stats  ##
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

View(Data1[Data1$machine_id == '61876734',])

Table.Category <- table(Data1$prod_category)
Table.Category <- Table.Category[names(Table.Category) %w/o% "No Purchase"]
Table.MachineId <- table(Data1$machine_id)

ProdCat <- sort(Table.Category, decreasing=T)

barplot(ProdCat[1:25], las=2, cex.axis=0.75, col=colors()[23],
        main='Top 20 Product Categories', horiz=F,
        cex.lab = 0.5, cex=0.6)
