rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

#################################################### 
## This script is to read in and do exploratory   ##
## analysis on the comScore data                  ##
#################################################### 

#################################################### 
## Initialize Header file and function library    ##
#################################################### 
FilePath <- '~/peterq/PA_BigData/RScripts_BigData/'
#FilePath <- '~/Google Drive/UWMadison/PA_BigData/RScripts_BigData/'
DataPath <- '~/peterq/PA_BigData/Comscore data/'
#DataPath <- '~/Google Drive/UWMadison/PA_BigData/Comscore Data/'
Filename.Header <- paste('~/RScripts/HeaderFile_Nandi.R', sep='')
#Filename.Header <- paste('~/Google Drive/UWMadison/RScripts/HeaderFile_HW.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
#################################################### 

#################################################### 
## Data Input                                     ##
#################################################### 
setwd(DataPath)
Filename <- 'Data_JanJunTransaction481511eb2ee054fc.txt'
Data <- read.table(file=Filename, header=TRUE, sep='\t', 
                   colClasses='character', skip=0, quote='', 
                   stringsAsFactors=FALSE, fill=TRUE, 
                   comment.char='')
## quote='' allows the columns to have ' and " in the strings

## colClasses is set to 'character' because some numeric values are too long
## comment.char='' allows the columns to have # in the strings
str(Data)
dim(Data)
#View(head(Data))
#View(Data)
####################################################

#################################################### 
## Exploratory Analysis                           ##
#################################################### 
RowsPerMC <- as.data.frame(table(Data$machine_id))
summary(RowsPerMC$Freq)
hist(RowsPerMC$Freq, breaks=1500)
RowsPerMC[RowsPerMC$Freq > 200,]
View(Data[Data$machine_id == '84449391',])

RowsPerMC[RowsPerMC$Freq > 10 & RowsPerMC$Freq < 20 ,]
View(Data[Data$machine_id == '95155670',])


summary(as.numeric(Data$prod_totprice))

View(Data[Data$prod_totprice <= 0,])