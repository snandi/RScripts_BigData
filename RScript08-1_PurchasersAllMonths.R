################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-07-06                                      ##
################################################################## 

################################################################## 
## This script reads in the data on different purchase categor- ## 
## -ies from janfeb, marapril and mayjune and coalesces them.   ##
## These datasets were created by RScript08. This is primarily  ##
## for exploratory purposes and note any glaring inconstencies, ##
## like starkly different purchase patterns, etc                ##
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

load("~/peterq/PA_BigData/RScripts_BigData/Data/janfeb_Purchasers_wCategory.RData")
Purchasers.janfeb <- Purchasers
Purchasers.janfeb$Months <- 'janfeb'

load("~/peterq/PA_BigData/RScripts_BigData/Data/marapril_Purchasers_wCategory.RData")
Purchasers.marapril <- Purchasers
Purchasers.marapril$Months <- 'marapril'

load("~/peterq/PA_BigData/RScripts_BigData/Data/mayjune_Purchasers_wCategory.RData")
Purchasers.mayjune <- Purchasers
Purchasers.mayjune$Months <- 'mayjune'

Purchasers.All <- rbind(Purchasers.janfeb, Purchasers.marapril, Purchasers.mayjune)
Purchasers.All <- Purchasers.All[order(Purchasers.All$machine_id, decreasing=T),]

Purchasers.ByCat <- aggregate(Purchasers.All$total_amt_spent, 
                              by=list(Purchasers.All$prod_category_id, Purchasers.All$Months), 
                              FUN=sum)
colnames(Purchasers.ByCat) <- c('prod_category', 'months', 'total_amt_spent')

Purchasers.ByCatByMonth <- reshape(data=Purchasers.ByCat, v.names='total_amt_spent', direction='wide', 
                                   idvar='prod_category', timevar='months')

#View(Purchasers.ByCatByMonth)

Filename.Out <- paste(OutputDataPath, 'AllMonths', '_', 'Purchasers_wCategory.RData', sep='')
save(Purchasers.All, file=Filename.Out)
