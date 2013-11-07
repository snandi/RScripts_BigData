################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-06-28                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript09, which is ## 
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

#View(Data1[Data1$machine_id == '61876734',])
write.table(x = Data1[Data1$machine_id == '61876734',],
            file = paste(DataPath.Scratch, 'Data_61876734.csv', sep=''),
            row.names = F, quote = F, sep = ',', col.names = T) 
#View(Data1[Data1$machine_id == '49645796',])
write.table(x = Data1[Data1$machine_id == '49645796',],
            file = paste(DataPath.Scratch, 'Data_49645796.csv', sep=''),
            row.names = F, quote = F, sep = ',', col.names = T) 

################################################################## 
## Top Purchase Categories                                      ##
################################################################## 
Table.Category <- table(Data1$prod_category)
Table.Category <- Table.Category[names(Table.Category) %w/o% "No Purchase"]
Table.MachineId <- table(Data1$machine_id)

ProdCat <- sort(Table.Category, decreasing=T)

jpeg(filename='../Meetings/2013-07-03/Plot1_ProdCat.jpg')
barplot(ProdCat[1:25], las=2, cex.axis=0.75, col=colors()[23],
        main='Popular Product Categories', horiz=F,
        cex.lab = 0.5, cex=0.6)
dev.off()

################################################################## 
## Demographics of Travel related purchasers                    ##
################################################################## 
Colnames <- colnames(Data1)
Colnames.keep <- c('machine_id', 'census_region', 'household_size', 'hoh_oldest_age', 
                   'household_income', 'racial_background', 'zip_code')
Data2 <- unique(Data1[,Colnames.keep])

Table1 <- table(Data2$racial_background)
Col1 <- rainbow(length(Table1))
Label1 <- paste(names(Table1), ' [', Table1, ']', sep='')
jpeg(filename='../Meetings/2013-07-03/Plot2_byRace.jpg')
pie3D(x=Table1, col=Col1, explode=0.1, radius=1.25)
dev.off()

jpeg(filename='../Meetings/2013-07-03/Plot3_byIncome.jpg')
Table2 <- table(Data2$household_income)
par(las=2) # make label text perpendicular to axis
barplot(Table2, col='darkolivegreen', border='darkolivegreen')
par(las=1)
dev.off()

Table3 <- table(Data2$census_region)
jpeg(filename='../Meetings/2013-07-03/Plot4_byCensus.jpg')
barplot(Table3, col='cyan4', border='cyan4')
dev.off()

jpeg(filename='../Meetings/2013-07-03/Plot5_byAge.jpg')
par(las=2) # make label text perpendicular to axis
barplot(table(Data2$hoh_oldest_age), col='dark red', border='dark red')
dev.off()

byZipCode <- as.data.frame(table(Data2$zip_code))
names(byZipCode) <- c('zip', 'Num_Households')
data(zipcode)
byZipCode <- merge(x=byZipCode, y=zipcode, by='zip', all.x=T, all.y=F)
byZipCode$MainLand <- sapply(byZipCode$state, FUN=function(state){state %notin% c('AK', 'HI')})
byState <- aggregate(byZipCode$Num_Households, by=list(byZipCode$state), FUN=sum)
names(byState) <- c('state', 'HH_byState')
byZipCode <- merge(x=byZipCode, y=byState, by='state')
mtstates <- byState$HH_byState
names(mtstates) <- byState$state
# Convert state abbreviations to full state names: 
mtfullstatenames <- tolower(state.name[match(names(mtstates), state.abb)])
mtfullstatenames[names(mtstates)=="DC"] <- "district of columbia"
names(mtstates) <- mtfullstatenames
# use "cut" to group the count data into several groups
mtstatesb <- cut(mtstates, breaks= c(0, 25, 50, 100, 150, Inf), 
                 labels=c("< 25", "25 - 50", "50 - 100", "100 - 150", "> 150"))  
jpeg(filename='../Meetings/2013-07-03/Plot6_bState.jpg')
library(maps)
usmap <- map("state")
mapstates <- sapply(strsplit(usmap$names, ":"), "[[", 1)
shapeFileN <- mtstatesb[match(mapstates,names(mtstates))]
cols <- rev(grey.colors(5))[shapeFileN]
map("state", col= cols, fill=T)
legend('bottomleft', legend=levels(mtstatesb), fill = rev(grey.colors(5)), 
       bty="n")
dev.off()

################################################################## 
## Top websites of travel transactions                          ##
################################################################## 
Data3 <- subset(x=Data1, prod_category != 'No Purchase')
Data3 <- Data3[,c('prod_category', 'domain_name')]

Table_Air <- table(subset(x=Data3, prod_category=='Air Travel')$domain_name)
Table_Air <- sort(Table_Air, decreasing=T)
Filename.Out <- '../Meetings/2013-07-03/Table1_Website_Air.csv'
write.table(x = as.data.frame(Table_Air), file = Filename.Out,
          row.names = T, quote = F, sep = ',') 

Table_Hotel <- table(subset(x=Data3, prod_category=='Hotel Reservations')$domain_name)
Table_Hotel <- sort(Table_Hotel, decreasing=T)
Filename.Out <- '../Meetings/2013-07-03/Table2_Website_Hotel.csv'
write.table(x = as.data.frame(Table_Hotel), file = Filename.Out,
          row.names = T, quote = F, sep = ',') 

Table_Car <- table(subset(x=Data3, prod_category=='Car Rental')$domain_name)
Table_Car <- sort(Table_Car, decreasing=T)
Filename.Out <- '../Meetings/2013-07-03/Table3_Website_Car.csv'
write.table(x = as.data.frame(Table_Car), file = Filename.Out,
          row.names = T, quote = F, sep = ',') 

