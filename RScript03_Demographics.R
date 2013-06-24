#################################################### 
## The author of this script is: Subhrangshu Nandi##
## Project: Big Data                              ##
## First Draft: 2013-06-05                        ##
#################################################### 

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

#################################################### 
## This script reads in the Demographics data     ##
## saved by RScripts02, and does descriptive stat-##
## istics on the customers                        ##
#################################################### 

#################################################### 
## Initialize Header file and function library    ##
#################################################### 
#FilePath <- 'C:/Users/Nandi/Documents/Google Drive/UWMadison/PA_BigData/RScripts_BigData/'
FilePath <- '~/peterq/PA_BigData/RScripts_BigData/'
#DataPath <- 'C:/Users/Nandi/Documents/Google Drive/UWMadison/PA_BigData/RScripts_BigData/Data/'
DataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
#OutputDataPath <- 'C:/Users/Nandi/Documents/Google Drive/UWMadison/PA_BigData/RScripts_BigData/Data/'
OutputDataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
#Filename.Header <- paste('C:/Users/Nandi/Documents/Google Drive/UWMadison/RScripts/HeaderFile_Nandi.R', sep='')
Filename.Header <- paste('~/RScripts/HeaderFile_Nandi.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
#################################################### 

#################################################### 
## Data Input                                     ##
#################################################### 
Filename <- paste(DataPath, 'janfeb_Demographics.RData', sep='')
load(Filename)
BigData <- BigData[BigData$zip_code != '',]
str(BigData)


#################################################### 
## Household Most Education                       ##
#################################################### 
BigData$hoh_most_education <- as.numeric(BigData$hoh_most_education)
BigData$hoh_most_education <- as.factor(BigData$hoh_most_education)

levels(BigData$hoh_most_education)

#################################################### 
## Racial Background                              ##
#################################################### 
BigData$racial_background <- sapply(X=BigData$racial_background, 
                            FUN=function(v){switch(v, 
                                                   '1' = 'White',
                                                   '2' = 'Black',
                                                   '3' = 'Asian',
                                                   '5' = 'Other')})
BigData$racial_background <- as.factor(BigData$racial_background)

Table1 <- table(BigData$racial_background)
Col1 <- rainbow(length(Table1))
Label1 <- paste(names(Table1), ' [', Table1, ']', sep='')
jpeg(filename='Data/Plot1_Race.jpg')
#pie3D(x=Table1, col=Col1, labels=Label1, labelcex=1.25, explode=0.1, radius=1.25, )
pie3D(x=Table1, col=Col1, explode=0.1, radius=1.25)
dev.off()

#################################################### 
## Household Income                               ##
#################################################### 
BigData$household_income <- as.factor(as.integer(BigData$household_income))
levels(BigData$household_income) <- c('< 15k', 
                                      '15k-25k', 
                                      '25k-35k', 
                                      '35k-50k', 
                                      '50k-75k', 
                                      '75k-100k', 
                                      '> 100k')
jpeg(filename='Data/Plot2_Inc.jpg')
Table2 <- table(BigData$household_income)
par(las=2) # make label text perpendicular to axis
barplot(Table2, col='darkolivegreen', border='darkolivegreen')
dev.off()

#################################################### 
## Children                                       ##
#################################################### 
BigData$children <- as.factor(BigData$children)
levels(BigData$children) <- c('No', 'Yes')
Table3 <- table(BigData$children)
jpeg(filename='Data/Plot3_Children.jpg')
pie3D(x=Table3, explode=0.1, radius=1.25)
dev.off()

#################################################### 
## Census Region                                  ##
#################################################### 
BigData$census_region <- as.factor(BigData$census_region)
levels(BigData$census_region) <- c('Unknown', 'Northeast', 'North Central', 'South', 'West')
Table4 <- table(BigData$census_region)
jpeg(filename='Data/Plot4_Census.jpg')
barplot(Table4, col='cyan4', border='cyan4')
dev.off()

#################################################### 
## Household Oldest Age                           ##
#################################################### 
BigData$hoh_oldest_age <- as.factor(as.integer(BigData$hoh_oldest_age))
levels(BigData$hoh_oldest_age) <- c('18-20', '21-24', '25-29', '30-34', '35-39', '40-44', 
                                    '45-49', '50-54', '55-59', '60-64', '65 and over', 
                                    'Missing')
jpeg(filename='Data/Plot5_Age.jpg')
par(las=2) # make label text perpendicular to axis
barplot(table(BigData$hoh_oldest_age), col='dark red', border='dark red')
dev.off()

#################################################### 
## By Zip Code                                    ##
#################################################### 
byZipCode <- as.data.frame(table(BigData$zip_code))
names(byZipCode) <- c('zip', 'Num_Households')
data(zipcode)

byZipCode <- merge(x=byZipCode, y=zipcode, by='zip', all.x=T, all.y=F)
byZipCode$MainLand <- sapply(byZipCode$state, FUN=function(state){state %notin% c('AK', 'HI')})

byState <- aggregate(byZipCode$Num_Households, by=list(byZipCode$state), FUN=sum)
names(byState) <- c('state', 'HH_byState')

byZipCode <- merge(x=byZipCode, y=byState, by='state')

jpeg(filename='Data/Plot6_Zip.jpg')
hist(byZipCode$Num_Households, xlab='Number of Households', main='', col='brown1', 
     border='brown1')
dev.off()

#qplot(x=longitude, y=latitude, data = byZipCode[byZipCode$MainLand==TRUE,], 
#      fill = HH_byState, geom = "polygon")

mtstates <- byState$HH_byState
names(mtstates) <- byState$state

# Convert state abbreviations to full state names: 
mtfullstatenames <- tolower(state.name[match(names(mtstates), state.abb)])
mtfullstatenames[names(mtstates)=="DC"] <- "district of columbia"
names(mtstates) <- mtfullstatenames

# use "cut" to group the count data into several groups
mtstatesb <- cut(mtstates, breaks= c(0, 250, 500, 1000, 2000, 4000, Inf), 
                 labels=c("< 250", "250 - 500", "500 - 1000", "1000 - 2000", "2000 - 4000", 
                          "> 4000"))  

library(maps)
usmap <- map("state")
mapstates <- sapply(strsplit(usmap$names, ":"), "[[", 1)
shapeFileN <- mtstatesb[match(mapstates,names(mtstates))]

cols <- rev(grey.colors(6))[shapeFileN]

jpeg(filename='Data/Plot7_State.jpg')
map("state", col= cols, fill=T)
legend('bottomleft', legend=levels(mtstatesb), fill = rev(grey.colors(6)), 
       bty="n")
dev.off()
