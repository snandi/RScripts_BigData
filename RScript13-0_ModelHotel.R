################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-07-14                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript12-1, which  ##
## is saved as RegData_Hotel.RData, and fits the first logistic ##
## regression model.                                            ## 
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
Filename.In <- paste(OutputDataPath, 'RegData_Hotel.RData', sep='')
load(Filename.In)

################################################################## 
## Preliminary Summary Statistics                               ##
################################################################## 
str(RegData.Hotel)
RegData.Hotel$census_region <- fn_formatCensus(Vector.In=RegData.Hotel$census_region)
RegData.Hotel$household_size <- fn_formatHHSize(Vector.In=RegData.Hotel$household_size)
RegData.Hotel$hoh_oldest_age <- fn_formatHHOldestAge(Vector.In=RegData.Hotel$hoh_oldest_age)
RegData.Hotel$household_income <- fn_formatHHIncome(Vector.In=RegData.Hotel$household_income)
RegData.Hotel$children <- fn_formatChildren(Vector.In=RegData.Hotel$children)
RegData.Hotel$racial_background <- fn_formatRacialBackground(Vector.In=RegData.Hotel$racial_background)

cor(RegData.Hotel[,c('Search_Duration', 'Search_NumPages', 'Search_NumWebsites')])

jpeg(filename='../Meetings/2013-07-18/Plot1_SearchWebsites.jpg')
boxplot(log(Search_NumWebsites) ~ Star, data=RegData.Hotel, 
        col=(c("red","darkgreen")), notch=TRUE, cex.axis=0.75, las=1, 
        ylab='log(number of websites searched)',
        main='Boxplot of Number of Websites with Luxury (Green) & \n Economy (Red)')
dev.off()

jpeg(filename='../Meetings/2013-07-18/Plot2_byIncome.jpg')
boxplot(log(Search_NumWebsites) ~ Star*household_income, data=RegData.Hotel, 
        col=(c("red","darkgreen")), notch=TRUE, cex.axis=0.75, las=2, 
        ylab='log(number of websites searched)', 
        main='Boxplot of Number of Websites vs Household Income with \n Luxury (Green) & \n Economy (Red)')
dev.off()

Table1 <- table(subset(RegData.Hotel, Star==1)$household_income)
jpeg(filename='../Meetings/2013-07-18/Plot2a_byIncome.jpg')
par(las=2) # make label text perpendicular to axis
barplot(Table1, col='red', border='red', 
        main='Income distributions of Economy Hotel purchasers')
par(las=1)
dev.off()


Table2 <- table(subset(RegData.Hotel, Star==4)$household_income)
jpeg(filename='../Meetings/2013-07-18/Plot2b_byIncome.jpg')
par(las=2) # make label text perpendicular to axis
barplot(Table2, col='darkolivegreen', border='darkolivegreen', 
        main='Income distributions of Luxury Hotel purchasers')
par(las=1)
dev.off()

jpeg(filename='../Meetings/2013-07-18/Plot3_byHHSize.jpg')
boxplot(log(Search_NumWebsites) ~ Star*household_size, data=RegData.Hotel, 
        col=(c("red","darkgreen")), notch=TRUE, cex.axis=0.75, las=1, 
        ylab='log(number of websites searched)', 
        main='Boxplot of Number of Websites vs Household Size with \n Luxury (Green) & \n Economy (Red)')
dev.off()


################################################################## 
## Logistic Regression Model                                    ##
################################################################## 

Model1.1 <- glm(as.factor(Star) ~ Search_Duration + Search_NumWebsites, family='binomial', 
              data=RegData.Hotel)
summary(Model1.1)
pchisq(q=deviance(Model1.1), df=df.residual(Model1.1), lower=FALSE)

Model2.1 <- glm(as.factor(Star) ~ Search_Duration + Search_NumWebsites + household_size + 
  hoh_oldest_age + household_income + children, family='binomial', 
                data=RegData.Hotel)
summary(Model2.1)
pchisq(q=deviance(Model2.1), df=df.residual(Model2.1), lower=FALSE)

