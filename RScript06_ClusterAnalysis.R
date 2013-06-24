#################################################### 
## The author of this script is: Subhrangshu Nandi##
## Project: Big Data                              ##
## First Draft: 2013-06-12                        ##
#################################################### 

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

#################################################### 
## This script reads in the data prepared for     ##
## Cluster analysis and conducts k-means and      ##
## hierarchical cluster analysis                  ##
#################################################### 

#################################################### 
## Initialize Header file and function library    ##
#################################################### 
FilePath <- 'C:/Users/Nandi/Documents/Google Drive/UWMadison/PA_BigData/RScripts_BigData/'
#FilePath <- '~/peterq/PA_BigData/RScripts_BigData/'
DataPath <- 'C:/Users/Nandi/Documents/Google Drive/UWMadison/PA_BigData/RScripts_BigData/Data/'
#DataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
OutputDataPath <- 'C:/Users/Nandi/Documents/Google Drive/UWMadison/PA_BigData/RScripts_BigData/Data/'
#OutputDataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
Filename.Header <- paste('C:/Users/Nandi/Documents/Google Drive/UWMadison/RScripts/HeaderFile_Nandi.R', sep='')
#Filename.Header <- paste('~/RScripts/HeaderFile_Nandi.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
#################################################### 

#################################################### 
## Data Input                                     ##
#################################################### 
Filename <- paste(DataPath, 'janfeb_forCluster.RData', sep='')
load(Filename)

Data <- Data[-c(11756),]
str(Data)

Data$racial_background <- try(fn_formatRacialBackground(Vector.In = Data$racial_background))
Data$household_income <- try(fn_formatHHIncome(Vector.In = Data$household_income))
Data$children <- try(fn_formatChildren(Vector.In = Data$children))
Data$census_region <- try(fn_formatCensus(Vector.In = Data$census_region))
Data$hoh_oldest_age <- try(fn_formatHHOldestAge(Vector.In = Data$hoh_oldest_age))
Data$household_size <- try(fn_formatHHSize(Vector.In = Data$household_size))
Data$zip_code <- as.numeric(Data$zip_code)

length(unique(Data$machine_id))

#################################################### 
## K-means clustering                             ##
#################################################### 
Data1 <- subset(x = Data, select = -c(machine_id))
Data1 <- as.data.frame(sapply(X = Data1, FUN = function(v){as.numeric(v)}))

Colnames.Cluster <- c('pages_viewed', 'duration', 'prod_totprice', 'household_size', 
                      'hoh_oldest_age', 'household_income')

#################################################### 
## K-means K=1000, with both purchasers and non-  ##
## purchasers together                            ##
#################################################### 
Time1 <- Sys.time()
Cluster1 <- kmeans(x=Data1[,Colnames.Cluster], centers=1000, iter.max=40)
Centers1 <- as.data.frame(cbind(round(Cluster1$centers, 2), cluster_size=Cluster1$size))
print(Sys.time() - Time1)
write.csv(Centers1, file='../Meetings/2013-06-13 MSC/Centers1.csv')
jpeg(filename='../Meetings/2013-06-13 MSC/Plot1_Hist1.jpg')
hist(Centers1$cluster_size, breaks=40, xlab='Cluster Size', main='K-Mean clustering', 
     col='blue')
text(x=15, y=100, labels='K = 1000, with \n purchasers and \n non-purchasers', 
     col='blue')
dev.off()

#################################################### 
## K-means K=1000, with purchasers and non-       ##
## purchasers separately                          ##
#################################################### 
Cluster2.P <- kmeans(x=Data1[Data1$prod_totprice > 0,Colnames.Cluster], 
                       centers=200, iter.max=40)
Centers2.P <- as.data.frame(cbind(round(Cluster2.P$centers, 2), 
                                  cluster_size=Cluster2.P$size))
jpeg(filename='../Meetings/2013-06-13 MSC/Plot2_Purchasers.jpg')
layout(matrix(1:4, ncol=2))
hist(Centers2.P$cluster_size, breaks=40, xlab='Cluster Size', main='K-Mean clustering', 
     col=colors()[114], border=colors()[114])
text(x=90, y=10, labels='K = 200, with \n purchasers only', 
     col='red')

hist(Centers2.P$prod_totprice, breaks=20, xlab='Total Purchase', 
     main='Histogram of Cluster Purchases', col=colors()[62], border=colors()[62])

plot(log(prod_totprice) ~ log(pages_viewed), data=Centers2.P, pch=19, col=colors()[134], 
     main='Purchase price vs Pages viewed')

plot(log(prod_totprice) ~ log(duration), data=Centers2.P, pch=19, col=colors()[51], 
     main='Purchase price vs Duration')
layout(matrix(1:1))
dev.off()

Cluster3.NoPur <- kmeans(x=Data1[Data1$prod_totprice == 0,c('pages_viewed', 'duration', 'prod_totprice', 'household_size')], 
                       centers=100, iter.max=80)

#################################################### 
## Some interesting exploratory analysis          ##
#################################################### 
Data$Purchaser <- Data$prod_totprice > 0 

jpeg(filename='../Meetings/2013-06-13 MSC/Plot3_Box1.jpg')
boxplot(log(duration + 1) ~ Purchaser*household_income, data=Data, 
        col=(c("red","darkgreen")), notch=TRUE, cex.axis=0.75, las=2, 
        main='Boxplot of Duration vs Household Income with \n Purchasers (Green) & \n Non-Purchasers (Red)')
dev.off()

jpeg(filename='../Meetings/2013-06-13 MSC/Plot4_Box2.jpg')
boxplot(log(pages_viewed + 1) ~ Purchaser*racial_background, data=Data, 
        col=(c("red","darkgreen")), notch=TRUE, cex.axis=0.75, las=2, 
        main='Boxplot of Pages Viewed vs Racial Background with \n Purchasers (Green) & \n Non-Purchasers (Red)')
dev.off()


