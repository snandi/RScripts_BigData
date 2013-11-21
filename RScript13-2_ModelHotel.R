################################################################## 
## The author of this script is: Subhrangshu Nandi              ##
## Project: Big Data                                            ##
## First Draft: 2013-08-19                                      ##
################################################################## 

################################################################## 
## This script reads in the data created by RScript12-2, which  ##
## is saved as RegData_Hotel.RData, and fits the second search  ##
## model. This includes ratings prepared by Yu Chen. This also  ##
## conducts cluster analysis
################################################################## 

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

################################################################## 
## Initialize Header file and function library                  ##
################################################################## 
FilePath <- '~/peterq/PA_BigData/RScripts_BigData/'
#FilePath <- 'C:/Users/Nandi/Documents/Google Drive/UWMadison/PA_BigData/RScripts_BigData/'
DataPath <- '~/peterq/PA_BigData/Comscore data/'
#DataPath.Scratch <- '/scratch/Nandi/'
DataPath.Scratch <- '~/peterq/scratch/'
OutputDataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
#OutputDataPath <- 'C:/Users/Nandi/Documents/Google Drive/UWMadison/PA_BigData/RScripts_BigData/Data/'
Filename.Header <- paste(FilePath, 'HeaderFile_BigMem.R', sep='')
#Filename.Header <- paste('C:/Users/Nandi/Documents/Google Drive/UWMadison/RScripts/HeaderFile_Nandi.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
################################################################## 

################################################################## 
## Data Input                                                   ##
################################################################## 
Filename.In <- paste(OutputDataPath, 'RegData_Hotel_Price.RData', sep='')
load(Filename.In)

Filename.In <- paste(OutputDataPath, 'RegData_Hotel_Rating.RData', sep='')
load(Filename.In)

Filename.In <- paste(OutputDataPath, 'RegData_Hotel_All.RData', sep='')
load(Filename.In)
# assign(x='RegData.Hotel', value=RegData1.Hotel, envir=.GlobalEnv)
# rm(RegData1.Hotel)

#RegData <- RegData.Hotel.Price
#RegData <- RegData.Hotel.Rating
RegData <- RegData.Hotel
#RegData <- RegData[-1812,]


cor(RegData[,c('Search_Duration', 'Search_NumPages', 'Search_NumWebsites')])
summary(RegData$DaysInAdvance)
str(RegData)
################################################################## 
## Price Model                                                  ##
################################################################## 
Data <- subset(RegData, PricePerNight > 0 & StarRating > 0)

PriceModel0 <- lm(log(PricePerNight) ~ StarRating, 
                  data=Data)
summary(PriceModel0)
plot(residuals(PriceModel0) ~ Data$DaysInAdvance)
lines(lowess(y=residuals(PriceModel0), x=Data$DaysInAdvance), col="Red")

PriceModel1 <- lm(log(PricePerNight) ~ StarRating + DaysInAdvance, 
                  data=Data)
summary(PriceModel1)
layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(PriceModel1)
layout(matrix(1:1))
X <- model.matrix(PriceModel1)
AIC(object=PriceModel1, k=log(dim(X)[2]))

PriceModel2 <- lm(log(PricePerNight) ~ StarRating + DaysInAdvance + I(DaysInAdvance^2), 
                  data=Data)
anova(PriceModel1, PriceModel2)
X <- model.matrix(PriceModel2)
AIC(object=PriceModel2, k=log(dim(X)[2]))
## Second order term for DaysInAdvance is not significant

PriceModel3 <- lm(log(PricePerNight) ~ as.factor(StarRating) + DaysInAdvance, 
                  data=Data)
summary(PriceModel3)
layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(PriceModel3)
layout(matrix(1:1))

PriceModel4 <- lm(log(PricePerNight) ~ StarRating + DaysInAdvance + IsWeekend, 
                  data=Data)
summary(PriceModel4)
anova(PriceModel1, PriceModel4)
plot(residuals(PriceModel4) ~ Data$StayDuration)
lines(lowess(y=residuals(PriceModel4), x=Data$StayDuration), col="Red")

PriceModel5 <- lm(log(PricePerNight) ~ StarRating + DaysInAdvance + StayDuration, 
                  data=Data)
summary(PriceModel5)

PriceModel6 <- lm(log(PricePerNight) ~ StarRating + DaysInAdvance + 
                    StayDuration + as.factor(domain_name), 
                  data=Data)
summary(PriceModel6)
layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(PriceModel6)
layout(matrix(1:1))
plot(PriceModel6, 1, main='Fit of PricePerNight')

PriceModel7 <- lm(log(PricePerNight) ~ StarRating + DaysInAdvance + 
                    StayDuration + as.factor(domain_name) + as.factor(HotelName), 
                  data=Data)
summary(PriceModel7)
X <- model.matrix(PriceModel7)
AIC(object=PriceModel7, k=log(dim(X)[2]))

################################################################## 
## Rating Model                                                  ##
################################################################## 
RatingModel6 <- lm(StarRating ~ log(PricePerNight) + DaysInAdvance + StayDuration + 
                    as.factor(domain_name), 
                  data=Data)
summary(RatingModel6)
layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(RatingModel6)
layout(matrix(1:1))

################################################################## 
## Search Model - Duration                                      ##
################################################################## 
RegData$PricePerNight[RegData$PricePerNight == 0] <- NA

hist(RegData.Hotel$Search_Duration, breaks=80)
hist(log(RegData.Hotel$Search_Duration), breaks=40)

plot(PricePerNight ~ StarRating, data=RegData)

hist(RegData$PricePerNight, breaks=40)
summary(RegData$Search_Duration)
View(subset(RegData, Search_Duration == 0))
View(subset(RegData, Search_NumWebsites == 0))

## Temporarily assign the Search duration to duration. Look at it in details later
RegData[RegData$Search_Duration == 0, 'Search_Duration'] <- RegData[RegData$Search_Duration == 0, 'duration']

Model1.Duration <- lm(log(Search_Duration) ~ PricePerNight + prod_qty +
                      StayDuration + as.factor(Tran_DayOfWeek) + IsWeekend,
                      data=RegData)
summary(Model1.Duration)
layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(Model1.Duration)
layout(matrix(1:1))

Model2.Duration <- lm(log(Search_Duration) ~ PricePerNight + prod_qty +
                        StayDuration + as.factor(Tran_DayOfWeek) + IsWeekend + 
                        DaysInAdvance,
                      data=RegData)
summary(Model2.Duration)
anova(Model1.Duration, Model2.Duration)

Model3.Duration <- lm(log(Search_Duration) ~ PricePerNight + prod_qty +
                        StayDuration + as.factor(Tran_DayOfWeek) + IsWeekend + 
                        DaysInAdvance + as.factor(household_income),
                      data=RegData)
summary(Model3.Duration)
anova(Model2.Duration, Model3.Duration)
vif(Model3.Duration)

Model4.Duration <- lm(log(Search_Duration) ~ PricePerNight + prod_qty +
                        StayDuration + as.factor(Tran_DayOfWeek) + IsWeekend + 
                        DaysInAdvance + as.factor(household_income) + as.factor(children),
                      data=RegData)
summary(Model4.Duration)
anova(Model3.Duration, Model4.Duration)
vif(Model4.Duration)

Model5.Duration <- lm(log(Search_Duration) ~ PricePerNight + prod_qty +
                        StayDuration + as.factor(Tran_DayOfWeek) + IsWeekend + 
                        DaysInAdvance + as.factor(household_income) + as.factor(children) + 
                        racial_background,
                      data=RegData)
summary(Model5.Duration)
anova(Model4.Duration, Model5.Duration)
vif(Model5.Duration)

Model6.Duration <- lm(log(Search_Duration) ~ PricePerNight + prod_qty +
                        StayDuration + as.factor(Tran_DayOfWeek) + IsWeekend + 
                        DaysInAdvance + as.factor(household_income) + as.factor(children) + 
                        racial_background + as.factor(domain_name),
                      data=RegData)
summary(Model6.Duration)
anova(Model5.Duration, Model6.Duration)
vif(Model6.Duration)
round(summary(Model6.Duration)$coefficients[,c(1, 4)], 4)

layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(Model6.Duration)
layout(matrix(1:1))

################################################################## 
## Search Model - Num Websites                                  ##
################################################################## 
Model1.Websites <- glm(log(Search_NumWebsites) ~ PricePerNight + prod_qty +
                      StayDuration + as.factor(Tran_DayOfWeek) + IsWeekend,
                      data=subset(RegData, Search_NumWebsites>0), family=poisson)
summary(Model1.Websites)
pchisq(q=deviance(Model1.Websites), df=df.residual(Model1.Websites), lower.tail=FALSE)

Model2.Websites <- glm(log(Search_NumWebsites) ~ PricePerNight + prod_qty +
                         StayDuration + as.factor(Tran_DayOfWeek) + IsWeekend +
                         DaysInAdvance,
                       data=subset(RegData, Search_NumWebsites>0), family=poisson)
summary(Model2.Websites)

Model6.Websites <- glm(log(Search_NumWebsites) ~ PricePerNight + prod_qty +
                         StayDuration + IsWeekend + 
                         DaysInAdvance + as.factor(household_income) + as.factor(children) + 
                         racial_background,
                       data=subset(RegData, Search_NumWebsites>0), family=poisson)
summary(Model6.Websites)
pchisq(q=deviance(Model6.Websites), df=df.residual(Model6.Websites), lower.tail=FALSE)
anova(Model2.Websites, Model6.Websites)


