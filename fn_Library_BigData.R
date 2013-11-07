#################################################### 
## This script is the function library for Big    ##
## Data project                                   ##
#################################################### 

#################################################### 
## This function reads in one dataset at a time   ##
#################################################### 
fn_readBigData <- function(FilePrefix='janfeb', FileIndex, DataPath, Colnames, 
                           Colnames.Keep, Unique=TRUE){
  Filename <- paste(DataPath, FilePrefix, '_', FileIndex, '.txt', sep='')
  print(Filename)
  Data <- read.table(file=Filename, header=FALSE, sep='\t', colClasses='character', 
                     skip=0, quote='', stringsAsFactors=FALSE, fill=TRUE, 
                     comment.char='')
  colnames(Data) <- Colnames
  if(Unique == TRUE){
    return(unique(Data[,Colnames.Keep]))
  } else{
    return(Data[,Colnames.Keep])
  }
} 
#################################################### 

############## Convert NAs to Zero #################
na.is.zero <- function(X)
{
  X1 <- X
  X1[is.na(X)] <- 0.0
  return(X1)
}
####################################################

####################################################
"%notin%" <- function(x, y){
  if(x %in% y){
    return(FALSE)
  } else{
    return(TRUE)
  }
}
####################################################

####################################################
"%w/o%" <- function(x, y){
  return(x[!x %in% y])
}
####################################################

####################################################
fn_formatRacialBackground <- function(Vector.In){
  Vector.Out <- sapply(X = Vector.In, FUN=function(v){switch(v, 
                                                             '1' = 'White', 
                                                             '2' = 'Black',
                                                             '3' = 'Asian',
                                                             '5' = 'Other')})
  Vector.Out <- as.factor(Vector.Out)
  return(Vector.Out)
}
####################################################

####################################################
fn_formatEventDate <- function(Vector.In){
  Vector.Out <- as.Date(x=Vector.In, format='%m/%d/%Y')
  return(Vector.Out)
}
####################################################

####################################################
fn_formatEventTime <- function(Vector.In){
  Vector.Out <- chron(times.=Vector.In)
  return(Vector.Out)
}
####################################################

#################################################### 
fn_formatHHIncome <- function(Vector.In){
  Vector.Out <- as.factor(as.integer(Vector.In))
  levels(Vector.Out) <- c('< 15k', 
                          '15k-25k', 
                          '25k-35k', 
                          '35k-50k', 
                          '50k-75k', 
                          '75k-100k', 
                          '> 100k')
  return(Vector.Out)
}
####################################################

####################################################
fn_formatChildren <- function(Vector.In){
  Vector.Out <- as.factor(Vector.In)
  levels(Vector.Out) <- c('No', 'Yes')
  return(Vector.Out)
}
####################################################

####################################################
fn_formatCensus <- function(Vector.In){
  Vector.Out <- as.factor(Vector.In)
  levels(Vector.Out) <- c('Unknown', 'Northeast', 'North Central', 'South', 'West')
  return(Vector.Out)
}
####################################################

####################################################
fn_formatHHOldestAge <- function(Vector.In){
  Vector.Out <- as.factor(as.integer(Vector.In))
  levels(Vector.Out) <- c('18-20', '21-24', '25-29', '30-34', '35-39', '40-44', '45-49', 
                         '50-54', '55-59', '60-64', '65 and over', 'Missing')
  return(Vector.Out)
}
####################################################

####################################################
fn_formatPagesViewed <- function(Vector.In){
  Vector.Out <- as.numeric(Vector.In)
  return(Vector.Out)
}
####################################################

####################################################
fn_formatDuration <- function(Vector.In){
  Vector.Out <- na.is.zero(as.numeric(Vector.In))
  return(Vector.Out)
}
####################################################

####################################################
fn_formatBasketTot <- function(Vector.In){
  Vector.Out <- na.is.zero(as.numeric(na.is.zero(Vector.In)))
  return(Vector.Out)
}
####################################################

####################################################
fn_formatProdTotPrice <- function(Vector.In){
  Vector.Out <- na.is.zero(as.numeric(na.is.zero(Vector.In)))
  return(Vector.Out)
}
####################################################

####################################################
fn_formatProdQty <- function(Vector.In){
  Vector.Out <- na.is.zero(as.numeric(Vector.In))
  return(Vector.Out)
}
####################################################

####################################################
fn_formatHHSize <- function(Vector.In){
  Vector.Out <- na.is.zero(as.numeric(Vector.In))
  return(Vector.Out)
}
####################################################

####################################################
fn_formatProdCategory <- function(Vector.In){
  Vector.Out <- na.is.zero(as.numeric(Vector.In))
  return(Vector.Out)
}

fn_formatProdCategory.F <- function(Vector.In){
  Vector1 <- as.character(Vector.In)
  Vector2 <- sapply(X=Vector1, FUN=function(Val){if(Val=='0') 'No Purchase'
                                                 else if(Val=='1') 'Apparel'
                                                 else if(Val=='2') 'Shoes'
                                                 else if(Val=='3') 'Accessories'
                                                 else if(Val=='4') 'Jewelry & Watches'
                                                 else if(Val=='5') 'Other Apparel Items'
                                                 else if(Val=='6') 'Home Furniture'
                                                 else if(Val=='7') 'Home Appliances'
                                                 else if(Val=='8') 'Tools & Equipment'
                                                 else if(Val=='9') 'Kitchen & Dining'
                                                 else if(Val=='10') 'Bed & Bath'
                                                 else if(Val=='11') 'Garden & Patio'
                                                 else if(Val=='12') 'Pet Supplies'
                                                 else if(Val=='13') 'Food & Beverage'
                                                 else if(Val=='14') 'Automotive Accessories'
                                                 else if(Val=='15') 'Sport & Fitness'
                                                 else if(Val=='16') 'Health & Beauty'    
                                                 else if(Val=='17') 'Art & Collectibles'
                                                 else if(Val=='18') 'Tobacco Products'
                                                 else if(Val=='19') 'Baby Supplies'
                                                 else if(Val=='20') 'Other Home & Living Items'
                                                 else if(Val=='21') 'Books & Magazines'
                                                 else if(Val=='22') 'Music'
                                                 else if(Val=='23') 'Movies & Videos'
                                                 else if(Val=='24') 'Other BMV'
                                                 else if(Val=='25') 'Desktop Computers'
                                                 else if(Val=='26') 'Laptop Computers'
                                                 else if(Val=='27') 'Handhelds, PDAs & Portable Devices'
                                                 else if(Val=='28') 'Printers, Monitors & Peripherals'
                                                 else if(Val=='29') 'Computer Software (excl'
                                                 else if(Val=='30') 'Other Computer Supplies'
                                                 else if(Val=='31') 'Audio & Video Equipment'
                                                 else if(Val=='32') 'Cameras & Equipment'
                                                 else if(Val=='33') 'Mobile Phones & Plans'
                                                 else if(Val=='34') 'Other Electronics & Supplies'
                                                 else if(Val=='35') 'PC Video Games'
                                                 else if(Val=='36') 'Console Video Games'
                                                 else if(Val=='37') 'Video Game Consoles & Accessories'
                                                 else if(Val=='38') 'Business Machines'
                                                 else if(Val=='39') 'Office Furniture'
                                                 else if(Val=='40') 'Office Supplies'
                                                 else if(Val=='41') 'Movie Tickets'
                                                 else if(Val=='42') 'Event Tickets'
                                                 else if(Val=='43') 'Air Travel'
                                                 else if(Val=='44') 'Hotel Reservations'
                                                 else if(Val=='45') 'Car Rental'
                                                 else if(Val=='46') 'Travel Packages'
                                                 else if(Val=='47') 'Other Travel'
                                                 else if(Val=='48') 'Online Content Sales'
                                                 else if(Val=='49') 'Online Service Subscriptions'
                                                 else if(Val=='50') 'Personals & Dating'
                                                 else if(Val=='51') 'Photo Printing Services'
                                                 else if(Val=='52') 'Shipping Services'
                                                 else if(Val=='53') 'Other Services'
                                                 else if(Val=='54') 'Toys & Games (excl PC Games)'
                                                 else if(Val=='55') 'Arts, Crafts & Party Supplies'
                                                 else if(Val=='56') 'Other Toy & Game Items'
                                                 else if(Val=='57') 'Flowers'
                                                 else if(Val=='58') 'Greetings'
                                                 else if(Val=='59') 'Gift Certificates & Coupons'
                                                 else if(Val=='60') 'Other Flower & Gift Items'
                                                 else if(Val=='99') 'Unclassified'
                                                 else if(Val=='0') 'No Purchase'})
  return(as.character(Vector2))
}
####################################################
  
####################################################
fn_formatAllData <- function(Data){
  Data$racial_background <- try(fn_formatRacialBackground(Vector.In = Data$racial_background))
  Data$household_income <- try(fn_formatHHIncome(Vector.In = Data$household_income))
  Data$household_size <- try(fn_formatHHSize(Vector.In = Data$household_size))
  Data$event_date <- try(fn_formatEventDate(Vector.In = Data$event_date))
  Data$event_time <- try(fn_formatEventTime(Vector.In = Data$event_time))
  Data$children <- try(fn_formatChildren(Vector.In = Data$children))
  Data$census_region <- try(fn_formatCensus(Vector.In = Data$census_region))
  Data$hoh_oldest_age <- try(fn_formatHHOldestAge(Vector.In = Data$hoh_oldest_age))
  Data$pages_viewed <- try(fn_formatPagesViewed(Vector.In = Data$pages_viewed))
  Data$duration <- try(fn_formatDuration(Vector.In = Data$duration))
  Data$prod_category_id <- try(fn_formatProdCategory(Vector.In = Data$prod_category_id))
  Data$prod_qty <- try(fn_formatProdQty(Vector.In = Data$prod_qty))
  Data$prod_totprice <- try(fn_formatProdTotPrice(Vector.In = Data$prod_totprice))
  Data$duration <- try(fn_formatDuration(Vector.In = Data$duration))
  Data$basket_tot <- try(fn_formatBasketTot(Vector.In = Data$basket_tot))
  
  return(Data)
}
####################################################

#################################################### 
## This function returns the column names         ##
#################################################### 
fn_getColumnNames <- function(FilePrefix='janfeb', DataPath){
  Filename <- paste(DataPath, FilePrefix, '.txt', sep='')
  Data <- read.table(file=Filename, header=TRUE, sep='\t', nrows=10,
                     colClasses='character', skip=0, quote='', 
                     stringsAsFactors=FALSE, fill=TRUE, 
                     comment.char='')
  ## quote='' allows the columns to have ' and " in the strings
  ## colClasses is set to 'character' because some numeric values are too long
  ## comment.char='' allows the columns to have # in the strings
  Colnames <- colnames(Data)
  return(Colnames)
} 
#################################################### 

#################################################### 
## This function prepares data for Cluster        ##
## analysis                                       ##
#################################################### 
fn_prepDataforCluster <- function(FilePrefix, FileIndex, DataPath, Colnames, Colnames.Keep){
  Data <- fn_readBigData(FilePrefix=FilePrefix, FileIndex=FileIndex, DataPath=DataPath, 
                         Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=FALSE)
  
#   Data$racial_background <- try(fn_formatRacialBackground(Vector.In = Data$racial_background))
#   Data$household_income <- try(fn_formatHHIncome(Vector.In = Data$household_income))
#   Data$children <- try(fn_formatChildren(Vector.In = Data$children))
#   Data$census_region <- try(fn_formatCensus(Vector.In = Data$census_region))
#   Data$hoh_oldest_age <- try(fn_formatHHOldestAge(Vector.In = Data$hoh_oldest_age))
  Data$pages_viewed <- try(fn_formatPagesViewed(Vector.In = Data$pages_viewed))
  Data$duration <- try(fn_formatDuration(Vector.In = Data$duration))
  Data$prod_totprice <- try(fn_formatProdTotPrice(Vector.In = Data$prod_totprice))
#   Data$duration <- try(fn_formatDuration(Vector.In = Data$duration))
#   Data$household_size <- try(fn_formatHHSize(Vector.In = Data$household_size))
  
  Data1 <- aggregate(x=Data[,c('pages_viewed', 'duration', 'prod_totprice')], 
                     by=list(Data$machine_id), FUN=sum)
  colnames(Data1)[colnames(Data1) == 'Group.1'] <- 'machine_id'
  Colnames.2 <- Colnames.Keep %w/o% c('pages_viewed', 'duration', 'prod_totprice')
  Data2 <- merge(Data1, unique(Data[,Colnames.2]))
  return(Data2)  
}
#################################################### 

#################################################### 
## This function returns data of customers that   ##
## have made at least one travel related purchase ##
#################################################### 
fn_getTravelPurchasers <- function(FilePrefix, FileIndex, DataPath, Colnames, 
                                   Colnames.Keep, Purchasers.Travel){
  Data <- fn_readBigData(FilePrefix=FilePrefix, FileIndex=FileIndex, DataPath=DataPath, 
                         Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=FALSE)
  Data1 <- Data[Data$machine_id %in% unique(Purchasers.Travel$machine_id),]
  Data1 <- fn_formatAllData(Data=Data1)
  return(Data1)
}
#################################################### 

#################################################### 
## This function returns the websites related to  ##
## search conducted for travel related purchases  ##
#################################################### 
fn_getSearchSites <- function(BigData, category_id){
  Data.Trans <- subset(x = BigData, prod_category_id %in% category_id)
  Data.Trans <- unique(Data.Trans[,c('machine_id', 'event_date')])
  ## This data contain dates of transactions
  
  for(machine in Data.Trans$machine_id){
    NewRow <- subset(Data.Trans, machine_id == machine)
    NewRow$event_date <- NewRow$event_date - 1
    Data.Trans <- rbind(Data.Trans, NewRow)
  }
  
  Data.Trans <- Data.Trans[order(Data.Trans$machine_id, Data.Trans$event_date), ]
  
  Data.Trans$Search <- 1
  
  Data3 <- merge(x = BigData, y = Data.Trans, by = c('machine_id', 'event_date'), all.x = T, all.y = F)
  Data3$Search <- na.is.zero(Data3$Search)
  
  Websites_Category <- Data3[Data3$Search==1,'domain_name']
  Search_Category <- table(Websites_Category)
  Search_Category <- sort(Search_Category, decreasing=T)
  return(Search_Category)
} 
#################################################### 

fn_getPurchaseSites <- function(Data, category){
  Data1 <- subset(x=Data, prod_category == category)
  Data1 <- Data1[,c('prod_category', 'domain_name')]
  Table_Category <- table(subset(x=Data1, prod_category==category)$domain_name)
  Table_Category <- sort(Table_Category, decreasing=T)
  return(Table_Category)  
}

#################################################### 
## This function returns the hotel data including ##
## ratings info, filled in by Yu Chen             ##
#################################################### 
fn_returnHotelRatings <- function(filename='HotelDetails_v3_Nandi.csv'){
  Filename <- paste(OutputDataPath, filename, sep='')
  Data <- read.table(file=Filename, header=TRUE, sep=',', row.names=NULL,
                               skip=0, quote='', stringsAsFactors=FALSE,
                               fill=TRUE, comment.char='')
  Data$event_date <- try(fn_formatEventDate(Vector.In = Data$event_date))
  Data$event_time <- try(fn_formatEventTime(Vector.In = Data$event_time))
  Data$pages_viewed <- try(fn_formatPagesViewed(Vector.In = Data$pages_viewed))
  Data$duration <- try(fn_formatDuration(Vector.In = Data$duration))
  Data$prod_category_id <- try(fn_formatProdCategory(Vector.In = Data$prod_category_id))
  Data$prod_qty <- try(fn_formatProdQty(Vector.In = Data$prod_qty))
  Data$prod_totprice <- try(fn_formatProdTotPrice(Vector.In = Data$prod_totprice))
  Data$basket_tot <- try(fn_formatBasketTot(Vector.In = Data$basket_tot))

  Data$StartDate <- try(fn_formatEventDate(Vector.In = Data$StartDate))
  Data$EndDate <- try(fn_formatEventDate(Vector.In = Data$EndDate))
  Data$StarRating <- try(fn_formatDuration(Vector.In = Data$StarRating))
  Data$IsWeekend <- try(fn_formatDuration(Vector.In = Data$IsWeekend))

  return(Data)
}

#################################################### 
## This function ESTimates the search parameters ##
## for hotel related transactions                 ##
#################################################### 
fn_estHotelSearch <- function(RowIndex, RegData=RegData.Hotel, BrowseData=Data.Hotel, 
                              NDaysBack=1){
  ## BrData stands for Browsing data
  RegData.Row <- RegData[RowIndex,]
  McId <- RegData[RowIndex, 'machine_id']
  TransactionDateTime <- as.POSIXct(paste(as.character(RegData[RowIndex, 'event_date']), 
                                          as.character(RegData[RowIndex, 'event_time'])))
  BrData <- subset(BrowseData, machine_id == McId)
  BrData$Search_EndTime <- as.POSIXct(paste(as.character(BrData$event_date), 
                                            as.character(BrData$event_time)))
  
  BrData <- subset(BrData, Search_EndTime <= TransactionDateTime)
  Time2 <- TransactionDateTime - (NDaysBack*24*3600)
  BrData <- subset(BrData, Search_EndTime >= Time2)
  
  BrData$Search_Ind <- BrData$domain_name %in% PurchaseSites_Hotel
  BrData$Search_Duration <- BrData$Search_Ind * BrData$duration
  BrData$Search_NumPages <- BrData$Search_Ind * BrData$pages_viewed
  
  RegData.Row['Search_Duration'] <- sum(BrData$Search_Duration)
  RegData.Row['Search_NumPages'] <- sum(BrData$Search_NumPages)
  RegData.Row['Search_NumWebsites'] <- sum(BrData$Search_Ind)
  
  rm(BrData)
  return(RegData.Row)
}
