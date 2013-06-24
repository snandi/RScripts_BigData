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
  Vector.Out <- as.numeric(Vector.In)
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
  Vector.Out <- as.numeric(Vector.In)
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
  Vector.Out <- as.factor(as.numeric(Vector.In))
  levels(Vector.Out) <- c('Apparel',
                          'Shoes',
                          'Accessories',
                          'Jewelry & Watches',
                          'Other Apparel Items',
                          'Home Furniture',
                          'Home Appliances',
                          'Tools & Equipment',
                          'Kitchen & Dining',
                          'Bed & Bath',
                          'Garden & Patio',
                          'Pet Supplies',
                          'Food & Beverage',
                          'Automotive Accessories',
                          'Sport & Fitness',
                          'Health & Beauty',
                          'Art & Collectibles',
                          'Tobacco Products',
                          'Baby Supplies',
                          'Other Home & Living Items',
                          'Books & Magazines',
                          'Music',
                          'Movies & Videos',
                          'Other BMV',
                          'Desktop Computers',
                          'Laptop Computers',
                          'Handhelds, PDAs & Portable Devices',
                          'Printers, Monitors & Peripherals',
                          'Computer Software (excl',
                          'Other Computer Supplies',
                          'Audio & Video Equipment',
                          'Cameras & Equipment',
                          'Mobile Phones & Plans',
                          'Other Electronics & Supplies',
                          'PC Video Games',
                          'Console Video Games',
                          'Video Game Consoles & Accessories',
                          'Business Machines',
                          'Office Furniture',
                          'Office Supplies',
                          'Movie Tickets',
                          'Event Tickets',
                          'Air Travel',
                          'Hotel Reservations',
                          'Car Rental',
                          'Travel Packages',
                          'Other Travel',
                          'Online Content Sales',
                          'Online Service Subscriptions',
                          'Personals & Dating',
                          'Photo Printing Services',
                          'Shipping Services',
                          'Other Services',
                          'Toys & Games (excl',
                          'Arts, Crafts & Party Supplies',
                          'Other Toy & Game Items',
                          'Flowers',
                          'Greetings',
                          'Gift Certificates & Coupons',
                          'Other Flower & Gift Items',
                          'Unclassified')  
  return(Vector.Out)
}
####################################################

####################################################
fn_formatAllData <- function(Data){
  Data$racial_background <- try(fn_formatRacialBackground(Vector.In = Data$racial_background))
  Data$household_income <- try(fn_formatHHIncome(Vector.In = Data$household_income))
  Data$household_size <- try(fn_formatHHSize(Vector.In = Data$household_size))
  Data$event_date <- try(fn_formatEventDate(Vector.In = Data$event_date))
  Data$event_time <- try(fn_formatEventDate(Vector.In = Data$event_time))
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
