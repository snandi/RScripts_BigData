#################################################### 
## The author of this script is: Subhrangshu Nandi##
## Project: Big Data                              ##
## First Draft: 2013-06-26                        ##
#################################################### 

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

#################################################### 
## This script is a modification RScript04. In    ##
## addition to purchaser total amount, this script##
## will record the purchase amount by category    ##
## This script is to read in the big data, one    ##
## small file at a time, separate out the         ##
## machines that have made at least one purchase  ##
## during Jan-Feb and save the file on the hard   ##
## drive.                                         ##
## This should run on BigMems                     ##
## This is a parallelized script                  ##
#################################################### 

#################################################### 
## Initialize Header file and function library    ##
#################################################### 
FilePath <- '~/peterq/PA_BigData/RScripts_BigData/'
DataPath <- '~/peterq/PA_BigData/Comscore data/'
DataPath.Scratch <- '/scratch/Nandi/'
#DataPath.Scratch <- '~/peterq/scratch/'
OutputDataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
#Filename.Header <- paste(FilePath, 'HeaderFile_BigMem.R', sep='')
Filename.Header <- paste('~/RScripts/HeaderFile_Nandi.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
#################################################### 

#################################################### 
## Data Input                                     ##
#################################################### 
#setwd(DataPath)
FilePrefix <- 'mayjune'
Colnames <- fn_getColumnNames(FilePrefix=FilePrefix, DataPath=DataPath)
Colnames.Keep <- c('machine_id', 'prod_category_id', 'prod_totprice')

Time1 <- Sys.time()
####################################################
# This part is parallelized for bigmem             #
####################################################
NumCores <- 12   ## Number of cores to be used
cl <- makeSOCKcluster(as.numeric(NumCores))
registerDoSNOW(cl)

Data <- foreach(Indices=1:28, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_readBigData(FilePrefix=FilePrefix, FileIndex=Indices, DataPath=DataPath.Scratch, 
                 Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=FALSE)

stopCluster(cl)
rm(cl)

####################################################
# This part is just an example for one dataset     #
####################################################
# Data <- fn_readBigData(FilePrefix='janfeb', FileIndex=1, DataPath=DataPath.Scratch, 
#                          Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=FALSE)

Data$prod_totprice <- fn_formatProdTotPrice(Vector.In = Data$prod_totprice)
Data$prod_category_id <- fn_formatProdCategory(Vector.In = Data$prod_category_id)
#Data$prod_qty <- fn_formatProdQty(Vector.In = Data$prod_qty)
#str(Data)

Purchasers <- stats::aggregate(x = Data[,'prod_totprice'], 
                               by = list(Data$machine_id, Data$prod_category_id), 
                               FUN = sum)
print(Sys.time() - Time1)

colnames(Purchasers) <- c('machine_id', 'prod_category_id', 'total_amt_spent')
Purchasers <- Purchasers[order(Purchasers$machine_id),]
Purchasers <- subset(x=Purchasers, subset=prod_category_id > 0)
Purchasers$prod_category_id <- fn_formatProdCategory.F(Vector.In = Purchasers$prod_category_id)

# View(Purchasers)
Filename.Out <- paste(OutputDataPath, FilePrefix, '_', 'Purchasers_wCategory.RData', sep='')
save(Purchasers, file=Filename.Out)
