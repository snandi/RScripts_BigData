#################################################### 
## The author of this script is: Subhrangshu Nandi##
## Project: Big Data                              ##
## First Draft: 2013-06-10                        ##
#################################################### 

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

#################################################### 
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
Colnames <- fn_getColumnNames(FilePrefix='janfeb', DataPath=DataPath)
Colnames.Keep <- c('machine_id', 'prod_totprice')
FilePrefix <- 'janfeb'

Time1 <- Sys.time()
####################################################
# This part is parallelized for bigmem             #
####################################################
NumCores <- 8   ## Number of cores to be used
cl <- makeSOCKcluster(as.numeric(NumCores))
registerDoSNOW(cl)

Data <- foreach(Indices=1:32, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_readBigData(FilePrefix='janfeb', FileIndex=Indices, DataPath=DataPath.Scratch, 
                 Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=FALSE)

stopCluster(cl)
rm(cl)

####################################################
# This part is just an example for one dataset     #
####################################################
# Data <- fn_readBigData(FilePrefix='janfeb', FileIndex=1, DataPath=DataPath.Scratch, 
#                         Colnames=Colnames, Colnames.Keep=Colnames.Keep, Unique=FALSE)

Data$prod_totprice <- fn_formatProdTotPrice(Vector.In = Data$prod_totprice)
print(Sys.time() - Time1)

Purchasers <- stats::aggregate(x = Data[,'prod_totprice'], 
                               by = list(Data[,'machine_id']), 
                               FUN = sum)

colnames(Purchasers) <- c('machine_id', 'total_amt_spent')
Purchasers$purchased <- (Purchasers$total_amt_spent > 0)
Filename.Out <- paste(OutputDataPath, FilePrefix, '_', 'Purchasers.RData', sep='')
save(Purchasers, file=Filename.Out)
