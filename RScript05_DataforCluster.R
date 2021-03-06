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
#DataPath.Scratch <- '/scratch/Nandi/'
DataPath.Scratch <- '~/peterq/scratch/'
OutputDataPath <- '~/peterq/PA_BigData/RScripts_BigData/Data/'
#Filename.Header <- paste(FilePath, 'HeaderFile_BigMem.R', sep='')
Filename.Header <- paste('~/RScripts/HeaderFile_Nandi.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
#################################################### 

#################################################### 
## Data Input                                     ##
#################################################### 
FilePrefix <- 'janfeb'
Filename.In <- paste(OutputDataPath, FilePrefix, '_Purchasers.RData', sep='')
load(Filename.In)

# hist(Purchasers$total_amt_spent[Purchasers$total_amt_spent > 0], breaks=40)
# summary(Purchasers$total_amt_spent[Purchasers$total_amt_spent > 0], breaks=40)

Colnames <- fn_getColumnNames(FilePrefix='janfeb', DataPath=DataPath)
Colnames.Keep <- c("machine_id", "pages_viewed", "duration", "prod_totprice",
                   "census_region", "household_size", "hoh_oldest_age", 
                   "household_income", "children", "racial_background", "zip_code")

####################################################
# This part is parallelized for bigmem             #
####################################################
NumCores <- 12   ## Number of cores to be used
cl <- makeSOCKcluster(as.numeric(NumCores))
registerDoSNOW(cl)

Data <- foreach(Indices=1:32, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_prepDataforCluster(FilePrefix='janfeb', FileIndex=Indices, DataPath=DataPath.Scratch, 
                        Colnames=Colnames, Colnames.Keep=Colnames.Keep)

stopCluster(cl)
rm(cl)

####################################################
# This part is just an example for one dataset     #
####################################################
# Data <- fn_prepDataforCluster(FilePrefix='janfeb', FileIndex=1, 
#                                DataPath=DataPath.Scratch, Colnames=Colnames, 
#                                Colnames.Keep=Colnames.Keep)

Filename.Out <- paste(OutputDataPath, FilePrefix, '_', 'forCluster.RData', sep='')
save(Data, file=Filename.Out)
