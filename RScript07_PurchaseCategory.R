#################################################### 
## The author of this script is: Subhrangshu Nandi##
## Project: Big Data                              ##
## First Draft: 2013-06-13                        ##
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
Colnames.Keep <- c("machine_id", "prod_totprice",
                   "prod_category_id", "prod_qty")

####################################################
# This part is parallelized for bigmem             #
####################################################
NumCores <- 12   ## Number of cores to be used
cl <- makeSOCKcluster(as.numeric(NumCores))
registerDoSNOW(cl)

Data <- foreach(Indices=1:32, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_readBigData(FilePrefix='janfeb', FileIndex=Indices, DataPath=DataPath.Scratch, 
                        Colnames=Colnames, Colnames.Keep=Colnames.Keep, 
                        Unique=FALSE)

stopCluster(cl)
rm(cl)

####################################################
# This part is just an example for one dataset     #
####################################################
# Data <- fn_readBigData(FilePrefix='janfeb', FileIndex=1, Unique=FALSE,
#                                DataPath=DataPath.Scratch, Colnames=Colnames, 
#                                Colnames.Keep=Colnames.Keep)
Data <- Data[Data$prod_category_id != '',]
Data$prod_category_id <- try(fn_formatProdCategory(Vector.In = Data$prod_category_id))
Data$prod_qty <- try(fn_formatProdQty(Vector.In = Data$prod_qty))
Data$prod_totprice <- try(fn_formatProdTotPrice(Vector.In = Data$prod_totprice))
str(Data)

ProdCat <- sort(table(Data$prod_category_id), decreasing=T)
jpeg(filename='../Meetings/2013-06-13_MSC/Plot5_ProdCat.jpg')
barplot(ProdCat[1:20], las=2, cex.axis=0.75, col='blue',
        main='Top 20 Product Categories', horiz=F,
        cex.lab = 0.5, cex=0.6)
dev.off()

## Filename.Out <- paste(OutputDataPath, FilePrefix, '_', 'forCluster.RData', sep='')
## save(Data, file=Filename.Out)
