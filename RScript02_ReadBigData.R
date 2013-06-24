rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

#################################################### 
## This script is to read in the big data, one    ##
## small file at a time and save the output on    ##
## the hard drive. This script should be used for ##
## creating the dataset for demographics only     ##
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
Filename.Header <- paste(FilePath, 'HeaderFile_BigMem.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library_BigData.R', sep=''))
#################################################### 

#################################################### 
## Data Input                                     ##
#################################################### 
#setwd(DataPath)
Filename <- paste(DataPath, 'janfeb.txt', sep='')
Data <- read.table(file=Filename, header=TRUE, sep='\t', nrows=10,
                   colClasses='character', skip=0, quote='', 
                   stringsAsFactors=FALSE, fill=TRUE, 
                   comment.char='')
## quote='' allows the columns to have ' and " in the strings
## colClasses is set to 'character' because some numeric values are too long
## comment.char='' allows the columns to have # in the strings
str(Data)
dim(Data)
#View(head(Data))
#View(Data)
####################################################
Colnames <- colnames(Data)
Colnames.Keep <- c('machine_id', 'hoh_most_education', 'census_region', 'household_size', 
                   'hoh_oldest_age', 'household_income', 'children', 'racial_background', 
                   'zip_code')
FilePrefix <- 'janfeb'

# BigData <- fn_readBigData(FilePrefix='janfeb', FileIndex=1, DataPath=DataPath.Scratch, 
#                          Colnames=Colnames, Colnames.Keep=Colnames.Keep)

Time1 <- Sys.time()
cl <- makeSOCKcluster(as.numeric(8))
registerDoSNOW(cl)

BigData <- foreach(Indices=1:32, .inorder=FALSE, .packages=MyAutoLoads, .combine=rbind) %dopar% 
  fn_readBigData(FilePrefix='janfeb', FileIndex=Indices, DataPath=DataPath.Scratch, Colnames=Colnames, Colnames.Keep=Colnames.Keep)

stopCluster(cl)
rm(cl)

print(Sys.time() - Time1)

BigData <- unique(BigData)
Filename.Out <- paste(OutputDataPath, FilePrefix, '_', 'Demographics.RData', sep='')
save(BigData, file=Filename.Out)

     
# byZipCode <- aggregate(x=Data1[,c('machine_id')], by=list(Data1$zip_code), FUN=length)
