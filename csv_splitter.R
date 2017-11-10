library(dplyr)
library(tidyverse)
library(data.table)
options(scipen=999,stringsAsFactors = F)
sum2=function(x){sum(x,na.rm=TRUE)}
mean2=function(x){mean(x,na.rm=TRUE)}
# set this to the maximum amount of working memory you need (WARNING: ensure you have enough space on your HDD)
memory.limit(size = 32000) 

# Unzip files 
zip.loc <- "E:/My Documents/Important Files/Work/SESYNC/Data analysis/IPUMS/data"
unzip.loc <- "E:/My Documents/Important Files/Work/SESYNC/Data analysis/IPUMS/data/downloads"
loc_files <- "E:/My Documents/Important Files/Work/SESYNC/Data analysis/IPUMS/data/ipumsi_countries/"

file.list<- list.files(zip.loc,pattern="*.gz",recursive = T, full.names = T)
for (i in 1:length(file.list)){
  gunzip(file.list[i],overwrite=T)
}

# Create separate csv files 
file.list<- list.files(unzip.loc,recursive = T, full.names = T)


# loop through the files and write a file from each country each country in each file 
for (i in 1:length(file.list)){
  ptm <- proc.time()               # to figure out how long it takes to run the function
  print(file.list[i])
  system.time(x <- fread(file.list[i], showProgress = T))
  ctry.list <- unique(x$COUNTRY)
  print(ctry.list)
  gc()
        
  for (j in (ctry.list)){
   ctry.data <- subset(x,COUNTRY==j)
   x <- filter(x,COUNTRY!=j)
   gc()
   fwrite(ctry.data,paste0(loc_files,"ipums_",j,".csv"), showProgress = T)
   }
  rm(x);rm(ctry.data)
  gc()
  proc.time() - ptm
  }

# get list of countries included
output.list<- list.files('E:/My Documents/Important Files/Work/SESYNC/Data analysis/IPUMS/data/ipumsi_countries',recursive = T, full.names = F)
output.list <- gsub('ipums_|.csv','',output.list)
output.list <-as.numeric(output.list)
write.csv(output.list,'included_countries.csv')

