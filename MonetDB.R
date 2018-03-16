#install.packages("MonetDBLite")
library(data.table)
library(DBI)

setwd("E:/My Documents/Important Files/Work/SESYNC/Data analysis/IPUMS/data/downloads")    # location of downloaded microdata
file.list<- list.files('.',recursive = T, full.names = T)
file.list
con <- dbConnect( MonetDBLite::MonetDBLite() , dbdir)
dbWriteTable(con, "seasia", seasia,overwrite =TRUE)




system.time(seasia <- fread('ipumsi_00028.csv',nrows = 50000000, showProgress = T))
system.time(seasia2 <- fread('ipumsi_00028.csv',skip = 50000000, showProgress = T))


#dbGetQuery(con, "SELECT MAX(mpg) FROM mtcars WHERE cyl = 8")

library(dplyr)
ms <- MonetDBLite::src_monetdblite(dbdir)
mt <- tbl(ms, "test")
test2 <- mt %>%
              filter(COUNTRY==360) %>%
              group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
              summarize(popXX = sum(PERWT))


for (i in 1:length(file.list)){
  print(file.list[i])
  system.time(x <- fread(file.list[2], showProgress = T))
  ctry.list <- unique(x$COUNTRY)
  print(paste0("countries in file: ",ctry.list)
        
        for (j in (ctry.list)){
          ctry.data <- filter(x,COUNTRY==j)
          fwrite(ctry.data,paste0("E:/My Documents/Important Files/Work/SESYNC/Data analysis/IPUMS/data/ipumsi_countries/ipums_",j,".csv"), showProgress = T)
        }
        rm(x);rm(ctry.data)
        gc()
}




ptm <- proc.time()
A <- collect(empl)
proc.time() - ptm
MonetDBLite::monetdblite_shutdown()
dbDisconnect()

