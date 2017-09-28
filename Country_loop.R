#install.packages("devtools")
#devtools::install_github("edwindj/ffbase2")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ff)
library(ffbase2)
library(data.table)

# Read in the data as a ffdf object to make it manageable in R
asia<- read.csv.ffdf(file="ipumsi_asia.csv", header=TRUE, VERBOSE=TRUE, 
                  first.rows=1000000, next.rows=1000000, colClasses=NA)
europe<- read.csv.ffdf(file="ipumsi_europe.csv", header=TRUE, VERBOSE=TRUE, 
                     first.rows=1000000, next.rows=1000000, colClasses=NA)
pryr::object_size(asia)
# Work with the data one country at a time
clist <- unique(asia$COUNTRY)


#Remove most variables, create an empty data frame
rm(list=setdiff(ls(), c("asia","europe","clist")))
all.data <- data.frame()
ptm <- proc.time()

for (i in 1:length(clist)){
  c1 <- as.data.frame(filter(asia,COUNTRY==clist[i]))

  # Total population
  tot.popn <- c1 %>%
    group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
    summarize(popXX = sum(PERWT))
  
  # Education
  educ <- filter(c1,EDATTAIN%in%(1:4) & AGE2%in%(9:20)) %>% # ages 25+
    mutate(EDATTAIN1=recode(EDATTAIN,`1` = "Less_primary", `2`="Primary",`3`="Secondary",
                            `4`="University",`9` = "Unknown")) %>%
    group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2, EDATTAIN1) %>%
    summarize(educ.totnum = sum(PERWT)) %>%
    mutate(educ.percent = educ.totnum / sum(educ.totnum)) %>%
    select(-educ.totnum)%>%
    filter(EDATTAIN1!="Unknown") %>%
    spread(key=EDATTAIN1, value=educ.percent) %>%
    rename(hsXX=Secondary,colXX=University) %>%
    select(-Less_primary,-Primary)

  # Employment
  empl <- filter(c1,EMPSTAT%in%c(1:2)) %>%
    mutate(EMPSTAT1=recode(EMPSTAT,`1` = "Employed", `2`="Unemployed")) %>%
    group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2, EMPSTAT1) %>%
    summarize(empl.totnum = sum(PERWT)) %>%
    mutate(emp.percent = empl.totnum / sum(empl.totnum)) %>%
    mutate(empl.force = sum(empl.totnum)) %>%
    filter(EMPSTAT1=="Unemployed") %>%
    rename(punempXX=emp.percent) %>%
    select(-empl.totnum,-EMPSTAT1)

  # Female participation in labor force
  emplf <- filter(c1,SEX==2 & EMPSTAT==1) %>%
    group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
    summarize(flabfXX= sum(PERWT))
  # Occupation type
  prof <- filter(c1,OCCISCO%in%(1:4)) %>%
    group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
    summarize(profXX = sum(PERWT))
  
  empl <- empl %>%
    left_join(emplf,by=c("COUNTRY"="COUNTRY","YEAR"="YEAR","GEOLEV1"="GEOLEV1","GEOLEV2"="GEOLEV2")) %>%
    left_join(prof,by=c("COUNTRY"="COUNTRY","YEAR"="YEAR","GEOLEV1"="GEOLEV1","GEOLEV2"="GEOLEV2")) %>%
    mutate(pflabfXX = flabfXX/empl.force) %>%
    mutate(pprofXX = profXX/empl.force) %>%
    select(-flabfXX,-profXX,-empl.force)
  
  
  c2 <- tot.popn %>%
    left_join(educ,
              by=c("COUNTRY"="COUNTRY","YEAR"="YEAR","GEOLEV1"="GEOLEV1","GEOLEV2"="GEOLEV2")) %>%
    left_join(empl,
              by=c("COUNTRY"="COUNTRY","YEAR"="YEAR","GEOLEV1"="GEOLEV1","GEOLEV2"="GEOLEV2"))
    
 
  all.data <- rbind(all.data,as.data.frame(c2))
  rm(c1)
  print(paste0("Country ",i, " completed"))
}
proc.time() - ptm

glvl1.data <- filter(all.data,is.na(GEOLEV2))
glvl2.data <- filter(all.data,!is.na(GEOLEV2))


summary(all.data)
write_csv(all.data,'Geolevel2asia.csv')
