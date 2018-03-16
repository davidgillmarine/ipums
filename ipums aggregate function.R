
#if you need to install ffbase remove hashtags from next 2 lines
#install.packages("devtools")
#devtools::install_github("edwindj/ffbase2")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(ff)
library(ffbase2)
library(rgdal)
options(scipen=999,stringsAsFactors = F)
sum2=function(x){sum(x,na.rm=TRUE)}
mean2=function(x){mean(x,na.rm=TRUE)}

# Set working directory
setwd("C:/Users/LocalAdmin/Documents/OneDrive - Conservation International 1/Data analysis/IPUMS")
setwd("E:/My Documents/Important Files/Work/SESYNC/Data analysis/IPUMS/ipums_extract")    # location of downloaded microdata


#---------------------------
# read in data
y <- "ipums_218.csv"
system.time(c1 <- fread(y))

# ensure to join by all same variables
join.names <- c("COUNTRY", "YEAR", "GEOLEV1", "GEOLEV2")

#----------------------------------------------
#-- Total population
tot.popn <- c1 %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(popXX = sum(PERWT))

#--Education                                    
# number of persons with hs degree or less (Secondary education)
educsec <- filter(c1,EDATTAIND%in%(311:322) & AGE%in%c(25:100)) %>% # ages 25+ yrs
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(edusecondXX = sum(PERWT))
educD <- filter(c1,EDATTAIND%in%(100:400) & AGE%in%c(25:100)) %>% # ages 25+ yrs
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(educDXX = sum(PERWT))

# number persons with at least a four year college degree
educter <- filter(c1,EDATTAIND==400 & AGE%in%c(25:100)) %>% # ages 25+ yrs
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(educterXX = sum(PERWT))

# Percent of persons age 10+ literate(numerator)"
lit10N <- filter(c1,LIT==2 & AGE%in%c(10:100)) %>% # ages 25+ yrs
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(lit10NXX = sum(PERWT))

# Percent of persons age 10+ literate (denominator)"
lit10D <- filter(c1,LIT%in%c(1:2) & AGE%in%c(10:100)) %>% # ages 25+ yrs
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(lit10DXX = sum(PERWT))

educ <- tot.popn[join.names] %>% 
  left_join(educsec,by = join.names) %>% 
  left_join(educter,by = join.names) %>% 
  left_join(educD,by = join.names) %>% 
  left_join(lit10N,by = join.names) %>% 
  left_join(lit10D,by = join.names)

#--Employment
# Current labor force (employed and unemployed individuals between 16-65; excludes housework)
#empclf <- filter(c1,EMPSTATD%in%c(100:240) & AGE%in%c(16:65)) %>%
#  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
#  summarize(empclfXX = sum(PERWT))

# Civilian labor force- persons 16 years and over (excluding armed forces where occupation is known)
clf <- filter(c1,EMPSTATD%in%c(100:120,140:240) & AGE%in%c(16:65) & !OCCISCO%in%c(10,97:99)) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>% #eliminating other column headers
  summarize(clfXX = sum(PERWT)) #can add abbreviated function due to <%> piping
# Unemployment for individuals between 16-65 (of civilians)
unempl <- filter(c1,EMPSTATD%in%c(200:240) & AGE%in%c(16:65) & !OCCISCO%in%c(10,97:99)) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(unempXX = sum(PERWT)) 

# Females 16 and over, not in armed forces (generally, employed or actively seeking employment)
lfpfemN <- filter(c1,SEX==2 & EMPSTATD%in%c(100:120,140) & AGE%in%c(16:65) & !OCCISCO%in%c(10,97:99)) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(lfpfemNXX= sum(PERWT))
lfpfemD <- filter(c1,SEX==2 & EMPSTATD%in%c(100:120,140:240) & AGE%in%c(16:65) & !OCCISCO%in%c(10,97:99)) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(lfpfemDXX= sum(PERWT))

# Percent of workers in professional or technical occupations (excluding clerks)
occprofN <- filter(c1,AGE%in%c(16:65) & EMPSTATD%in%c(100:120,140) & OCCISCO%in%c(1:3)) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(occprofNXX = sum(PERWT)) 
occprofD <- filter(c1,AGE%in%c(16:65) & EMPSTATD%in%c(100:120,140) & !OCCISCO%in%c(10,97:99)) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(occprofDXX = sum(PERWT)) 

# Percent of currently employed persons age 16 to 65 who are working in agriculture or fisheries
occagrN <- filter(c1,AGE%in%c(16:65) & EMPSTATD%in%c(100:120,140) & OCCISCO==6) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(occagrNXX = sum(PERWT)) 
occagrD <- filter(c1,AGE%in%c(16:65) & EMPSTATD%in%c(100:120,140) & !OCCISCO%in%c(10,97:99)) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(occagrDXX = sum(PERWT)) 

empl <- tot.popn[join.names] %>%
  left_join(clf,by=join.names) %>%
  left_join(unempl,by=join.names) %>%
  left_join(lfpfemN,by=join.names) %>%
  left_join(lfpfemD,by=join.names) %>%
  left_join(occprofN,by=join.names) %>%
  left_join(occprofD,by=join.names) %>%
  left_join(occagrN,by=join.names) %>%
  left_join(occagrD,by=join.names) 

#--Assets
# Refrigerator ownership
refrigN <- filter(c1,REFRIG==2 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(refrigNXX = sum(HHWT))
refrigD <- filter(c1,REFRIG%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(refrigDXX = sum(HHWT)) 

# hot water access (water heater in unit)
hotwaterN <- filter(c1,HOTWATER==2 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(hotwaterNXX = sum(HHWT))
hotwaterD <- filter(c1,HOTWATER%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(hotwaterDXX = sum(HHWT)) 


# cellphone access
cellN <- filter(c1,CELL==1 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(cellNXX = sum(HHWT))
cellD <- filter(c1,CELL%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(cellDXX = sum(HHWT)) 

# TV ownership
tvN <- filter(c1,TV%in%c(20:43) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(tvNXX = sum(HHWT))
tvD <- filter(c1,TV%in%c(10:43) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(tvDXX = sum(HHWT)) 

# Radio ownership
radioN <- filter(c1,RADIO==2 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(radioNXX = sum(HHWT))
  radioD <- filter(c1,RADIO%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(radioDXX = sum(HHWT)) 

# phone ownership
phoneN <- filter(c1,PHONE==2 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(phoneNXX = sum(HHWT))
phoneD <- filter(c1,PHONE%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(phoneDXX = sum(HHWT)) 

# computer ownership
computerN <- filter(c1,COMPUTER==2 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(computerNXX = sum(HHWT))
computerD <- filter(c1,COMPUTER%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(computerDXX = sum(HHWT)) 

# internet access
internetN <- filter(c1,INTERNET==2 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(internetNXX = sum(HHWT))
internetD <- filter(c1,INTERNET%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(internetDXX = sum(HHWT)) 

# Washer ownership
washerN <- filter(c1,WASHER%in%c(2:4) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(washerNXX = sum(HHWT))
washerD <- filter(c1,WASHER%in%c(1:4) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(washerDXX = sum(HHWT)) 

# Air-conditioner ownership
airconN <- filter(c1,AIRCON%in%c(20:29) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(airconNXX = sum(HHWT))
airconD <- filter(c1,AIRCON%in%c(10:29) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(airconDXX = sum(HHWT)) 

# Auto ownership
autoN <- filter(c1,AUTOS%in%c(1:7) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(autoNXX = sum(HHWT))
autoD <- filter(c1,AUTOS%in%c(0:7) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(autoDXX = sum(HHWT)) 


assets <- tot.popn[join.names]   %>%
  left_join(hotwaterN,by=join.names) %>%
  left_join(hotwaterD,by=join.names) %>%
  left_join(cellN,by=join.names) %>%
  left_join(cellD,by=join.names) %>%
  left_join(refrigN,by=join.names) %>%
  left_join(refrigD,by=join.names) %>%
  left_join(tvN,by=join.names) %>%
  left_join(tvD,by=join.names) %>% 
  left_join(radioN,by=join.names) %>%
  left_join(radioD,by=join.names) %>%
  left_join(phoneN,by=join.names) %>%
  left_join(phoneD,by=join.names) %>%
  left_join(computerN,by=join.names) %>%
  left_join(computerD,by=join.names) %>%
  left_join(internetN,by=join.names) %>%
  left_join(internetD,by=join.names) %>%
  left_join(washerN,by=join.names) %>%
  left_join(washerD,by=join.names) %>%
  left_join(airconN,by=join.names) %>%
  left_join(airconD,by=join.names) %>%
  left_join(autoN,by=join.names) %>%
  left_join(autoD,by=join.names)


#-- Household attributes
#Percent of households with electricity
electricN <- filter(c1,ELECTRIC==1 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(electricNXX = sum(HHWT))
electricD <- filter(c1,ELECTRIC%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(electricDXX = sum(HHWT)) 

#Percent of households with a flush toilet
flushtoiletN <- filter(c1,TOILET==21 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize( flushtoiletNXX = sum(HHWT))
flushtoiletD <- filter(c1,TOILET%in%c(10:23) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize( flushtoiletDXX = sum(HHWT))

# Cooking fuel
#Electricity
cookelectN <- filter(c1,FUELCOOK==20 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(cookelectNXX = sum(HHWT))
cookD <- filter(c1,FUELCOOK%in%c(10:77) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(cookDXX = sum(HHWT))
#Gas
cookgasN <- filter(c1,FUELCOOK%in%c(30:47) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(cookgasNXX = sum(HHWT))
#Wood and solid fuels
cookwoodN <- filter(c1,FUELCOOK%in%c(50:56) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(cookwoodNXX = sum(HHWT))

#Percentage of households with access to piped water
pipedN <- filter(c1,WATSUP%in%c(10:18) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(pipedNXX = sum(HHWT))
pipedD <- filter(c1,WATSUP%in%c(10:20) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(pipedDXX = sum(HHWT))

#Percentage of households with access to sewage facilities
sewageN <- filter(c1,SEWAGE%in%c(10:12) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(sewageNXX = sum(HHWT))
sewageD <- filter(c1,SEWAGE%in%c(10:20) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(sewageDXX = sum(HHWT))

#Percentage household ownership
homeownN <- filter(c1,OWNERSHIP==1 & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(homeownNXX = sum(HHWT))
homeownD <- filter(c1,OWNERSHIP%in%c(1:2) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(homeownDXX = sum(HHWT))

#Percentage land ownership
landownN <- filter(c1,LANDOWN%in%c(10:14) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(landownNXX = sum(HHWT))
landownD <- filter(c1,LANDOWN%in%c(10:30) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(landownDXX = sum(HHWT))

#Roof types
# Masonary, concrete, tiles
roofmasonN <- filter(c1,ROOF%in%c(10:29) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(roofmasonNXX = sum(HHWT))
roofD <- filter(c1,ROOF%in%c(10:90) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(roofDXX = sum(HHWT))
# Metal
roofmetalN <- filter(c1,ROOF%in%c(30:35) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(roofmetalNXX = sum(HHWT))
# Wood and/or clay
roofwoodN <- filter(c1,ROOF%in%c(40:61) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(roofwoodNXX = sum(HHWT))

#trash
# Collected or not collected
trashcollectN <- filter(c1,TRASH%in%c(10:14) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(trashcollectNXX = sum(HHWT))
trashcollectD <- filter(c1,TRASH%in%c(10:39) & GQ==10 & PERNUM==1) %>%
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(trashcollectDXX = sum(HHWT))

#Person per household                                      
hhsize <- filter(c1, GQ==10 & PERNUM==1) %>% 
  group_by(COUNTRY,YEAR,GEOLEV1,GEOLEV2) %>%
  summarize(hhsizeXX = sum2(PERSONS*HHWT)/sum2(HHWT))


houseatt <- tot.popn[join.names]   %>%
  left_join(electricN,by=join.names) %>%
  left_join(electricD,by=join.names) %>%
  left_join(flushtoiletN,by=join.names) %>%
  left_join(flushtoiletD,by=join.names) %>%
  left_join(cookelectN,by=join.names) %>%
  left_join(cookgasN,by=join.names) %>%
  left_join(cookwoodN,by=join.names) %>%
  left_join(cookD,by=join.names) %>% 
  left_join(pipedN,by=join.names) %>%
  left_join(pipedD,by=join.names) %>%
  left_join(sewageN,by=join.names) %>%
  left_join(sewageD,by=join.names) %>%
  left_join(homeownN,by=join.names) %>%
  left_join(homeownD,by=join.names) %>%
  left_join(landownN,by=join.names) %>%
  left_join(landownD,by=join.names) %>%
  left_join(roofmasonN,by=join.names) %>%
  left_join(roofmetalN,by=join.names) %>%  
  left_join(roofwoodN,by=join.names) %>%
  left_join(roofD,by=join.names) %>%
  left_join(trashcollectN,by=join.names) %>%
  left_join(trashcollectD,by=join.names) %>%
  left_join(hhsize,by=join.names) %>%
  

c2 <- tot.popn %>%
  left_join(educ,by=join.names) %>%
  left_join(empl,by=join.names) %>%
  left_join(assets, by=join.names) %>%
  left_join(houseatt,by=join.names)

c4 <- c2 %>%
  mutate(peducsecXX=edusecondXX/educDXX,
         peducterXX=educterXX/educDXX,
         plit10XX=lit10NXX/lit10DXX,
         punempXX=unempXX/clfXX,
         plfpfemXX=lfpfemNXX/lfpfemDXX,
         poccprofXX=occprofNXX/occprofDXX,
         poccagrXX=occagrNXX/occagrDXX,
         photwaterXX=hotwaterNXX/hotwaterDXX,
         pcellXX=cellNXX/cellDXX,
         prefrigXX=refrigNXX/refrigDXX,
         ptvXX=tvNXX/tvDXX,
         pradioXX=radioNXX/radioDXX,
         pphoneXX=phoneNXX/phoneDXX,
         pcomputerXX=computerNXX/computerDXX,
         pinternetXX=internetNXX/internetDXX,
         pwasherXX=washerNXX/washerDXX,
         pairconpXX=airconNXX/airconDXX,
         pautoXX=autoNXX/autoDXX,
         pelectricXX=electricNXX/electricDXX,
         pflushtoiletXX=flushtoiletNXX/flushtoiletDXX,
         pcookelectXX=cookelectNXX/cookDXX,
         pcookgasXX=cookgasNXX/cookDXX,
         pcookwoodXX=cookwoodNXX/cookDXX,
         ppipedXX=pipedNXX/pipedDXX,
         psewageXX=sewageNXX/sewageDXX,
         phomeownXX=homeownNXX/homeownDXX,
         plandownXX=landownNXX/landownDXX,
         proofmasonXX=roofmasonNXX/roofDXX,
         proofmetalXX=roofmetalNXX/roofDXX,
         proofwoodXX=roofwoodNXX/roofDXX,
         trashcollectXX=trashcollectNXX/trashcollectDXX)
