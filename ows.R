rm(list=ls())
library(lubridate)
library(dplyr)

#load data
load("pbears.RData")
land  <- filter(all, land==1)

#### open water season ###############
######################################

# split into columns for each ows

all$date <- ymd(paste(all$year, all$month, all$day,sep = "-"), tz="US/Alaska")
all$ows2004 <- ifelse(all$date >= "2004-06-16" & all$date<= "2004-10-31",1,0)
all$ows2005 <- ifelse(all$date >= "2005-06-26" & all$date <= "2005-11-04",1,0)
all$ows2006 <- ifelse(all$date >= "2006-07-24" & all$date <= "2006-11-07",1,0)
all$ows2007 <- ifelse(all$date >= "2007-06-08" & all$date <= "2007-11-17",1,0)
all$ows2008 <- ifelse(all$date >= "2008-06-15" & all$date <= "2008-11-06",1,0)
all$ows2009 <- ifelse(all$date >= "2009-06-25" & all$date <= "2009-11-10",1,0)
all$ows2010 <- ifelse(all$date >= "2010-06-11" & all$date <= "2010-11-02",1,0)
all$ows2011 <- ifelse(all$date >= "2011-06-22" & all$date <= "2011-11-12",1,0)
all$ows2012 <- ifelse(all$date >= "2012-06-20" & all$date <= "2012-11-10",1,0)
all$ows2013 <- ifelse(all$date >= "2013-07-04" & all$date <= "2013-11-04",1,0)
all$ows2014 <- ifelse(all$date >= "2014-06-29" & all$date <= "2014-10-31",1,0)
all$ows2015 <- ifelse(all$date >= "2015-06-15" & all$date <= "2015-11-11",1,0)
all$ows2016 <- ifelse(all$date >= "2016-05-30" & all$date <= "2016-11-21",1,0)

#separate objects for each ows
ows2004 <- filter(all, ows2004==1)
ows2005 <- filter(all, ows2005==1)
ows2006 <- filter(all, ows2006==1)
ows2007 <- filter(all, ows2007==1)
ows2008 <- filter(all, ows2008==1)
ows2009 <- filter(all, ows2009==1)
ows2010 <- filter(all, ows2010==1)
ows2011 <- filter(all, ows2011==1)
ows2012 <- filter(all, ows2012==1)
ows2013 <- filter(all, ows2013==1)
ows2014 <- filter(all, ows2014==1)
ows2015 <- filter(all, ows2015==1)
ows2016 <- filter(all, ows2016==1)

#combine
ows <- rbind(ows2004,ows2005,ows2006,ows2007,ows2008,ows2009,ows2010,ows2011,ows2012,ows2013,ows2014,ows2015,ows2016)
ows <- select(ows,animal:end.swim)

save(ows, file="ows.RData")

##### Checking against ows_complete_v1.csv #############
########################################################

ows.csv <- read.csv("ows_all_v2.csv")

ows.csv <- dplyr::select(ows.csv, animal, gps_lat, gps_lon, year, month, day, hour, minute, second) #identical cols
ows <- dplyr::select(ows,animal, gps_lat, gps_lon, year, month, day, hour, minute, second)
diff <- dplyr::setdiff(ows.csv,ows) #differences between two 

## DIFFERENCES ARE ALL DUPLICATES ######