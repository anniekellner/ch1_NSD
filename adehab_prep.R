rm(list = ls())

load("ows_land.RData")
ows.land$datetime <- as.POSIXct(strptime(as.character(ows.land$datetime),"%Y-%m-%d %H:%M:%S", tz="US/Alaska")) 
traj<-as.ltraj(xy=ows.land[,15:16], date=ows.land$datetime, id=as.character(ows.land$animal))

sum <-summary(traj)

### Checking traj against ows.land

pb_06810 <- dplyr::filter(ows.land, animal=="pb_06810") #accurately reflects ows.land
pb_20525 <- dplyr::filter(ows.land, animal=="pb_20525") #needs to be cut


traj.df<-ld(traj) #df
traj.df$year<-as.numeric(format(traj.df$date, "%Y"))
traj.df$ID_Year<- paste(traj.df$id, traj.df$year, sep="_")

traj2<-as.ltraj(traj.df[,1:2], date=traj.df$date, id=traj.df$id, burst=traj.df$ID_Year)

#refda <- parse_date_time(paste(min(ows.land$datetime)), orders = 'ymd HMS', tz = 'US/Alaska') #set reference date 
#traj2 <- setNA(traj,refda,1,units = 'hour') ## not necessary yet - look into this later 









