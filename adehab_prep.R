rm(list = ls())

load("ows_land.RData")
ows.land$datetime <- as.POSIXct(strptime(as.character(ows.land$datetime),"%Y-%m-%d %H:%M:%S", tz="US/Alaska")) 
traj<-as.ltraj(xy=ows.land[,15:16], date=ows.land$datetime, id=as.character(ows.land$animal))

sum <-summary(traj)

### Checking traj against ows.land

pb_06810 <- dplyr::filter(ows.land, animal=="pb_06810") #accurately reflects ows.land
pb_20525 <- dplyr::filter(ows.land, animal=="pb_20525") #needs to be cut

#### Set reference date and NA's 

refda <- parse_date_time(paste(min(ows.land$datetime)), orders = 'ymd HMS', tz = 'US/Alaska') #set reference date 
traj2 <- setNA(traj,refda,1,units = 'hour')



## cut multi-year bursts - NOT WORKING

cut <- function(dt){
  return(dt>(20*3600*24))
}

traj3 <- cutltraj(traj2, "cut(dt)", nextr = TRUE)


