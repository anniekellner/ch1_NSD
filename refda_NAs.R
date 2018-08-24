rm(list = ls())

library(dplyr)
library(lubridate)

load("ows_land.RData")
load("pb_traj_df.RData")

refda <- parse_date_time(paste(min(ows.land$datetime)), orders = 'ymd HMS', tz = 'US/Alaska') #set reference date 


sum <- as.table(tapply(pb.traj.df$dt, pb.traj.df$burst, mean, na.rm=T))#mean dt for each burst
sum<- as.data.frame(sum)
colnames(sum) <- c("burst","dt_seconds")
sum <- mutate(sum, hours=dt_seconds/3600) #convert seconds to hours

#write.csv(sum, file="fix_intervals.csv")




